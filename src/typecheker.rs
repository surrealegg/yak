use crate::{
    ast::{
        Expression, Function, LiteralKind, Param, Prototype, Statement, Type, VariableDeclaration,
    },
    error::{Error, ErrorKind, ErrorSeverity},
};

#[derive(Debug, Clone)]
struct StoredVariable {
    pub scope: usize,
    pub name: String,
    pub kind: Type,
    pub mutable: bool,
    pub used: bool,
    pub mutated: bool,
}

#[derive(Debug, Clone)]
struct StoredFunction {
    pub name: String,
    pub return_kind: Type,
    pub params: Vec<Param>,
}

pub struct Typecheker {
    scope: usize,
    cursor: usize,
    variables: Vec<StoredVariable>,
    functions: Vec<StoredFunction>,
    warnings: Vec<Error>,
    in_loop: usize,
    expected_type: Type,
}

impl Typecheker {
    pub fn new() -> Self {
        Self {
            variables: vec![],
            functions: vec![],
            warnings: vec![],
            scope: 0,
            cursor: 0,
            in_loop: 0,
            expected_type: Type::Unknown,
        }
    }

    fn find_variable_by_name(&self, name: &str) -> Option<&StoredVariable> {
        self.variables.iter().find(|item| item.name == name)
    }

    fn find_function_by_name(&self, name: &str) -> Option<&StoredFunction> {
        self.functions.iter().find(|item| item.name == name)
    }

    fn check_expression(&self, expression: &Expression) -> Result<Type, Error> {
        match expression {
            Expression::Literal(literal) => match &literal.kind {
                LiteralKind::Boolean(_) => Ok(Type::Bool),
                LiteralKind::Char(_) => Ok(Type::CChar),
                LiteralKind::String(_) => Ok(Type::Raw(Box::new(Type::CChar))),
                LiteralKind::Identifier(name) => {
                    if let Some(variable) = self.find_variable_by_name(name) {
                        Ok(variable.kind.clone())
                    } else {
                        Err(Error {
                            kind: ErrorKind::VariableNotFound(name.clone()),
                            span: expression.span(),
                            severity: ErrorSeverity::Error,
                        })
                    }
                }
                LiteralKind::Integer(_) => Ok(Type::I64),
            },
            Expression::Binary(binary) => {
                let lhs = self.check_expression(&binary.left)?;
                let rhs = self.check_expression(&binary.right)?;
                if lhs.equal(&rhs) {
                    Ok(lhs)
                } else {
                    Err(Error {
                        kind: ErrorKind::BinaryBetweenIncompatibleTypes(lhs, rhs),
                        severity: ErrorSeverity::Error,
                        span: binary.span,
                    })
                }
            }
            Expression::Unary(unary) => self.check_expression(&unary.expr),
            Expression::Call(call) => {
                if let Some(function) = self.find_function_by_name(&call.name) {
                    if call.arguments.len() != function.params.len() {
                        return Err(Error {
                            kind: ErrorKind::FunctionArgumentCountMismatch(
                                function.params.len(),
                                call.arguments.len(),
                            ),
                            severity: ErrorSeverity::Error,
                            span: call.span,
                        });
                    }
                    for i in 0..call.arguments.len() {
                        let expr = &call.arguments[i];
                        let item_type = self.check_expression(&expr.value)?;
                        if let Some(param) = function.params.get(i) {
                            if item_type != param.kind {
                                return Err(Error {
                                    kind: ErrorKind::MismatchedTypes(param.kind.clone(), item_type),
                                    severity: ErrorSeverity::Error,
                                    span: expr.value.span(),
                                });
                            }
                            if (!param.anon || expr.name != "")
                                && expr.name != param.name
                                && expr.value.get_variable_name() != param.name
                            {
                                return Err(Error {
                                    kind: ErrorKind::WrongLabel(
                                        param.name.clone(),
                                        expr.name.clone(),
                                    ),
                                    severity: ErrorSeverity::Error,
                                    span: expr.value.span(),
                                });
                            }
                        }
                    }

                    Ok(function.return_kind.clone())
                } else {
                    Err(Error {
                        kind: ErrorKind::FunctionNotFound(call.name.clone()),
                        severity: ErrorSeverity::Error,
                        span: call.span,
                    })
                }
            }
            Expression::Grouping(grouping) => self.check_expression(&grouping.expr),
            Expression::Cast(cast) => {
                let current_expression = self.check_expression(&cast.expr)?;
                if current_expression.can_cast(&cast.kind) {
                    Ok(cast.kind.clone())
                } else {
                    Err(Error {
                        kind: ErrorKind::NonPrimitive(current_expression, cast.kind.clone()),
                        severity: ErrorSeverity::Error,
                        span: cast.span,
                    })
                }
            }
        }
    }

    fn get_statement_return_type(&self, statements: &[Statement]) -> Result<Type, Error> {
        let mut it = statements.iter().peekable();
        while let Some(statement) = it.next() {
            if let Statement::Return(ret) = statement {
                // FIXME: Sort out clippy's error
                // if it.peek().is_some() {
                //     self.warnings.push(Error {
                //         span: ret.span,
                //         kind: ErrorKind::DeadCode,
                //         severity: ErrorSeverity::Warning,
                //     });
                // }
                return Ok(self.check_expression(&ret.expression)?);
            }
        }
        Ok(Type::Void)
    }

    fn create_prototype(&self, prototype: &Prototype) -> Result<StoredFunction, Error> {
        if let Some(_) = self.find_function_by_name(&prototype.name) {
            Err(Error {
                kind: ErrorKind::RedefinedName(prototype.name.clone()),
                severity: ErrorSeverity::Error,
                span: prototype.span.clone(),
            })
        } else {
            Ok(StoredFunction {
                name: prototype.name.clone(),
                return_kind: prototype.return_type.clone(),
                params: prototype.params.clone(),
            })
        }
    }

    fn create_function(&self, function: &Function) -> Result<StoredFunction, Error> {
        let returned_kind = self.get_statement_return_type(&function.statements)?;
        if function.prototype.return_type != Type::Unknown
            && !function.prototype.return_type.equal(&returned_kind)
        {
            return Err(Error {
                kind: ErrorKind::MismatchedTypes(
                    function.prototype.return_type.clone(),
                    returned_kind,
                ),
                severity: ErrorSeverity::Error,
                span: function.prototype.span,
            });
        }

        Ok(StoredFunction {
            name: function.prototype.name.clone(),
            return_kind: returned_kind,
            params: function.prototype.params.clone(),
        })
    }

    fn create_variable(&self, decl: &VariableDeclaration) -> Result<StoredVariable, Error> {
        let kind = self.check_expression(&decl.value)?;
        if decl.variable_type != Type::Unknown && kind != decl.variable_type {
            return Err(Error {
                kind: ErrorKind::MismatchedTypes(decl.variable_type.clone(), kind),
                severity: ErrorSeverity::Error,
                span: decl.span,
            });
        }

        Ok(StoredVariable {
            scope: self.scope,
            name: decl.name.clone(),
            kind,
            mutable: decl.mutable,
            used: false,
            mutated: false,
        })
    }

    pub fn enter_scope(&mut self) {
        self.scope += 1;
    }

    pub fn leave_scope(&mut self) {
        self.variables = self
            .variables
            .clone()
            .into_iter()
            .filter(|item| item.scope < self.scope)
            .collect();
        self.scope -= 1;
    }

    fn check_statmenet(&mut self, statement: &Statement) -> Result<(), Error> {
        match statement {
            Statement::Expression(expression) => {
                self.check_expression(expression)?;
            }
            Statement::VariableDeclaration(decl) => {
                self.variables.push(self.create_variable(decl)?);
            }
            Statement::Block(block) | Statement::Loop(block) => {
                self.enter_scope();
                self.check_statmenets(&block.statements)?;
                self.leave_scope();
            }
            Statement::While(while_block) => {
                let check = self.check_expression(&while_block.expression)?;
                if check != Type::Bool {
                    return Err(Error {
                        kind: ErrorKind::MismatchedTypes(Type::Bool, check),
                        severity: ErrorSeverity::Error,
                        span: while_block.expression.span(),
                    });
                }
                self.in_loop += 1;
                self.check_statmenets(&while_block.block)?;
                self.in_loop -= 1;
            }
            Statement::Break(break_statement) => {
                if self.in_loop == 0 {
                    return Err(Error {
                        kind: ErrorKind::BreakOutsideLoop,
                        severity: ErrorSeverity::Error,
                        span: break_statement.span,
                    });
                }
            }
            Statement::Continue(continue_statement) => {
                if self.in_loop == 0 {
                    return Err(Error {
                        kind: ErrorKind::ContinueOutsideLoop,
                        severity: ErrorSeverity::Error,
                        span: continue_statement.span,
                    });
                }
            }
            Statement::If(if_statement) => {
                let check = self.check_expression(&if_statement.expression)?;
                if check != Type::Bool {
                    return Err(Error {
                        kind: ErrorKind::MismatchedTypes(Type::Bool, check),
                        severity: ErrorSeverity::Error,
                        span: if_statement.expression.span(),
                    });
                }
                self.check_statmenets(&if_statement.true_block)?;
                self.check_statmenets(&if_statement.else_block)?;
            }
            Statement::Prototype(prototype) => {
                self.functions.push(self.create_prototype(prototype)?);
            }
            Statement::Function(function) => {
                self.functions.push(self.create_function(function)?);
                self.check_statmenets(&function.statements)?;
            }
            Statement::Return(_) => {}
        }
        Ok(())
    }

    pub fn check_statmenets(&mut self, statements: &[Statement]) -> Result<(), Error> {
        let mut iter = statements.iter();
        while let Some(statement) = iter.next() {
            self.check_statmenet(statement)?;
        }
        Ok(())
    }
}
