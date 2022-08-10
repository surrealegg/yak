use crate::{
    ast::{
        BinaryKind, Expression, Function, LiteralKind, Param, Prototype, Statement, Type,
        VariableDeclaration,
    },
    error::{Error, ErrorKind, ErrorSeverity},
};

#[derive(Debug, Clone)]
struct StoredVariable {
    pub scope: usize,
    pub name: String,
    pub kind: Type,
    pub mutable: bool,
}

impl StoredFunction {
    pub fn change_type(&mut self, kind: Type) {
        self.return_kind = kind;
    }
}

#[derive(Debug, Clone)]
struct StoredFunction {
    pub name: String,
    pub return_kind: Type,
    pub params: Vec<Param>,
}

pub struct Linter {
    scope: usize,
    variables: Vec<StoredVariable>,
    functions: Vec<StoredFunction>,
    temp_function: Option<StoredFunction>,
    in_loop: usize,
    warnings: Vec<Error>,
}

impl Linter {
    pub fn new() -> Self {
        Self {
            variables: vec![],
            functions: vec![],
            warnings: vec![],
            temp_function: None,
            scope: 0,
            in_loop: 0,
        }
    }

    fn find_variable_by_name(&self, name: &str) -> Option<&StoredVariable> {
        self.variables
            .iter()
            .find(|item| item.name == name && self.scope >= item.scope)
    }

    fn find_function_by_name(&self, name: &str) -> Option<&StoredFunction> {
        if let Some(function) = self.functions.iter().find(|item| item.name == name) {
            return Some(function);
        }
        if let Some(temp_function) = &self.temp_function {
            return Some(temp_function);
        }
        None
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

                if binary.kind.is_assign() {
                    let name = binary.left.get_variable_name();
                    if name.is_empty() {
                        return Err(Error {
                            kind: ErrorKind::InvalidLeftHandSideAssignment,
                            severity: ErrorSeverity::Error,
                            span: binary.span,
                        });
                    }

                    if let Some(variable) = self.find_variable_by_name(&name) {
                        if !variable.mutable {
                            return Err(Error {
                                kind: ErrorKind::CantAssignImmutableVariable(name),
                                severity: ErrorSeverity::Error,
                                span: binary.span,
                            });
                        }
                    }
                }

                if !lhs.equal(&rhs) || (binary.kind == BinaryKind::Modulo && !lhs.is_integer()) {
                    Err(Error {
                        kind: ErrorKind::BinaryBetweenIncompatibleTypes(
                            lhs,
                            rhs,
                            binary.kind.clone(),
                        ),
                        severity: ErrorSeverity::Error,
                        span: binary.span,
                    })
                } else {
                    Ok(match binary.kind {
                        BinaryKind::DoubleEqual
                        | BinaryKind::BangEqual
                        | BinaryKind::Great
                        | BinaryKind::GreatEqual
                        | BinaryKind::Less
                        | BinaryKind::LessEqual => Type::Bool,
                        _ => lhs,
                    })
                }
            }
            Expression::Unary(unary) => {
                let expr = self.check_expression(&unary.expr)?;
                if unary.kind.can_apply(&expr) {
                    Ok(expr)
                } else {
                    Err(Error {
                        kind: ErrorKind::CantApplyUnary(unary.kind.clone(), expr),
                        severity: ErrorSeverity::Error,
                        span: unary.span,
                    })
                }
            }
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

                            let label_empty = expr.name.is_empty();
                            let label = if label_empty {
                                expr.value.get_variable_name()
                            } else {
                                expr.name.clone()
                            };
                            if (!param.anon || !label_empty) && label != param.name {
                                return Err(Error {
                                    kind: ErrorKind::WrongLabel(param.name.clone(), label),
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

    fn get_statement_return_type(&mut self, statements: &[Statement]) -> Result<Type, Error> {
        let mut it = statements.iter().peekable();
        while let Some(statement) = it.next() {
            if let Statement::Return(ret) = statement {
                // FIXME: Sort out clippy's error
                if it.peek().is_some() {
                    self.warnings.push(Error {
                        span: ret.span,
                        kind: ErrorKind::DeadCode,
                        severity: ErrorSeverity::Warning,
                    });
                }
                return Ok(if let Some(expression) = &ret.expression {
                    self.check_expression(&expression)?
                } else {
                    Type::Void
                });
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

    fn create_function(&mut self, function: &Function) -> Result<Type, Error> {
        let mut prototype = self.create_prototype(&function.prototype)?;
        for item in prototype.params.iter() {
            self.variables.push(StoredVariable {
                scope: self.scope,
                name: item.name.clone(),
                kind: item.kind.clone(),
                mutable: true,
            });
        }

        let old_temp_function = self.temp_function.clone();
        self.temp_function = Some(prototype.clone());

        let returned_kind = self.get_statement_return_type(&function.statements)?;
        if returned_kind.equal(&Type::Unknown) {
            return Err(Error {
                kind: ErrorKind::AmbiguousType,
                severity: ErrorSeverity::Error,
                span: function.prototype.span,
            });
        }

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

        prototype.change_type(returned_kind.clone());
        self.functions.push(prototype);
        self.temp_function = old_temp_function;

        Ok(returned_kind)
    }

    fn create_variable(&self, decl: &VariableDeclaration) -> Result<StoredVariable, Error> {
        if let Some(_) = self
            .variables
            .iter()
            .find(|item| item.name == decl.name && self.scope == item.scope)
        {
            return Err(Error {
                kind: ErrorKind::RedefinitionVariable(decl.name.clone()),
                severity: ErrorSeverity::Error,
                span: decl.span,
            });
        }

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

    fn check_statement(&mut self, statement: &mut Statement) -> Result<(), Error> {
        let temp = match statement {
            Statement::Expression(expression) => {
                self.check_expression(expression)?;
                None
            }
            Statement::VariableDeclaration(decl) => {
                let variable = self.create_variable(decl)?;
                let variable_kind = variable.kind.clone();
                self.variables.push(variable);
                let result = Statement::VariableDeclaration(VariableDeclaration {
                    mutable: decl.mutable,
                    name: decl.name.clone(),
                    span: decl.span,
                    value: decl.value.clone(),
                    variable_type: variable_kind,
                });
                Some(result)
            }
            Statement::Block(block) => {
                self.in_loop += 1;
                self.check_statements(&mut block.statements)?;
                self.in_loop -= 1;
                None
            }
            Statement::Loop(block) => {
                self.in_loop += 1;
                self.check_statements(&mut block.statements)?;
                self.in_loop -= 1;
                None
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
                self.check_statements(&mut while_block.block)?;
                self.in_loop -= 1;
                None
            }
            Statement::Break(break_statement) => {
                if self.in_loop == 0 {
                    return Err(Error {
                        kind: ErrorKind::BreakOutsideLoop,
                        severity: ErrorSeverity::Error,
                        span: break_statement.span,
                    });
                }
                None
            }
            Statement::Continue(continue_statement) => {
                if self.in_loop == 0 {
                    return Err(Error {
                        kind: ErrorKind::ContinueOutsideLoop,
                        severity: ErrorSeverity::Error,
                        span: continue_statement.span,
                    });
                }
                None
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
                self.check_statements(&mut if_statement.true_block)?;
                self.check_statements(&mut if_statement.else_block)?;
                None
            }
            Statement::Prototype(prototype) => {
                let mut stored_prototype = self.create_prototype(prototype)?;
                let mut saved_kind = stored_prototype.return_kind.clone();
                if stored_prototype.return_kind == Type::Unknown {
                    stored_prototype.return_kind = Type::Void;
                    saved_kind = Type::Void;
                }
                self.functions.push(stored_prototype);
                Some(Statement::Prototype(Prototype {
                    is_extern: prototype.is_extern,
                    name: prototype.name.clone(),
                    params: prototype.params.clone(),
                    return_type: saved_kind,
                    span: prototype.span,
                }))
            }
            Statement::Function(function) => {
                let kind = self.create_function(function)?;
                self.check_statements(&mut function.statements)?;

                Some(Statement::Function(Function {
                    prototype: Prototype {
                        params: function.prototype.params.clone(),
                        return_type: kind,
                        name: function.prototype.name.clone(),
                        is_extern: function.prototype.is_extern,
                        span: function.prototype.span,
                    },
                    statements: function.statements.clone(),
                }))
            }
            Statement::Return(value) => {
                if let Some(expression) = &value.expression {
                    self.check_expression(expression)?;
                }
                None
            }
        };

        if let Some(temp) = temp {
            *statement = temp;
        }

        Ok(())
    }

    pub fn check_statements(&mut self, statements: &mut [Statement]) -> Result<Vec<Error>, Error> {
        self.enter_scope();
        let mut iter = statements.iter_mut();
        let mut result = vec![];
        while let Some(statement) = iter.next() {
            result.push(self.check_statement(statement)?);
        }
        self.leave_scope();
        Ok(self.warnings.clone())
    }
}
