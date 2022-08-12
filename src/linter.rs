use crate::{
    ast::{
        BinaryKind, Expression, Function, LiteralKind, Param, Prototype, Statement, Type,
        UnaryKind, VariableDeclaration,
    },
    error::{Error, ErrorKind, ErrorSeverity},
    utils::Span,
};

#[derive(Debug, Clone)]
struct StoredVariable {
    pub scope: usize,
    pub name: String,
    pub kind: Type,
    pub mutable: bool,
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
    in_unsafe: usize,
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
            in_unsafe: 0,
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

    fn check_expression(&self, expression: &mut Expression) -> Result<Type, Error> {
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
                let lhs = self.check_expression(&mut binary.left)?;
                let rhs = self.check_expression(&mut binary.right)?;

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
                let expr = self.check_expression(&mut unary.expr)?;
                if unary.kind.can_apply(&expr) {
                    if let UnaryKind::Load = unary.kind {
                        match expr {
                            Type::Ref(reference) | Type::MutRef(reference) => Ok(*reference),
                            _ => Err(Error {
                                kind: ErrorKind::DeferenceNonPointerValue,
                                severity: ErrorSeverity::Error,
                                span: unary.span.clone(),
                            }),
                        }
                    } else {
                        Ok(expr)
                    }
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
                        let expr = &mut call.arguments[i];
                        let item_type = self.check_expression(&mut expr.value)?;
                        if let Some(param) = function.params.get(i) {
                            if !item_type.equal(&param.kind) {
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
            Expression::Grouping(grouping) => self.check_expression(&mut grouping.expr),
            Expression::Cast(cast) => {
                let current_expression = self.check_expression(&mut cast.expr)?;
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
            Expression::Ref(reference) => {
                if let Some(variable) = self.find_variable_by_name(&reference.name) {
                    if !variable.mutable && reference.mutable {
                        return Err(Error {
                            kind: ErrorKind::MutableReferenceNotAllowed,
                            severity: ErrorSeverity::Error,
                            span: reference.span,
                        });
                    }
                    let boxed = Box::from(variable.kind.clone());
                    Ok(if reference.mutable {
                        Type::MutRef(boxed)
                    } else {
                        Type::Ref(boxed)
                    })
                } else {
                    Err(Error {
                        kind: ErrorKind::VariableNotFound(reference.name.clone()),
                        span: expression.span(),
                        severity: ErrorSeverity::Error,
                    })
                }
            }
        }
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

    fn create_function(&mut self, function: &mut Function) -> Result<Type, Error> {
        let mut prototype = self.create_prototype(&function.prototype)?;
        for item in prototype.params.iter() {
            self.variables.push(StoredVariable {
                scope: self.scope + 1,
                name: item.name.clone(),
                kind: item.kind.clone(),
                mutable: true,
            });
        }

        let old_temp_function = self.temp_function.clone();
        self.temp_function = Some(prototype.clone());

        let test = self.check_statements(&mut function.statements)?;
        let returned_kind = Type::merge(&test);

        if prototype.return_kind != Type::Unknown && !prototype.return_kind.equal(&returned_kind) {
            return Err(Error {
                kind: ErrorKind::MismatchedTypes(
                    function.prototype.return_type.clone(),
                    returned_kind,
                ),
                severity: ErrorSeverity::Error,
                span: function.prototype.span,
            });
        }

        prototype.return_kind = returned_kind.clone();
        self.functions.push(prototype.clone());
        self.temp_function = old_temp_function;
        Ok(returned_kind)
    }

    fn create_variable(&self, decl: &mut VariableDeclaration) -> Result<StoredVariable, Error> {
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

        let kind = self.check_expression(&mut decl.value)?;
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

    fn check_statement(
        &mut self,
        statement: &mut Statement,
        types: &mut Vec<Type>,
    ) -> Result<(), Error> {
        Ok(match statement {
            Statement::Expression(expression) => {
                self.check_expression(expression)?;
            }
            Statement::VariableDeclaration(decl) => {
                let variable = self.create_variable(decl)?;
                let variable_kind = variable.kind.clone();
                self.variables.push(variable);
                *decl = VariableDeclaration {
                    mutable: decl.mutable,
                    name: decl.name.clone(),
                    span: decl.span,
                    value: decl.value.clone(),
                    variable_type: variable_kind,
                };
            }
            Statement::Block(block) => {
                self.in_loop += 1;
                let kind = Type::merge(&self.check_statements(&mut block.statements)?);
                self.in_loop -= 1;
                types.push(kind);
            }
            Statement::Loop(block) => {
                self.in_loop += 1;
                let kind = Type::merge(&self.check_statements(&mut block.statements)?);
                self.in_loop -= 1;
                types.push(kind);
            }
            Statement::While(while_block) => {
                let check = self.check_expression(&mut while_block.expression)?;
                if check != Type::Bool {
                    return Err(Error {
                        kind: ErrorKind::MismatchedTypes(Type::Bool, check),
                        severity: ErrorSeverity::Error,
                        span: while_block.expression.span(),
                    });
                }
                self.in_loop += 1;
                let kind = Type::merge(&self.check_statements(&mut while_block.block)?);
                self.in_loop -= 1;
                types.push(kind);
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
                let check = self.check_expression(&mut if_statement.expression)?;
                if check != Type::Bool {
                    return Err(Error {
                        kind: ErrorKind::MismatchedTypes(Type::Bool, check),
                        severity: ErrorSeverity::Error,
                        span: if_statement.expression.span(),
                    });
                }
                let lhs = Type::merge(&self.check_statements(&mut if_statement.true_block)?);

                if !if_statement.else_block.is_empty() {
                    let rhs = Type::merge(&self.check_statements(&mut if_statement.else_block)?);
                    if !lhs.equal(&rhs) {
                        return Err(Error {
                            kind: ErrorKind::MismatchedTypes(lhs, rhs),
                            severity: ErrorSeverity::Error,
                            span: Span::default(),
                        });
                    }
                    types.push(lhs);
                } else {
                    types.push(Type::Void);
                }
            }
            Statement::Prototype(prototype) => {
                let mut stored_prototype = self.create_prototype(prototype)?;
                let mut saved_kind = stored_prototype.return_kind.clone();
                if stored_prototype.return_kind == Type::Unknown {
                    stored_prototype.return_kind = Type::Void;
                    saved_kind = Type::Void;
                }
                self.functions.push(stored_prototype);
                *prototype = Prototype {
                    is_extern: prototype.is_extern,
                    name: prototype.name.clone(),
                    params: prototype.params.clone(),
                    return_type: saved_kind.clone(),
                    span: prototype.span,
                };
                types.push(saved_kind);
            }
            Statement::Function(function) => {
                function.prototype.return_type = self.create_function(function)?;
                types.push(function.prototype.return_type.clone());
            }
            Statement::Return(value) => {
                types.push(if let Some(expression) = &mut value.expression {
                    self.check_expression(expression)?
                } else {
                    Type::Void
                });
            }
            Statement::Unsafe(unsafe_statement) => {
                if self.in_unsafe > 0 {
                    self.warnings.push(Error {
                        kind: ErrorKind::UnnecessaryUnsafe,
                        severity: ErrorSeverity::Warning,
                        span: unsafe_statement.span,
                    });
                }
                self.in_unsafe += 1;
                let kind = Type::merge(&self.check_statements(&mut unsafe_statement.statements)?);
                self.in_unsafe -= 1;
                types.push(kind);
            }
        })
    }

    pub fn check_statements(&mut self, statements: &mut [Statement]) -> Result<Vec<Type>, Error> {
        self.enter_scope();
        let mut iter = statements.iter_mut();
        let mut result = vec![];
        while let Some(statement) = iter.next() {
            self.check_statement(statement, &mut result)?;
        }
        self.leave_scope();

        for kind in result.iter() {
            if kind == &Type::Unknown {
                // FIXME: statements[0].span() may crash
                return Err(Error {
                    kind: ErrorKind::AmbiguousType,
                    severity: ErrorSeverity::Error,
                    span: statements[0].span(),
                });
            }
        }

        Ok(result)
    }

    pub fn lint(&mut self, statements: &mut [Statement]) -> Result<Vec<Error>, Error> {
        self.check_statements(statements)?;
        Ok(self.warnings.clone())
    }
}
