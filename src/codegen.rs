use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    passes::PassManager,
    types::{BasicType, BasicTypeEnum},
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace, FloatPredicate, IntPredicate,
};

use crate::ast::{
    Array, ArrayAccess, Assignment, AssignmentKind, Binary, BinaryKind, Call, Cast, Expression,
    Function, If, Literal, LiteralKind, Prototype, Statement, Type, Unary, UnaryKind,
    VariableDeclaration, While,
};

#[derive(Debug, Clone)]
pub struct StoredVariable<'a> {
    pub name: String,
    pub scope: usize,
    pub kind: BasicTypeEnum<'a>,
    pub value: PointerValue<'a>,
}

pub struct Codegen<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub module: &'a Module<'ctx>,
    pub variables: Vec<StoredVariable<'a>>,
    pub scope: usize,
    pub current_function: Option<FunctionValue<'a>>,
    pub begin_block: Option<BasicBlock<'a>>,
    pub break_block: Option<BasicBlock<'a>>,
    pub continue_block: Option<BasicBlock<'a>>,
    pub return_value: Option<BasicValueEnum<'a>>,
    pub need_br: bool,
}

// using unwrap and unreachable, because it should be checked in typechecker.rs
impl<'a, 'ctx> Codegen<'a, 'ctx> {
    fn find_variable_by_name(&self, name: &str) -> &StoredVariable<'a> {
        self.variables
            .iter()
            .find(|item| item.name == name && self.scope >= item.scope)
            .unwrap()
    }

    fn enter_scope(&mut self) {
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

    fn literal(&self, literal: &Literal) -> Option<BasicValueEnum<'a>> {
        Some(match &literal.kind {
            LiteralKind::Boolean(value) => BasicValueEnum::IntValue(
                self.context
                    .bool_type()
                    .const_int(if *value { 1 } else { 0 }, false),
            ),
            LiteralKind::Char(_) => unimplemented!(),
            LiteralKind::String(value) => BasicValueEnum::PointerValue(
                self.builder
                    .build_global_string_ptr(value, ".str")
                    .as_pointer_value(),
            ),
            LiteralKind::Identifier(name) => self
                .builder
                .build_load(self.find_variable_by_name(name).value, ".load_identifier"),
            LiteralKind::Integer(value) => {
                BasicValueEnum::IntValue(self.context.i64_type().const_int(*value as u64, true))
            }
        })
    }

    fn add(&self, lhs: BasicValueEnum<'a>, rhs: BasicValueEnum<'a>) -> Option<BasicValueEnum<'a>> {
        Some(match (lhs, rhs) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                BasicValueEnum::IntValue(self.builder.build_int_add(lhs, rhs, ".addi"))
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                BasicValueEnum::FloatValue(self.builder.build_float_add(lhs, rhs, ".addf"))
            }
            _ => unreachable!(),
        })
    }

    fn sub(&self, lhs: BasicValueEnum<'a>, rhs: BasicValueEnum<'a>) -> Option<BasicValueEnum<'a>> {
        Some(match (lhs, rhs) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                BasicValueEnum::IntValue(self.builder.build_int_sub(lhs, rhs, ".subi"))
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                BasicValueEnum::FloatValue(self.builder.build_float_sub(lhs, rhs, ".subf"))
            }
            _ => unreachable!(),
        })
    }

    fn mul(&self, lhs: BasicValueEnum<'a>, rhs: BasicValueEnum<'a>) -> Option<BasicValueEnum<'a>> {
        Some(match (lhs, rhs) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                BasicValueEnum::IntValue(self.builder.build_int_mul(lhs, rhs, ".muli"))
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                BasicValueEnum::FloatValue(self.builder.build_float_mul(lhs, rhs, ".mulf"))
            }
            _ => unreachable!(),
        })
    }

    fn div(&self, lhs: BasicValueEnum<'a>, rhs: BasicValueEnum<'a>) -> Option<BasicValueEnum<'a>> {
        Some(match (lhs, rhs) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                BasicValueEnum::IntValue(self.builder.build_int_signed_div(lhs, rhs, ".divi"))
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                BasicValueEnum::FloatValue(self.builder.build_float_div(lhs, rhs, ".divf"))
            }
            _ => unreachable!(),
        })
    }

    fn cmp(
        &self,
        lhs: BasicValueEnum<'a>,
        rhs: BasicValueEnum<'a>,
        int_op: IntPredicate,
        float_op: FloatPredicate,
    ) -> Option<BasicValueEnum<'a>> {
        Some(match (lhs, rhs) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                BasicValueEnum::IntValue(self.builder.build_int_compare(int_op, lhs, rhs, ".cmpi"))
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                BasicValueEnum::IntValue(
                    self.builder
                        .build_float_compare(float_op, lhs, rhs, ".cmpf"),
                )
            }
            _ => unreachable!(),
        })
    }

    fn modulo(
        &self,
        lhs: BasicValueEnum<'a>,
        rhs: BasicValueEnum<'a>,
    ) -> Option<BasicValueEnum<'a>> {
        let lhs = lhs.into_int_value();
        let rhs = rhs.into_int_value();
        let result = self.builder.build_int_signed_rem(lhs, rhs, ".remi");
        Some(result.as_basic_value_enum())
    }

    fn binary(&mut self, binary: &Binary) -> Option<BasicValueEnum<'a>> {
        let lhs = self.expression(&binary.left).unwrap();
        let rhs = self.expression(&binary.right).unwrap();
        let lhs = if let BasicValueEnum::PointerValue(ptr) = lhs {
            self.builder.build_load(ptr, ".load_binary")
        } else {
            lhs
        };
        let rhs = if let BasicValueEnum::PointerValue(ptr) = rhs {
            self.builder.build_load(ptr, ".load_binary")
        } else {
            rhs
        };

        let result = match binary.kind {
            BinaryKind::Plus => self.add(lhs, rhs),
            BinaryKind::Minus => self.sub(lhs, rhs),
            BinaryKind::Asterisk => self.mul(lhs, rhs),
            BinaryKind::Slash => self.div(lhs, rhs),
            BinaryKind::DoubleEqual => self.cmp(lhs, rhs, IntPredicate::EQ, FloatPredicate::OEQ),
            BinaryKind::BangEqual => self.cmp(lhs, rhs, IntPredicate::NE, FloatPredicate::ONE),
            BinaryKind::Great => self.cmp(lhs, rhs, IntPredicate::SGT, FloatPredicate::OGT),
            BinaryKind::GreatEqual => self.cmp(lhs, rhs, IntPredicate::SGE, FloatPredicate::OGE),
            BinaryKind::Less => self.cmp(lhs, rhs, IntPredicate::SLT, FloatPredicate::OLT),
            BinaryKind::LessEqual => self.cmp(lhs, rhs, IntPredicate::SLE, FloatPredicate::OLE),
            BinaryKind::RightShift => unimplemented!(),
            BinaryKind::LeftShift => unimplemented!(),
            BinaryKind::Modulo => self.modulo(lhs, rhs),
        };

        result
    }

    fn unary(&mut self, unary: &Unary) -> Option<BasicValueEnum<'a>> {
        let value = self.expression(&unary.expr).unwrap();
        Some(match unary.kind {
            UnaryKind::Not => {
                if let BasicValueEnum::IntValue(value) = value {
                    return Some(BasicValueEnum::IntValue(
                        self.builder.build_not(value, ".not"),
                    ));
                }
                unreachable!()
            }
            UnaryKind::Minus => match value {
                BasicValueEnum::IntValue(value) => {
                    BasicValueEnum::IntValue(self.builder.build_int_neg(value, ".negi"))
                }
                BasicValueEnum::FloatValue(value) => {
                    BasicValueEnum::FloatValue(self.builder.build_float_neg(value, ".negi"))
                }
                _ => unreachable!(),
            },
            UnaryKind::Plus => value,
            UnaryKind::Load => self.builder.build_load(value.into_pointer_value(), ".load"),
        })
    }

    fn call(&mut self, call: &Call) -> Option<BasicValueEnum<'a>> {
        let function = self.module.get_function(&call.name).unwrap();
        let args = call
            .arguments
            .iter()
            .map(|item| self.expression(&item.value).unwrap().into())
            .collect::<Vec<BasicMetadataValueEnum<'a>>>();
        self.builder
            .build_call(function, &args, &call.name)
            .try_as_basic_value()
            .left()
    }

    fn from_type_to_llvm_basic_type(&self, from: &Type) -> Option<BasicTypeEnum<'ctx>> {
        match from {
            Type::I8 | Type::U8 | Type::CChar | Type::Char => Some(self.context.i8_type().into()),
            Type::I16 | Type::U16 => Some(self.context.i16_type().into()),
            Type::I32 | Type::U32 | Type::CInt => Some(self.context.i32_type().into()),
            Type::I64 | Type::U64 | Type::USize => Some(self.context.i64_type().into()),
            Type::F32 => Some(self.context.f32_type().into()),
            Type::F64 => Some(self.context.f64_type().into()),
            Type::Bool => Some(self.context.bool_type().into()),
            Type::String => Some(
                self.context
                    .i8_type()
                    .ptr_type(AddressSpace::Generic)
                    .as_basic_type_enum(),
            ),
            Type::Raw(value) => Some(
                self.from_type_to_llvm_basic_type(value)
                    .unwrap()
                    .ptr_type(AddressSpace::Generic)
                    .as_basic_type_enum(),
            ),
            Type::Void => None,
            Type::Unknown => unreachable!(),
            Type::Ref(inner) | Type::MutRef(inner) => Some(
                self.from_type_to_llvm_basic_type(inner)
                    .unwrap()
                    .ptr_type(AddressSpace::Generic)
                    .as_basic_type_enum(),
            ),
            Type::Array(inner) => Some(
                self.from_type_to_llvm_basic_type(inner)
                    .unwrap()
                    .ptr_type(AddressSpace::Generic)
                    .as_basic_type_enum(),
            ),
        }
    }

    fn cast(&mut self, cast: &Cast) -> Option<BasicValueEnum<'a>> {
        let expression = self.expression(&cast.expr).unwrap();
        let target_type = self.from_type_to_llvm_basic_type(&cast.kind);

        // TODO: We should use different instructions for different types,
        //       For now we just use bitcast
        // let current_type = expression.get_type();

        Some(
            self.builder
                .build_bitcast(expression, target_type.unwrap(), ".bitcast"),
        )
    }

    fn array(&mut self, array: &Array) -> Option<BasicValueEnum<'a>> {
        let kind = self.from_type_to_llvm_basic_type(&array.kind).unwrap();
        let alloca = self.builder.build_array_alloca(
            kind,
            self.context
                .i64_type()
                .const_int(array.values.len() as u64, false),
            ".alloca_array",
        );

        for (index, expression) in array.values.iter().enumerate() {
            unsafe {
                self.builder.build_store(
                    self.builder.build_in_bounds_gep(
                        alloca,
                        &[self.context.i64_type().const_int(index as u64, false)],
                        ".load",
                    ),
                    self.expression(expression).unwrap(),
                );
            }
        }

        Some(alloca.as_basic_value_enum())
    }

    fn array_access(&mut self, array_access: &ArrayAccess) -> Option<BasicValueEnum<'a>> {
        let variable = self.expression(&array_access.expression).unwrap();
        let index = self.expression(&array_access.index).unwrap();

        unsafe {
            let ptr = self.builder.build_in_bounds_gep(
                variable.into_pointer_value(),
                &[index.into_int_value()],
                ".gep_array_access",
            );
            Some(self.builder.build_load(ptr, ".load_array_access"))
        }
    }

    fn assignment(&mut self, assignment: &Assignment) -> Option<BasicValueEnum<'a>> {
        let lhs = self.expression_lhs(&assignment.left).unwrap();
        let rhs = self.expression(&assignment.right).unwrap();
        let rhs = match assignment.kind {
            AssignmentKind::Equal => rhs,
            AssignmentKind::PlusEqual => {
                let lhs = self.expression(&assignment.left).unwrap();
                self.add(lhs, rhs).unwrap()
            }
            AssignmentKind::MinusEqual => {
                let lhs = self.expression(&assignment.left).unwrap();
                self.sub(lhs, rhs).unwrap()
            }
            AssignmentKind::AsteriskEqual => {
                let lhs = self.expression(&assignment.left).unwrap();
                self.mul(lhs, rhs).unwrap()
            }
            AssignmentKind::SlashEqual => {
                let lhs = self.expression(&assignment.left).unwrap();
                self.div(lhs, rhs).unwrap()
            }
            AssignmentKind::RightShiftEqual => todo!(),
            AssignmentKind::LeftShiftEqual => todo!(),
            AssignmentKind::ModuloEqual => {
                let lhs = self.expression(&assignment.left).unwrap();
                self.modulo(lhs, rhs).unwrap()
            }
        };

        self.builder.build_store(lhs, rhs);
        None
    }

    fn expression(&mut self, expression: &Expression) -> Option<BasicValueEnum<'a>> {
        match expression {
            Expression::Literal(literal) => self.literal(literal),
            Expression::Binary(binary) => self.binary(binary),
            Expression::Unary(unary) => self.unary(unary),
            Expression::Call(call) => self.call(call),
            Expression::Grouping(grouping) => self.expression(&grouping.expr),
            Expression::Cast(cast) => self.cast(cast),
            Expression::Ref(reference) => Some(
                self.find_variable_by_name(&reference.name)
                    .value
                    .as_basic_value_enum(),
            ),
            Expression::Array(array) => self.array(array),
            Expression::ArrayAccess(array_access) => self.array_access(array_access),
            Expression::Assignment(assignment) => self.assignment(assignment),
        }
    }

    fn expression_lhs(&mut self, expression: &Expression) -> Option<PointerValue<'a>> {
        match expression {
            Expression::Literal(literal) => {
                if let LiteralKind::Identifier(name) = &literal.kind {
                    let var = self.find_variable_by_name(name);
                    return Some(var.value);
                }
                unreachable!()
            }
            Expression::Unary(unary) => {
                if let UnaryKind::Load = unary.kind {
                    return Some(
                        self.builder
                            .build_load(self.expression_lhs(&unary.expr).unwrap(), ".load_lhs")
                            .into_pointer_value(),
                    );
                }
                unreachable!()
            }
            Expression::Grouping(inner) => self.expression_lhs(&inner.expr),
            Expression::ArrayAccess(array_access) => Some(
                self.array_access(array_access)
                    .unwrap()
                    .into_pointer_value(),
            ),
            _ => unreachable!(),
        }
    }

    fn variable_declaration(&mut self, decl: &VariableDeclaration) {
        let value = self.expression(&decl.value).unwrap();
        let alloca = self.builder.build_alloca(value.get_type(), &decl.name);
        self.builder.build_store(alloca, value);
        self.variables.push(StoredVariable {
            kind: value.get_type(),
            name: decl.name.clone(),
            scope: self.scope,
            value: alloca,
        })
    }

    fn loop_statement(&mut self, statements: &Vec<Statement>) {
        let function = self.current_function.unwrap();
        let loop_block = self.context.append_basic_block(function, ".loop");
        let loop_end = self.context.append_basic_block(function, ".loop_end");
        let old_continue_block = self.continue_block;
        let old_break_block = self.break_block;
        self.break_block = Some(loop_end);
        self.continue_block = Some(loop_block);
        self.builder.build_unconditional_branch(loop_block);
        self.builder.position_at_end(loop_block);
        if !self.statements(statements) {
            self.builder.build_unconditional_branch(loop_block);
        }
        self.break_block = old_break_block;
        self.continue_block = old_continue_block;
        self.builder.position_at_end(loop_end);
    }

    fn while_statement(&mut self, while_statement: &While) {
        let function = self.current_function.unwrap();
        let while_check = self.context.append_basic_block(function, ".while_check");
        let while_true = self.context.append_basic_block(function, ".while_true");
        let while_end = self.context.append_basic_block(function, ".while_end");
        let old_break_block = self.break_block;
        let old_continue_block = self.continue_block;

        self.break_block = Some(while_end);
        self.continue_block = Some(while_check);
        self.builder.build_unconditional_branch(while_check);
        self.builder.position_at_end(while_check);
        let condition = self
            .expression(&while_statement.expression)
            .unwrap()
            .into_int_value();

        self.builder
            .build_conditional_branch(condition, while_true, while_end);
        self.builder.position_at_end(while_true);
        self.statements(&while_statement.block);
        self.builder.build_unconditional_branch(while_check);
        self.break_block = old_break_block;
        self.continue_block = old_continue_block;
        if !self.need_br {
            self.builder.position_at_end(while_end);
        }
    }

    fn break_statement(&mut self, is_break: bool) {
        self.builder.build_unconditional_branch(if is_break {
            self.break_block.unwrap()
        } else {
            self.continue_block.unwrap()
        });
    }

    fn if_statement(&mut self, if_statement: &If) {
        let function = self.current_function.unwrap();
        let if_true = self.context.append_basic_block(function, ".if_true");
        let if_end = self.context.append_basic_block(function, ".if_end");
        let if_else = if if_statement.else_block.is_empty() {
            None
        } else {
            Some(self.context.append_basic_block(function, ".if_else"))
        };
        let condition = self
            .expression(&if_statement.expression)
            .unwrap()
            .into_int_value();
        self.builder.build_conditional_branch(
            condition,
            if_true,
            if let Some(if_else) = if_else {
                if_else
            } else {
                if_end
            },
        );
        self.builder.position_at_end(if_true);
        if !self.statements(&if_statement.true_block) {
            self.builder.build_unconditional_branch(if_end);
        }

        if let Some(if_else) = if_else {
            self.builder.position_at_end(if_else);
            if !self.statements(&if_statement.else_block) {
                self.builder.build_unconditional_branch(if_end);
            }
        }

        self.builder.position_at_end(if_end);
    }

    fn prototype(&mut self, prototype: &Prototype, linkage: Linkage) -> FunctionValue<'ctx> {
        let types = prototype
            .params
            .iter()
            .map(|item| {
                self.from_type_to_llvm_basic_type(&item.kind)
                    .unwrap()
                    .into()
            })
            .collect::<Vec<_>>();
        let parsed_kind = self.from_type_to_llvm_basic_type(&prototype.return_type);
        let prototype_type = if let Some(value) = parsed_kind {
            value.fn_type(&types, false)
        } else {
            self.context.void_type().fn_type(&types, false)
        };
        self.module
            .add_function(&prototype.name, prototype_type, Some(linkage))
    }

    fn function(&mut self, function: &Function) {
        let current_function = self.prototype(&function.prototype, Linkage::Internal);
        self.current_function = Some(current_function);

        let block = self.context.append_basic_block(current_function, ".entry");
        self.builder.position_at_end(block);

        current_function
            .get_param_iter()
            .zip(&function.prototype.params)
            .for_each(|params| {
                let aloca = self
                    .builder
                    .build_alloca(params.0.get_type(), &params.1.name);
                self.builder.build_store(aloca, params.0);
                self.variables.push(StoredVariable {
                    kind: params.0.get_type(),
                    name: params.1.name.clone(),
                    scope: self.scope + 1,
                    value: aloca,
                });
            });

        if !self.statements(&function.statements) {
            self.builder.build_return(None);
        }
        self.current_function = None;
    }

    fn statement(&mut self, statement: &Statement) -> bool {
        match statement {
            Statement::Expression(expression) => {
                self.expression(expression);
            }
            Statement::VariableDeclaration(decl) => self.variable_declaration(decl),
            Statement::Block(block) => {
                self.statements(&block.statements);
            }
            Statement::Loop(loop_statement) => self.loop_statement(&loop_statement.statements),
            Statement::While(while_statement) => self.while_statement(while_statement),
            Statement::Break(_) => {
                self.break_statement(true);
                return true;
            }
            Statement::Continue(_) => {
                self.break_statement(false);
                return true;
            }
            Statement::If(if_statement) => self.if_statement(if_statement),
            Statement::Prototype(prototype) => {
                self.prototype(prototype, Linkage::External);
            }
            Statement::Function(function) => self.function(function),
            Statement::Return(return_statement) => {
                if let Some(expression) = &return_statement.expression {
                    if let Some(value) = self.expression(&expression) {
                        self.builder.build_return(Some(&value));
                        return true;
                    }
                }
                self.builder.build_return(None);
                return true;
            }
            Statement::Unsafe(unsafe_statement) => {
                self.statements(&unsafe_statement.statements);
            }
        }
        return false;
    }

    pub fn statements(&mut self, statements: &Vec<Statement>) -> bool {
        self.enter_scope();
        for statement in statements.iter() {
            let statement_terminated = self.statement(statement);
            if statement_terminated {
                self.leave_scope();
                return true;
            }
        }
        self.leave_scope();
        false
    }

    pub fn codegen(&mut self, statements: &Vec<Statement>) -> bool {
        // I made a seperate function so I can prepare builtin functions
        self.statements(statements)
    }
}
