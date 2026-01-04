use std::collections::HashMap;

use inkwell::{
    AddressSpace, FloatPredicate, IntPredicate,
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    types::BasicTypeEnum,
    values::{BasicValueEnum, FunctionValue, InstructionOpcode, PointerValue},
};

use crate::{
    parser::{Expr, Statement, Stmt, Type},
    token::{Literal, NumericType, Operator},
};

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,

    main_function: FunctionValue<'ctx>,
    current_block: BasicBlock<'ctx>,

    variables: HashMap<String, (PointerValue<'ctx>, BasicTypeEnum<'ctx>)>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let builder = context.create_builder();
        let module = context.create_module("main");
        let i32_type = context.i32_type();
        let main_function_type = i32_type.fn_type(&[], false);
        let main_function = module.add_function("", main_function_type, None);
        let entry_block = context.append_basic_block(main_function, "entry");

        Self {
            context,
            builder,
            module,

            main_function,
            current_block: entry_block,

            variables: HashMap::new(),
        }
    }

    fn switch_block(&mut self, block: BasicBlock<'ctx>) {
        self.current_block = block;
        self.builder.position_at_end(block);
    }

    fn get_basic_type(&self, ty: &Type) -> anyhow::Result<BasicTypeEnum<'ctx>> {
        Ok(match ty {
            Type::Boolean => self.context.bool_type().into(),
            Type::Void => anyhow::bail!("Void is not a basic type"),
            Type::Numeric(numeric) => match numeric {
                NumericType::I8 | NumericType::U8 => self.context.i8_type().into(),
                NumericType::I16 | NumericType::U16 => self.context.i16_type().into(),
                NumericType::I32 | NumericType::U32 => self.context.i32_type().into(),
                NumericType::I64 | NumericType::U64 | NumericType::ISize | NumericType::USize => {
                    self.context.i64_type().into()
                }
                NumericType::F32 => self.context.f32_type().into(),
                NumericType::F64 => self.context.f64_type().into(),
            },
            Type::Named(name) => match name.as_str() {
                "String" => self.context.ptr_type(AddressSpace::default()).into(),
                "char" => self.context.i8_type().into(),
                _ => anyhow::bail!("Unknown named type: {}", name),
            },
            Type::Pointer(_) => self.context.ptr_type(AddressSpace::default()).into(),
            _ => anyhow::bail!("Unsupported type: {:?}", ty),
        })
    }

    fn compile_stmt(&mut self, stmt: &Stmt, function: FunctionValue<'ctx>) -> anyhow::Result<()> {
        match stmt {
            Stmt::Expr(expr) => {
                self.compile_expr(expr)?;
                Ok(())
            }
            Stmt::Scope { statements } => {
                for statement in statements {
                    self.compile_stmt(&statement.stmt, function)?;
                }
                Ok(())
            }
            Stmt::Return { value } => {
                if let Some(expr) = value {
                    let ret_val = self.compile_expr(expr)?.unwrap();
                    self.builder.build_return(Some(&ret_val))?;
                } else {
                    self.builder.build_return(None)?;
                }
                Ok(())
            }
            Stmt::Continue => {
                todo!()
            }
            Stmt::Break => {
                todo!()
            }
            Stmt::Func {
                name,
                params,
                ty,
                body,
            } => {
                let mut function_params = Vec::new();
                let mut variadic = false;
                for param in params {
                    match &param.ty {
                        Type::Boolean => {
                            function_params.push(self.context.bool_type().into());
                        }
                        Type::Void => {}
                        Type::Numeric(numeric) => match numeric {
                            NumericType::I8 | NumericType::U8 => {
                                function_params.push(self.context.i8_type().into());
                            }
                            NumericType::I16 | NumericType::U16 => {
                                function_params.push(self.context.i16_type().into());
                            }
                            NumericType::I32 | NumericType::U32 => {
                                function_params.push(self.context.i32_type().into());
                            }
                            NumericType::I64 | NumericType::U64 => {
                                function_params.push(self.context.i64_type().into());
                            }
                            NumericType::ISize | NumericType::USize => {
                                function_params.push(self.context.i64_type().into()); // ! TODO
                            }
                            NumericType::F32 => {
                                function_params.push(self.context.f32_type().into());
                            }
                            NumericType::F64 => {
                                function_params.push(self.context.f64_type().into());
                            }
                        },
                        Type::Named(name) => match name.as_str() {
                            "String" => {
                                function_params
                                    .push(self.context.ptr_type(AddressSpace::default()).into());
                            }
                            "char" => {
                                function_params.push(self.context.i8_type().into());
                            }
                            _ => anyhow::bail!("Unknown named type: {}", name),
                        },
                        Type::Variadic => {
                            if variadic {
                                anyhow::bail!(
                                    "Multiple variadic parameters in function declaration '{}'",
                                    name
                                );
                            }
                            variadic = true;
                        }
                        Type::Pointer(_) => {
                            function_params
                                .push(self.context.ptr_type(AddressSpace::default()).into());
                        }
                    }
                }
                let function_type = match &ty {
                    Type::Boolean => self.context.bool_type().fn_type(&function_params, variadic),
                    Type::Void => self.context.void_type().fn_type(&function_params, variadic),
                    Type::Numeric(numeric) => match numeric {
                        NumericType::I8 | NumericType::U8 => {
                            self.context.i8_type().fn_type(&function_params, variadic)
                        }
                        NumericType::I16 | NumericType::U16 => {
                            self.context.i16_type().fn_type(&function_params, variadic)
                        }
                        NumericType::I32 | NumericType::U32 => {
                            self.context.i32_type().fn_type(&function_params, variadic)
                        }
                        NumericType::I64 | NumericType::U64 => {
                            self.context.i64_type().fn_type(&function_params, variadic)
                        }
                        NumericType::ISize | NumericType::USize => {
                            self.context.i64_type().fn_type(&function_params, variadic)
                        }
                        NumericType::F32 => {
                            self.context.f32_type().fn_type(&function_params, variadic)
                        }
                        NumericType::F64 => {
                            self.context.f64_type().fn_type(&function_params, variadic)
                        }
                    },
                    Type::Named(name) => match name.as_str() {
                        "String" => self
                            .context
                            .ptr_type(AddressSpace::default())
                            .fn_type(&function_params, variadic),
                        "char" => self.context.i8_type().fn_type(&function_params, variadic),
                        _ => anyhow::bail!("Unknown named type: {}", name),
                    },
                    Type::Variadic => {
                        anyhow::bail!("Function '{}' type can't be variadic", name);
                    }
                    Type::Pointer(_) => self
                        .context
                        .ptr_type(AddressSpace::default())
                        .fn_type(&function_params, variadic),
                };

                let saved_vars = self.variables.clone();
                self.variables.clear();

                let func = self.module.add_function(name, function_type, None);
                let last_block = self.current_block;
                let block = self.context.append_basic_block(func, "entry");
                self.switch_block(block);

                for (index, param) in params.iter().enumerate() {
                    let basic_type = self.get_basic_type(&param.ty)?;
                    let ptr = self.builder.build_alloca(basic_type, &param.name)?;
                    let func_param = func.get_nth_param(index as u32).unwrap();
                    self.builder.build_store(ptr, func_param)?;
                    self.variables.insert(param.name.clone(), (ptr, basic_type));
                }

                match &body.stmt {
                    Stmt::Scope { statements } => {
                        for statement in statements {
                            self.compile_stmt(&statement.stmt, func)?;
                        }
                    }
                    _ => anyhow::bail!("Function body must be a scope"),
                }

                self.variables = saved_vars;
                self.switch_block(last_block);

                Ok(())
            }
            Stmt::Extern { name, params, ty } => {
                let mut function_params = Vec::new();
                let mut variadic = false;
                for param in params {
                    match &param.ty {
                        Type::Boolean => {
                            function_params.push(self.context.bool_type().into());
                        }
                        Type::Void => {}
                        Type::Numeric(numeric) => match numeric {
                            NumericType::I8 | NumericType::U8 => {
                                function_params.push(self.context.i8_type().into());
                            }
                            NumericType::I16 | NumericType::U16 => {
                                function_params.push(self.context.i16_type().into());
                            }
                            NumericType::I32 | NumericType::U32 => {
                                function_params.push(self.context.i32_type().into());
                            }
                            NumericType::I64 | NumericType::U64 => {
                                function_params.push(self.context.i64_type().into());
                            }
                            NumericType::ISize | NumericType::USize => {
                                function_params.push(self.context.i64_type().into()); // ! TODO
                            }
                            NumericType::F32 => {
                                function_params.push(self.context.f32_type().into());
                            }
                            NumericType::F64 => {
                                function_params.push(self.context.f64_type().into());
                            }
                        },
                        Type::Named(name) => match name.as_str() {
                            "String" => {
                                function_params
                                    .push(self.context.ptr_type(AddressSpace::default()).into());
                            }
                            "char" => {
                                function_params.push(self.context.i8_type().into());
                            }
                            _ => anyhow::bail!("Unknown named type: {}", name),
                        },
                        Type::Variadic => {
                            if variadic {
                                anyhow::bail!(
                                    "Multiple variadic parameters in function declaration '{}'",
                                    name
                                );
                            }
                            variadic = true;
                        }
                        Type::Pointer(_) => {
                            function_params
                                .push(self.context.ptr_type(AddressSpace::default()).into());
                        }
                    }
                }
                let function_type = match &ty {
                    Type::Boolean => self.context.bool_type().fn_type(&function_params, variadic),
                    Type::Void => self.context.void_type().fn_type(&function_params, variadic),
                    Type::Numeric(numeric) => match numeric {
                        NumericType::I8 | NumericType::U8 => {
                            self.context.i8_type().fn_type(&function_params, variadic)
                        }
                        NumericType::I16 | NumericType::U16 => {
                            self.context.i16_type().fn_type(&function_params, variadic)
                        }
                        NumericType::I32 | NumericType::U32 => {
                            self.context.i32_type().fn_type(&function_params, variadic)
                        }
                        NumericType::I64 | NumericType::U64 => {
                            self.context.i64_type().fn_type(&function_params, variadic)
                        }
                        NumericType::ISize | NumericType::USize => {
                            self.context.i64_type().fn_type(&function_params, variadic)
                        }
                        NumericType::F32 => {
                            self.context.f32_type().fn_type(&function_params, variadic)
                        }
                        NumericType::F64 => {
                            self.context.f64_type().fn_type(&function_params, variadic)
                        }
                    },
                    Type::Named(name) => match name.as_str() {
                        "String" => self
                            .context
                            .ptr_type(AddressSpace::default())
                            .fn_type(&function_params, variadic),
                        "char" => self.context.i8_type().fn_type(&function_params, variadic),
                        _ => anyhow::bail!("Unknown named type: {}", name),
                    },
                    Type::Variadic => {
                        anyhow::bail!("Function '{}' type can't be variadic", name);
                    }
                    Type::Pointer(_) => self
                        .context
                        .ptr_type(AddressSpace::default())
                        .fn_type(&function_params, variadic),
                };

                self.module.add_function(name, function_type, None);

                Ok(())
            }
            Stmt::Let { name, value, ty } => {
                let init_val = self.compile_expr(value)?.unwrap();
                let val_type = init_val.get_type();
                let decl_ty = ty.as_ref().map(|x| self.get_basic_type(x)).transpose()?;
                if let Some(decl_ty) = decl_ty
                    && decl_ty != val_type
                {
                    anyhow::bail!(
                        "Type mismatch in variable declaration '{}': expected {:?}, got {:?}",
                        name,
                        decl_ty,
                        val_type
                    );
                }
                let ptr = self.builder.build_alloca(val_type, name)?;
                self.builder.build_store(ptr, init_val)?;
                self.variables.insert(name.clone(), (ptr, val_type));
                Ok(())
            }
            Stmt::While { condition, body } => {
                let while_before = self.context.append_basic_block(function, "while_before");
                let while_then = self.context.append_basic_block(function, "while_then");
                let while_after = self.context.append_basic_block(function, "while_after");

                if let Some(last_instruction) = self.current_block.get_last_instruction()
                    && last_instruction.get_opcode() != InstructionOpcode::Return
                {
                    self.builder.build_unconditional_branch(while_before)?;
                }

                self.switch_block(while_before);

                let compiled_condition = self.compile_expr(condition)?.unwrap().into_int_value();

                self.builder.build_conditional_branch(
                    compiled_condition,
                    while_then,
                    while_after,
                )?;

                self.switch_block(while_then);

                let block = match &body.stmt {
                    Stmt::Scope { statements } => statements,
                    _ => anyhow::bail!("While body must be a scope"),
                };
                for statement in block {
                    self.compile_stmt(&statement.stmt, function)?;
                }

                if let Some(last_instruction) = while_then.get_last_instruction()
                    && last_instruction.get_opcode() != inkwell::values::InstructionOpcode::Return
                {
                    self.builder.build_unconditional_branch(while_before)?;
                }

                self.switch_block(while_after);

                Ok(())
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_val = self.compile_expr(condition)?.unwrap();

                let then_block = self.context.append_basic_block(function, "then");
                let else_block = self.context.append_basic_block(function, "else");
                let merge_block = self.context.append_basic_block(function, "ifcont");

                self.builder.build_conditional_branch(
                    cond_val.into_int_value(),
                    then_block,
                    else_block,
                )?;

                self.switch_block(then_block);
                for statement in then_branch {
                    self.compile_stmt(&statement.stmt, function)?;
                }
                if self.current_block.get_terminator().is_none() {
                    self.builder.build_unconditional_branch(merge_block)?;
                }

                self.switch_block(else_block);
                if let Some(else_branch) = else_branch {
                    for statement in else_branch {
                        self.compile_stmt(&statement.stmt, function)?;
                    }
                }
                if self.current_block.get_terminator().is_none() {
                    self.builder.build_unconditional_branch(merge_block)?;
                }

                self.switch_block(merge_block);

                Ok(())
            }
            Stmt::Semicolon => Ok(()),
        }
    }

    fn compile_expr(&mut self, expr: &Expr) -> anyhow::Result<Option<BasicValueEnum<'ctx>>> {
        match expr {
            Expr::Literal(lit) => match lit {
                Literal::Numeric(lit) => {
                    // Split at underscore using find instead of split to avoid allocation
                    let value = if let Some(pos) = lit.find('_') {
                        &lit[..pos]
                    } else {
                        lit
                    };
                    let Some(num_type) = NumericType::from_literal(lit)? else {
                        return Ok(if value.contains('.') {
                            Some(BasicValueEnum::FloatValue(
                                self.context.f64_type().const_float(value.parse()?),
                            ))
                        } else {
                            Some(BasicValueEnum::IntValue(
                                self.context
                                    .i32_type()
                                    .const_int(value.parse::<i32>()? as u64, true),
                            ))
                        });
                    };
                    if value.starts_with("-")
                        && matches!(
                            num_type,
                            NumericType::U8
                                | NumericType::U16
                                | NumericType::U32
                                | NumericType::U64
                                | NumericType::USize
                        )
                    {
                        anyhow::bail!("Negative value for unsigned numeric type");
                    }
                    match num_type {
                        NumericType::I8 => Ok(Some(BasicValueEnum::IntValue(
                            self.context
                                .i8_type()
                                .const_int(value.parse::<i8>()? as u64, true),
                        ))),
                        NumericType::U8 => Ok(Some(BasicValueEnum::IntValue(
                            self.context
                                .i8_type()
                                .const_int(value.parse::<u8>()? as u64, false),
                        ))),
                        NumericType::I16 => Ok(Some(BasicValueEnum::IntValue(
                            self.context
                                .i16_type()
                                .const_int(value.parse::<i16>()? as u64, true),
                        ))),
                        NumericType::U16 => Ok(Some(BasicValueEnum::IntValue(
                            self.context
                                .i16_type()
                                .const_int(value.parse::<u16>()? as u64, false),
                        ))),
                        NumericType::I32 => Ok(Some(BasicValueEnum::IntValue(
                            self.context
                                .i32_type()
                                .const_int(value.parse::<i32>()? as u64, true),
                        ))),
                        NumericType::U32 => Ok(Some(BasicValueEnum::IntValue(
                            self.context
                                .i32_type()
                                .const_int(value.parse::<u32>()? as u64, false),
                        ))),
                        NumericType::I64 | NumericType::ISize => {
                            Ok(Some(BasicValueEnum::IntValue(
                                self.context
                                    .i64_type()
                                    .const_int(value.parse::<i64>()? as u64, true),
                            )))
                        }
                        NumericType::U64 | NumericType::USize => {
                            Ok(Some(BasicValueEnum::IntValue(
                                self.context
                                    .i64_type()
                                    .const_int(value.parse::<u64>()?, false),
                            )))
                        }
                        NumericType::F32 => Ok(Some(BasicValueEnum::FloatValue(
                            self.context.f32_type().const_float(value.parse()?),
                        ))),
                        NumericType::F64 => Ok(Some(BasicValueEnum::FloatValue(
                            self.context.f64_type().const_float(value.parse()?),
                        ))),
                    }
                }
                Literal::String(s) => {
                    let str_const = self
                        .builder
                        .build_global_string_ptr(&unescaper::unescape(s)?, "str")?;
                    Ok(Some(BasicValueEnum::PointerValue(
                        str_const.as_pointer_value(),
                    )))
                }
                Literal::Char(c) => Ok(Some(BasicValueEnum::IntValue(
                    self.context.i8_type().const_int(*c as u8 as u64, false),
                ))),
                Literal::Boolean(s) => Ok(Some(BasicValueEnum::IntValue(
                    self.context
                        .bool_type()
                        .const_int(if s == "true" { 1 } else { 0 }, false),
                ))),
            },
            Expr::Variable(name) => {
                let (ptr, ty) = self
                    .variables
                    .get(name)
                    .ok_or_else(|| anyhow::anyhow!("Undefined variable: {}", name))?;
                Ok(Some(self.builder.build_load(*ty, *ptr, name)?))
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let left_val = self.compile_expr(left)?;
                let right_val = self.compile_expr(right)?;
                match (left_val, right_val, operator) {
                    (
                        Some(BasicValueEnum::IntValue(lhs)),
                        Some(BasicValueEnum::IntValue(rhs)),
                        Operator::Plus,
                    ) => Ok(Some(BasicValueEnum::IntValue(
                        self.builder.build_int_add(lhs, rhs, "tmpadd")?,
                    ))),
                    (
                        Some(BasicValueEnum::FloatValue(lhs)),
                        Some(BasicValueEnum::FloatValue(rhs)),
                        Operator::Plus,
                    ) => Ok(Some(BasicValueEnum::FloatValue(
                        self.builder.build_float_add(lhs, rhs, "tmpadd")?,
                    ))),

                    (
                        Some(BasicValueEnum::IntValue(lhs)),
                        Some(BasicValueEnum::IntValue(rhs)),
                        Operator::Minus,
                    ) => Ok(Some(BasicValueEnum::IntValue(
                        self.builder.build_int_sub(lhs, rhs, "tmpsub")?,
                    ))),
                    (
                        Some(BasicValueEnum::FloatValue(lhs)),
                        Some(BasicValueEnum::FloatValue(rhs)),
                        Operator::Minus,
                    ) => Ok(Some(BasicValueEnum::FloatValue(
                        self.builder.build_float_sub(lhs, rhs, "tmpsub")?,
                    ))),

                    (
                        Some(BasicValueEnum::IntValue(lhs)),
                        Some(BasicValueEnum::IntValue(rhs)),
                        Operator::Asterisk,
                    ) => Ok(Some(BasicValueEnum::IntValue(
                        self.builder.build_int_mul(lhs, rhs, "tmpmul")?,
                    ))),
                    (
                        Some(BasicValueEnum::FloatValue(lhs)),
                        Some(BasicValueEnum::FloatValue(rhs)),
                        Operator::Asterisk,
                    ) => Ok(Some(BasicValueEnum::FloatValue(
                        self.builder.build_float_mul(lhs, rhs, "tmpmul")?,
                    ))),

                    (
                        Some(BasicValueEnum::IntValue(lhs)),
                        Some(BasicValueEnum::IntValue(rhs)),
                        Operator::Slash,
                    ) => Ok(Some(BasicValueEnum::IntValue(
                        self.builder.build_int_signed_div(lhs, rhs, "tmpdiv")?,
                    ))),
                    (
                        Some(BasicValueEnum::FloatValue(lhs)),
                        Some(BasicValueEnum::FloatValue(rhs)),
                        Operator::Slash,
                    ) => Ok(Some(BasicValueEnum::FloatValue(
                        self.builder.build_float_div(lhs, rhs, "tmpdiv")?,
                    ))),

                    (
                        Some(BasicValueEnum::IntValue(lhs)),
                        Some(BasicValueEnum::IntValue(rhs)),
                        Operator::Percent,
                    ) => Ok(Some(BasicValueEnum::IntValue(
                        self.builder.build_int_signed_rem(lhs, rhs, "tmprem")?,
                    ))),
                    (
                        Some(BasicValueEnum::FloatValue(lhs)),
                        Some(BasicValueEnum::FloatValue(rhs)),
                        Operator::Percent,
                    ) => Ok(Some(BasicValueEnum::FloatValue(
                        self.builder.build_float_rem(lhs, rhs, "tmprem")?,
                    ))),

                    (
                        Some(BasicValueEnum::IntValue(lhs)),
                        Some(BasicValueEnum::IntValue(rhs)),
                        Operator::Equals,
                    ) => Ok(Some(BasicValueEnum::IntValue(
                        self.builder
                            .build_int_compare(IntPredicate::EQ, lhs, rhs, "tmpeq")?,
                    ))),
                    (
                        Some(BasicValueEnum::FloatValue(lhs)),
                        Some(BasicValueEnum::FloatValue(rhs)),
                        Operator::Equals,
                    ) => Ok(Some(BasicValueEnum::IntValue(
                        self.builder
                            .build_float_compare(FloatPredicate::OEQ, lhs, rhs, "tmpeq")?,
                    ))),

                    (
                        Some(BasicValueEnum::IntValue(lhs)),
                        Some(BasicValueEnum::IntValue(rhs)),
                        Operator::NotEquals,
                    ) => Ok(Some(BasicValueEnum::IntValue(
                        self.builder
                            .build_int_compare(IntPredicate::NE, lhs, rhs, "tmpne")?,
                    ))),
                    (
                        Some(BasicValueEnum::FloatValue(lhs)),
                        Some(BasicValueEnum::FloatValue(rhs)),
                        Operator::NotEquals,
                    ) => Ok(Some(BasicValueEnum::IntValue(
                        self.builder
                            .build_float_compare(FloatPredicate::ONE, lhs, rhs, "tmpne")?,
                    ))),

                    (
                        Some(BasicValueEnum::IntValue(lhs)),
                        Some(BasicValueEnum::IntValue(rhs)),
                        Operator::Greater,
                    ) => Ok(Some(BasicValueEnum::IntValue(
                        self.builder
                            .build_int_compare(IntPredicate::SGT, lhs, rhs, "tmpgt")?,
                    ))),
                    (
                        Some(BasicValueEnum::FloatValue(lhs)),
                        Some(BasicValueEnum::FloatValue(rhs)),
                        Operator::Greater,
                    ) => Ok(Some(BasicValueEnum::IntValue(
                        self.builder
                            .build_float_compare(FloatPredicate::OGT, lhs, rhs, "tmpgt")?,
                    ))),

                    (
                        Some(BasicValueEnum::IntValue(lhs)),
                        Some(BasicValueEnum::IntValue(rhs)),
                        Operator::Less,
                    ) => Ok(Some(BasicValueEnum::IntValue(
                        self.builder
                            .build_int_compare(IntPredicate::SLT, lhs, rhs, "tmplt")?,
                    ))),
                    (
                        Some(BasicValueEnum::FloatValue(lhs)),
                        Some(BasicValueEnum::FloatValue(rhs)),
                        Operator::Less,
                    ) => Ok(Some(BasicValueEnum::IntValue(
                        self.builder
                            .build_float_compare(FloatPredicate::OLT, lhs, rhs, "tmplt")?,
                    ))),

                    (
                        Some(BasicValueEnum::IntValue(lhs)),
                        Some(BasicValueEnum::IntValue(rhs)),
                        Operator::GreaterEquals,
                    ) => Ok(Some(BasicValueEnum::IntValue(
                        self.builder
                            .build_int_compare(IntPredicate::SGE, lhs, rhs, "tmpge")?,
                    ))),
                    (
                        Some(BasicValueEnum::FloatValue(lhs)),
                        Some(BasicValueEnum::FloatValue(rhs)),
                        Operator::GreaterEquals,
                    ) => Ok(Some(BasicValueEnum::IntValue(
                        self.builder
                            .build_float_compare(FloatPredicate::OGE, lhs, rhs, "tmpge")?,
                    ))),

                    (
                        Some(BasicValueEnum::IntValue(lhs)),
                        Some(BasicValueEnum::IntValue(rhs)),
                        Operator::LessEquals,
                    ) => Ok(Some(BasicValueEnum::IntValue(
                        self.builder
                            .build_int_compare(IntPredicate::SLE, lhs, rhs, "tmple")?,
                    ))),
                    (
                        Some(BasicValueEnum::FloatValue(lhs)),
                        Some(BasicValueEnum::FloatValue(rhs)),
                        Operator::LessEquals,
                    ) => Ok(Some(BasicValueEnum::IntValue(
                        self.builder
                            .build_float_compare(FloatPredicate::OLE, lhs, rhs, "tmple")?,
                    ))),

                    (
                        Some(BasicValueEnum::IntValue(lhs)),
                        Some(BasicValueEnum::IntValue(rhs)),
                        Operator::LogicalAnd,
                    ) => {
                        let lhsb = self.builder.build_int_compare(
                            IntPredicate::NE,
                            lhs,
                            lhs.get_type().const_zero(),
                            "lhs_bool",
                        )?;

                        let rhsb = self.builder.build_int_compare(
                            IntPredicate::NE,
                            rhs,
                            rhs.get_type().const_zero(),
                            "rhs_bool",
                        )?;

                        Ok(Some(BasicValueEnum::IntValue(
                            self.builder.build_and(lhsb, rhsb, "tmpand")?,
                        )))
                    }
                    (
                        Some(BasicValueEnum::IntValue(lhs)),
                        Some(BasicValueEnum::IntValue(rhs)),
                        Operator::LogicalOr,
                    ) => {
                        let lhsb = self.builder.build_int_compare(
                            IntPredicate::NE,
                            lhs,
                            lhs.get_type().const_zero(),
                            "lhs_bool",
                        )?;

                        let rhsb = self.builder.build_int_compare(
                            IntPredicate::NE,
                            rhs,
                            rhs.get_type().const_zero(),
                            "rhs_bool",
                        )?;

                        Ok(Some(BasicValueEnum::IntValue(
                            self.builder.build_or(lhsb, rhsb, "tmpor")?,
                        )))
                    }
                    (
                        Some(BasicValueEnum::IntValue(lhs)),
                        Some(BasicValueEnum::IntValue(rhs)),
                        Operator::Ampersand,
                    ) => Ok(Some(BasicValueEnum::IntValue(self.builder.build_and(
                        lhs,
                        rhs,
                        "tmpbitand",
                    )?))),
                    (
                        Some(BasicValueEnum::IntValue(lhs)),
                        Some(BasicValueEnum::IntValue(rhs)),
                        Operator::Pipe,
                    ) => Ok(Some(BasicValueEnum::IntValue(
                        self.builder.build_or(lhs, rhs, "tmpbitor")?,
                    ))),

                    _ => anyhow::bail!("Unsupported binary operation"),
                }
            }
            Expr::Unary { operator, operand } => {
                if matches!(operator, Operator::Ampersand) {
                    let var_name = match operand.as_ref() {
                        Expr::Variable(name) => name,
                        _ => anyhow::bail!("Can only take address of variables"),
                    };
                    let (ptr, _) = self
                        .variables
                        .get(var_name)
                        .ok_or_else(|| anyhow::anyhow!("Undefined variable: {}", var_name))?;
                    return Ok(Some(BasicValueEnum::PointerValue(*ptr)));
                }
                if matches!(operator, Operator::Asterisk) {
                    let operand_val = self.compile_expr(operand)?;
                    let ptr = match operand_val {
                        Some(BasicValueEnum::PointerValue(p)) => p,
                        _ => anyhow::bail!("Can only dereference pointer types"),
                    };
                    let var_name = match operand.as_ref() {
                        Expr::Variable(name) => name,
                        _ => anyhow::bail!("Can only take address of variables"),
                    };
                    let (_, ty) = self
                        .variables
                        .get(var_name)
                        .ok_or_else(|| anyhow::anyhow!("Undefined variable: {}", var_name))?;
                    let loaded_val = self.builder.build_load(*ty, ptr, "tmpload")?;
                    return Ok(Some(loaded_val));
                }
                let operand = self.compile_expr(operand)?;
                match (operand, operator) {
                    (Some(BasicValueEnum::IntValue(val)), Operator::Minus) => Ok(Some(
                        BasicValueEnum::IntValue(self.builder.build_int_neg(val, "tmpneg")?),
                    )),
                    (Some(BasicValueEnum::FloatValue(val)), Operator::Minus) => Ok(Some(
                        BasicValueEnum::FloatValue(self.builder.build_float_neg(val, "tmpneg")?),
                    )),
                    (Some(BasicValueEnum::IntValue(val)), Operator::Exclem) => Ok(Some(
                        BasicValueEnum::IntValue(self.builder.build_not(val, "tmpnot")?),
                    )),
                    _ => anyhow::bail!("Unsupported unary operation"),
                }
            }
            Expr::FunctionCall { name, arguments } => {
                let func = self.module.get_function(name);
                let args = arguments
                    .iter()
                    .map(|x| self.compile_expr(x))
                    .collect::<anyhow::Result<Vec<_>>>()?;
                let func = func.ok_or_else(|| anyhow::anyhow!("Function not found: {}", name))?;
                let args: Vec<_> = args.iter().map(|arg| arg.unwrap().into()).collect();
                let return_val = self.builder.build_call(func, &args, name)?;
                Ok(return_val.try_as_basic_value().basic())
            }
            Expr::Assignment { target, value } => {
                let ptr = match target.as_ref() {
                    Expr::Variable(name) => {
                        let (ptr, _) = self
                            .variables
                            .get(name)
                            .ok_or_else(|| anyhow::anyhow!("Undefined variable: {}", name))?;
                        *ptr
                    }
                    Expr::Unary {
                        operator: Operator::Asterisk,
                        operand,
                    } => {
                        let operand_val = self.compile_expr(operand)?;
                        match operand_val {
                            Some(BasicValueEnum::PointerValue(p)) => p,
                            _ => anyhow::bail!("Can only dereference pointer types"),
                        }
                    }
                    _ => anyhow::bail!("Invalid assignment target"),
                };

                let val = self.compile_expr(value)?.unwrap();
                self.builder.build_store(ptr, val)?;
                Ok(None)
            }
        }
    }

    pub fn generate(&mut self, global_scope: &[Statement]) -> anyhow::Result<Module<'ctx>> {
        self.builder.position_at_end(self.current_block);

        for statement in global_scope {
            self.compile_stmt(&statement.stmt, self.main_function)?;
        }

        self.builder
            .build_return(Some(&self.context.i32_type().const_int(0, false)))?;

        self.module.verify().map_err(|e| anyhow::anyhow!("{e}"))?;

        Ok(self.module.clone())
    }
}
