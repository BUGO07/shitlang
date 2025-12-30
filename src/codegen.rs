use std::{collections::HashMap, path::Path, process::Command};

use inkwell::{
    AddressSpace, FloatPredicate, IntPredicate, OptimizationLevel,
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
    values::{BasicValueEnum, FunctionValue, PointerValue, ValueKind},
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

    variables: HashMap<String, PointerValue<'ctx>>,
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

    fn compile_stmt(&mut self, stmt: &Stmt, function: FunctionValue<'ctx>) -> anyhow::Result<()> {
        match stmt {
            Stmt::Expr(expr) => {
                self.compile_expr(expr, function)?;
                Ok(())
            }
            Stmt::Scope { statements } => {
                let block = self.current_block;
                let basic_block = self.context.append_basic_block(function, "scope");
                self.switch_block(basic_block);
                for statement in statements {
                    self.compile_stmt(&statement.stmt, function)?;
                }
                self.switch_block(block);
                Ok(())
            }
            Stmt::Return { value } => {
                if let Some(expr) = value {
                    let ret_val = self.compile_expr(expr, function)?.unwrap();
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
                        Type::Named(name) if name == "String" => {
                            function_params
                                .push(self.context.ptr_type(AddressSpace::default()).into());
                        }
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
                        _ => todo!(),
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
                            self.context.i64_type().fn_type(&function_params, variadic) // ! TODO
                        }
                        NumericType::F32 => {
                            self.context.f32_type().fn_type(&function_params, variadic)
                        }
                        NumericType::F64 => {
                            self.context.f64_type().fn_type(&function_params, variadic)
                        }
                    },
                    Type::Named(name) if name == "String" => self
                        .context
                        .ptr_type(AddressSpace::default())
                        .fn_type(&function_params, variadic),
                    Type::Variadic => {
                        anyhow::bail!("Function '{}' type can't be variadic", name);
                    }
                    Type::Pointer(_) => self
                        .context
                        .ptr_type(AddressSpace::default())
                        .fn_type(&function_params, variadic),
                    _ => todo!(),
                };

                self.variables.clear();

                let func = self.module.add_function(name, function_type, None);
                let last_block = self.current_block;
                let block = self.context.append_basic_block(func, name);
                self.switch_block(block);

                for (index, param) in params.iter().enumerate() {
                    let ptr = match &param.ty {
                        Type::Boolean => self
                            .builder
                            .build_alloca(self.context.bool_type(), &param.name),
                        Type::Void => continue,
                        Type::Numeric(numeric) => match numeric {
                            NumericType::I8 | NumericType::U8 => self
                                .builder
                                .build_alloca(self.context.i8_type(), &param.name),
                            NumericType::I16 | NumericType::U16 => self
                                .builder
                                .build_alloca(self.context.i16_type(), &param.name),
                            NumericType::I32 | NumericType::U32 => self
                                .builder
                                .build_alloca(self.context.i32_type(), &param.name),
                            NumericType::I64 | NumericType::U64 => self
                                .builder
                                .build_alloca(self.context.i64_type(), &param.name),
                            NumericType::ISize | NumericType::USize => {
                                self.builder
                                    .build_alloca(self.context.i64_type(), &param.name) // ! TODO
                            }
                            NumericType::F32 => self
                                .builder
                                .build_alloca(self.context.f32_type(), &param.name),
                            NumericType::F64 => self
                                .builder
                                .build_alloca(self.context.f64_type(), &param.name),
                        },
                        Type::Named(name) if name == "String" => self.builder.build_alloca(
                            self.context.ptr_type(AddressSpace::default()),
                            &param.name,
                        ),
                        Type::Pointer(_) => self.builder.build_alloca(
                            self.context.ptr_type(AddressSpace::default()),
                            &param.name,
                        ),
                        _ => todo!(),
                    }?;
                    let func_param = func.get_nth_param(index as u32).unwrap();
                    self.builder.build_store(ptr, func_param)?;
                    self.variables.insert(param.name.clone(), ptr);
                }
                match &body.stmt {
                    Stmt::Scope { statements } => {
                        for statement in statements {
                            self.compile_stmt(&statement.stmt, func)?;
                        }
                    }
                    _ => anyhow::bail!("Function body must be a scope"),
                }

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
                        Type::Named(name) if name == "String" => {
                            function_params
                                .push(self.context.ptr_type(AddressSpace::default()).into());
                        }
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
                        _ => todo!(),
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
                            self.context.i64_type().fn_type(&function_params, variadic) // ! TODO
                        }
                        NumericType::F32 => {
                            self.context.f32_type().fn_type(&function_params, variadic)
                        }
                        NumericType::F64 => {
                            self.context.f64_type().fn_type(&function_params, variadic)
                        }
                    },
                    Type::Named(name) if name == "String" => self
                        .context
                        .ptr_type(AddressSpace::default())
                        .fn_type(&function_params, variadic),
                    Type::Variadic => {
                        anyhow::bail!("Function '{}' type can't be variadic", name);
                    }
                    Type::Pointer(_) => self
                        .context
                        .ptr_type(AddressSpace::default())
                        .fn_type(&function_params, variadic),
                    _ => todo!(),
                };

                self.variables.clear();

                self.module.add_function(name, function_type, None);

                Ok(())
            }
            // ! TODO: FIX
            Stmt::Let { name, value, .. } => {
                let init_val = self.compile_expr(value, function)?.unwrap();
                let ptr = self.builder.build_alloca(init_val.get_type(), name)?;
                self.builder.build_store(ptr, init_val)?;
                self.variables.insert(name.clone(), ptr);
                Ok(())
            }
            Stmt::While { .. } => {
                todo!()
            }
        }
    }

    fn compile_expr(
        &mut self,
        expr: &Expr,
        function: FunctionValue<'ctx>,
    ) -> anyhow::Result<Option<BasicValueEnum<'ctx>>> {
        match expr {
            Expr::Literal(lit) => match lit {
                Literal::Numeric(lit) => {
                    let value = lit.split("_").next().unwrap();
                    let Some(num_type) = NumericType::from_literal(lit)? else {
                        return Ok(if value.contains(".") {
                            Some(BasicValueEnum::FloatValue(
                                self.context.f64_type().const_float(value.parse()?),
                            ))
                        } else {
                            Some(BasicValueEnum::IntValue(
                                self.context
                                    .i32_type()
                                    .const_int(value.parse::<u64>()?, false),
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
                        NumericType::I8 | NumericType::U8 => Ok(Some(BasicValueEnum::IntValue(
                            self.context
                                .i8_type()
                                .const_int(value.parse::<u64>()?, false),
                        ))),
                        NumericType::I16 | NumericType::U16 => Ok(Some(BasicValueEnum::IntValue(
                            self.context
                                .i16_type()
                                .const_int(value.parse::<u64>()?, false),
                        ))),
                        NumericType::I32 | NumericType::U32 => Ok(Some(BasicValueEnum::IntValue(
                            self.context
                                .i32_type()
                                .const_int(value.parse::<u64>()?, false),
                        ))),
                        NumericType::I64
                        | NumericType::ISize
                        | NumericType::U64
                        | NumericType::USize => Ok(Some(BasicValueEnum::IntValue(
                            self.context
                                .i64_type()
                                .const_int(value.parse::<u64>()?, false),
                        ))),
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
                        .build_global_string_ptr(&s.replace("\\n", "\n"), "str")?;
                    Ok(Some(BasicValueEnum::PointerValue(
                        str_const.as_pointer_value(),
                    )))
                }
                Literal::Boolean(s) => Ok(Some(BasicValueEnum::IntValue(
                    self.context
                        .bool_type()
                        .const_int(if s == "true" { 1 } else { 0 }, false),
                ))),
            },
            Expr::Variable(name) => self
                .variables
                .get(name)
                .ok_or_else(|| anyhow::anyhow!("Undefined variable: {}", name))
                .and_then(|ptr| {
                    Ok(Some(self.builder.build_load(
                        self.context.i32_type(),
                        *ptr,
                        name,
                    )?))
                }),
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let left_val = self.compile_expr(left, function)?;
                let right_val = self.compile_expr(right, function)?;
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
            Expr::Unary { .. } => {
                todo!()
            }
            Expr::FunctionCall { name, arguments } => {
                let func = self.module.get_function(name);
                let args = arguments
                    .iter()
                    .map(|x| self.compile_expr(x, function))
                    .collect::<anyhow::Result<Vec<_>>>()?;
                let func = func.ok_or_else(|| anyhow::anyhow!("Function not found: {}", name))?;
                let args: Vec<_> = args.iter().map(|arg| arg.unwrap().into()).collect();
                let return_val = self.builder.build_call(func, &args, name)?;
                if let ValueKind::Basic(x) = return_val.try_as_basic_value() {
                    Ok(Some(x))
                } else {
                    Ok(None)
                }
            }
            Expr::Assignment { target, value } => {
                let target = match &**target {
                    Expr::Variable(name) => name,
                    _ => anyhow::bail!("Invalid assignment target"),
                };
                let val = self.compile_expr(value, function)?;
                self.variables
                    .get(target)
                    .ok_or_else(|| anyhow::anyhow!("Undefined variable: {}", target))
                    .and_then(|ptr| Ok(self.builder.build_store(*ptr, val.unwrap())?))?;
                Ok(None)
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_val = self.compile_expr(condition, function)?.unwrap();
                let current_block = self.current_block;
                let then_block = self.context.append_basic_block(self.main_function, "then");
                self.switch_block(then_block);
                for statement in then_branch {
                    self.compile_stmt(&statement.stmt, function)?;
                }
                let else_block = self.context.append_basic_block(self.main_function, "else");
                self.switch_block(else_block);
                if let Some(else_branch) = else_branch {
                    for statement in else_branch {
                        self.compile_stmt(&statement.stmt, function)?;
                    }
                }
                self.switch_block(current_block);
                self.builder.build_conditional_branch(
                    cond_val.into_int_value(),
                    then_block,
                    else_block,
                )?;

                Ok(None) // TODO: make this return a value (like rust)
            }
        }
    }

    pub fn generate(&mut self, global_scope: &[Statement]) -> anyhow::Result<()> {
        self.builder.position_at_end(self.current_block);

        for statement in global_scope {
            self.compile_stmt(&statement.stmt, self.main_function)?;
        }

        self.builder
            .build_return(Some(&self.context.i32_type().const_int(0, false)))?;

        self.module
            .verify()
            .map_err(|e| anyhow::anyhow!("{:?}", e))?;

        self.module.print_to_file("build/output.ll").unwrap();

        Target::initialize_native(&InitializationConfig::default()).unwrap();
        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple).unwrap();
        let cpu = "generic";
        let features = "";

        let target_machine = target
            .create_target_machine(
                &triple,
                cpu,
                features,
                OptimizationLevel::Default,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .unwrap();

        target_machine
            .write_to_file(&self.module, FileType::Object, Path::new("build/output.o"))
            .unwrap();

        Command::new("clang")
            .args(["build/output.o", "-o", "build/output"])
            .status()
            .map_err(|e| anyhow::anyhow!("Failed to execute clang: {}", e))?;

        Command::new("build/output").status()?;

        Ok(())
    }
}
