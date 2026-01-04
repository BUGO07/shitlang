use std::{collections::HashMap, fmt, sync::Arc};

use crate::{
    parser::{Expr, Param, Statement, Stmt, Type},
    token::{Literal, Location, NumericType, Operator},
};

#[derive(Debug, Clone, PartialEq)]
pub enum InterpretValue {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    ISize(isize),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    USize(usize),
    F32(f32),
    F64(f64),
    Boolean(bool),
    String(String),
    Pointer(usize, Type),
    Void,
}

impl fmt::Display for InterpretValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InterpretValue::I8(v) => write!(f, "{}", v),
            InterpretValue::I16(v) => write!(f, "{}", v),
            InterpretValue::I32(v) => write!(f, "{}", v),
            InterpretValue::I64(v) => write!(f, "{}", v),
            InterpretValue::ISize(v) => write!(f, "{}", v),
            InterpretValue::U8(v) => write!(f, "{}", v),
            InterpretValue::U16(v) => write!(f, "{}", v),
            InterpretValue::U32(v) => write!(f, "{}", v),
            InterpretValue::U64(v) => write!(f, "{}", v),
            InterpretValue::USize(v) => write!(f, "{}", v),
            InterpretValue::F32(v) => write!(f, "{}", v),
            InterpretValue::F64(v) => write!(f, "{}", v),
            InterpretValue::Boolean(v) => write!(f, "{}", v),
            InterpretValue::String(v) => write!(f, "{}", v),
            InterpretValue::Pointer(v, t) => write!(f, "Pointer({:?}): {:#x}", t, v),
            InterpretValue::Void => write!(f, "void"),
        }
    }
}

impl InterpretValue {
    pub fn from_literal(lit: Literal) -> anyhow::Result<Self> {
        match lit {
            Literal::Numeric(lit) => {
                // Split at underscore using find instead of split to avoid allocation
                let value = if let Some(pos) = lit.find('_') {
                    &lit[..pos]
                } else {
                    &lit
                };
                let Some(num_type) = NumericType::from_literal(&lit)? else {
                    return Ok(if value.contains('.') {
                        InterpretValue::F64(value.parse()?)
                    } else {
                        InterpretValue::I32(value.parse()?)
                    });
                };
                if value.starts_with('-')
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
                    NumericType::I8 => Ok(InterpretValue::I8(value.parse()?)),
                    NumericType::I16 => Ok(InterpretValue::I16(value.parse()?)),
                    NumericType::I32 => Ok(InterpretValue::I32(value.parse()?)),
                    NumericType::I64 => Ok(InterpretValue::I64(value.parse()?)),
                    NumericType::ISize => Ok(InterpretValue::ISize(value.parse()?)),
                    NumericType::U8 => Ok(InterpretValue::U8(value.parse()?)),
                    NumericType::U16 => Ok(InterpretValue::U16(value.parse()?)),
                    NumericType::U32 => Ok(InterpretValue::U32(value.parse()?)),
                    NumericType::U64 => Ok(InterpretValue::U64(value.parse()?)),
                    NumericType::USize => Ok(InterpretValue::USize(value.parse()?)),
                    NumericType::F32 => Ok(InterpretValue::F32(value.parse()?)),
                    NumericType::F64 => Ok(InterpretValue::F64(value.parse()?)),
                }
            }
            Literal::String(lit) => Ok(InterpretValue::String(lit)),
            Literal::Char(lit) => Ok(InterpretValue::U8(lit as u8)),
            Literal::Boolean(lit) => Ok(InterpretValue::Boolean(lit == "true")),
        }
    }

    pub fn add(&self, other: &InterpretValue) -> anyhow::Result<InterpretValue> {
        match (self, other) {
            (InterpretValue::I8(a), InterpretValue::I8(b)) => Ok(InterpretValue::I8(a + b)),
            (InterpretValue::I16(a), InterpretValue::I16(b)) => Ok(InterpretValue::I16(a + b)),
            (InterpretValue::I32(a), InterpretValue::I32(b)) => Ok(InterpretValue::I32(a + b)),
            (InterpretValue::I64(a), InterpretValue::I64(b)) => Ok(InterpretValue::I64(a + b)),
            (InterpretValue::ISize(a), InterpretValue::ISize(b)) => {
                Ok(InterpretValue::ISize(a + b))
            }
            (InterpretValue::U8(a), InterpretValue::U8(b)) => Ok(InterpretValue::U8(a + b)),
            (InterpretValue::U16(a), InterpretValue::U16(b)) => Ok(InterpretValue::U16(a + b)),
            (InterpretValue::U32(a), InterpretValue::U32(b)) => Ok(InterpretValue::U32(a + b)),
            (InterpretValue::U64(a), InterpretValue::U64(b)) => Ok(InterpretValue::U64(a + b)),
            (InterpretValue::USize(a), InterpretValue::USize(b)) => {
                Ok(InterpretValue::USize(a + b))
            }
            (InterpretValue::F32(a), InterpretValue::F32(b)) => Ok(InterpretValue::F32(a + b)),
            (InterpretValue::F64(a), InterpretValue::F64(b)) => Ok(InterpretValue::F64(a + b)),
            (InterpretValue::String(a), InterpretValue::String(b)) => {
                Ok(InterpretValue::String(format!("{}{}", a, b)))
            }
            x => anyhow::bail!("Addition is not supported for given value types {x:?}"),
        }
    }

    pub fn sub(&self, other: &InterpretValue) -> anyhow::Result<InterpretValue> {
        match (self, other) {
            (InterpretValue::I8(a), InterpretValue::I8(b)) => Ok(InterpretValue::I8(a - b)),
            (InterpretValue::I16(a), InterpretValue::I16(b)) => Ok(InterpretValue::I16(a - b)),
            (InterpretValue::I32(a), InterpretValue::I32(b)) => Ok(InterpretValue::I32(a - b)),
            (InterpretValue::I64(a), InterpretValue::I64(b)) => Ok(InterpretValue::I64(a - b)),
            (InterpretValue::ISize(a), InterpretValue::ISize(b)) => {
                Ok(InterpretValue::ISize(a - b))
            }
            (InterpretValue::U8(a), InterpretValue::U8(b)) => Ok(InterpretValue::U8(a - b)),
            (InterpretValue::U16(a), InterpretValue::U16(b)) => Ok(InterpretValue::U16(a - b)),
            (InterpretValue::U32(a), InterpretValue::U32(b)) => Ok(InterpretValue::U32(a - b)),
            (InterpretValue::U64(a), InterpretValue::U64(b)) => Ok(InterpretValue::U64(a - b)),
            (InterpretValue::USize(a), InterpretValue::USize(b)) => {
                Ok(InterpretValue::USize(a - b))
            }
            (InterpretValue::F32(a), InterpretValue::F32(b)) => Ok(InterpretValue::F32(a - b)),
            (InterpretValue::F64(a), InterpretValue::F64(b)) => Ok(InterpretValue::F64(a - b)),
            x => anyhow::bail!("Subtraction is not supported for given value types {x:?}"),
        }
    }

    pub fn mul(&self, other: &InterpretValue) -> anyhow::Result<InterpretValue> {
        match (self, other) {
            (InterpretValue::I8(a), InterpretValue::I8(b)) => Ok(InterpretValue::I8(a * b)),
            (InterpretValue::I16(a), InterpretValue::I16(b)) => Ok(InterpretValue::I16(a * b)),
            (InterpretValue::I32(a), InterpretValue::I32(b)) => Ok(InterpretValue::I32(a * b)),
            (InterpretValue::I64(a), InterpretValue::I64(b)) => Ok(InterpretValue::I64(a * b)),
            (InterpretValue::ISize(a), InterpretValue::ISize(b)) => {
                Ok(InterpretValue::ISize(a * b))
            }
            (InterpretValue::U8(a), InterpretValue::U8(b)) => Ok(InterpretValue::U8(a * b)),
            (InterpretValue::U16(a), InterpretValue::U16(b)) => Ok(InterpretValue::U16(a * b)),
            (InterpretValue::U32(a), InterpretValue::U32(b)) => Ok(InterpretValue::U32(a * b)),
            (InterpretValue::U64(a), InterpretValue::U64(b)) => Ok(InterpretValue::U64(a * b)),
            (InterpretValue::USize(a), InterpretValue::USize(b)) => {
                Ok(InterpretValue::USize(a * b))
            }
            (InterpretValue::F32(a), InterpretValue::F32(b)) => Ok(InterpretValue::F32(a * b)),
            (InterpretValue::F64(a), InterpretValue::F64(b)) => Ok(InterpretValue::F64(a * b)),
            x => anyhow::bail!("Multiplication is not supported for given value types {x:?}"),
        }
    }

    pub fn div(&self, other: &InterpretValue) -> anyhow::Result<InterpretValue> {
        match (self, other) {
            (InterpretValue::I8(a), InterpretValue::I8(b)) => Ok(InterpretValue::I8(a / b)),
            (InterpretValue::I16(a), InterpretValue::I16(b)) => Ok(InterpretValue::I16(a / b)),
            (InterpretValue::I32(a), InterpretValue::I32(b)) => Ok(InterpretValue::I32(a / b)),
            (InterpretValue::I64(a), InterpretValue::I64(b)) => Ok(InterpretValue::I64(a / b)),
            (InterpretValue::ISize(a), InterpretValue::ISize(b)) => {
                Ok(InterpretValue::ISize(a / b))
            }
            (InterpretValue::U8(a), InterpretValue::U8(b)) => Ok(InterpretValue::U8(a / b)),
            (InterpretValue::U16(a), InterpretValue::U16(b)) => Ok(InterpretValue::U16(a / b)),
            (InterpretValue::U32(a), InterpretValue::U32(b)) => Ok(InterpretValue::U32(a / b)),
            (InterpretValue::U64(a), InterpretValue::U64(b)) => Ok(InterpretValue::U64(a / b)),
            (InterpretValue::USize(a), InterpretValue::USize(b)) => {
                Ok(InterpretValue::USize(a / b))
            }
            (InterpretValue::F32(a), InterpretValue::F32(b)) => Ok(InterpretValue::F32(a / b)),
            (InterpretValue::F64(a), InterpretValue::F64(b)) => Ok(InterpretValue::F64(a / b)),
            x => anyhow::bail!("Division is not supported for given value types {x:?}"),
        }
    }

    pub fn rem(&self, other: &InterpretValue) -> anyhow::Result<InterpretValue> {
        match (self, other) {
            (InterpretValue::I8(a), InterpretValue::I8(b)) => Ok(InterpretValue::I8(a % b)),
            (InterpretValue::I16(a), InterpretValue::I16(b)) => Ok(InterpretValue::I16(a % b)),
            (InterpretValue::I32(a), InterpretValue::I32(b)) => Ok(InterpretValue::I32(a % b)),
            (InterpretValue::I64(a), InterpretValue::I64(b)) => Ok(InterpretValue::I64(a % b)),
            (InterpretValue::ISize(a), InterpretValue::ISize(b)) => {
                Ok(InterpretValue::ISize(a % b))
            }
            (InterpretValue::U8(a), InterpretValue::U8(b)) => Ok(InterpretValue::U8(a % b)),
            (InterpretValue::U16(a), InterpretValue::U16(b)) => Ok(InterpretValue::U16(a % b)),
            (InterpretValue::U32(a), InterpretValue::U32(b)) => Ok(InterpretValue::U32(a % b)),
            (InterpretValue::U64(a), InterpretValue::U64(b)) => Ok(InterpretValue::U64(a % b)),
            (InterpretValue::USize(a), InterpretValue::USize(b)) => {
                Ok(InterpretValue::USize(a % b))
            }
            x => anyhow::bail!("Modulus is not supported for given value types {x:?}"),
        }
    }

    pub fn eq(&self, other: &InterpretValue) -> anyhow::Result<InterpretValue> {
        match (self, other) {
            (InterpretValue::I8(a), InterpretValue::I8(b)) => Ok(InterpretValue::Boolean(a == b)),
            (InterpretValue::I16(a), InterpretValue::I16(b)) => Ok(InterpretValue::Boolean(a == b)),
            (InterpretValue::I32(a), InterpretValue::I32(b)) => Ok(InterpretValue::Boolean(a == b)),
            (InterpretValue::I64(a), InterpretValue::I64(b)) => Ok(InterpretValue::Boolean(a == b)),
            (InterpretValue::ISize(a), InterpretValue::ISize(b)) => {
                Ok(InterpretValue::Boolean(a == b))
            }
            (InterpretValue::U8(a), InterpretValue::U8(b)) => Ok(InterpretValue::Boolean(a == b)),
            (InterpretValue::U16(a), InterpretValue::U16(b)) => Ok(InterpretValue::Boolean(a == b)),
            (InterpretValue::U32(a), InterpretValue::U32(b)) => Ok(InterpretValue::Boolean(a == b)),
            (InterpretValue::U64(a), InterpretValue::U64(b)) => Ok(InterpretValue::Boolean(a == b)),
            (InterpretValue::USize(a), InterpretValue::USize(b)) => {
                Ok(InterpretValue::Boolean(a == b))
            }
            (InterpretValue::F32(a), InterpretValue::F32(b)) => Ok(InterpretValue::Boolean(a == b)),
            (InterpretValue::F64(a), InterpretValue::F64(b)) => Ok(InterpretValue::Boolean(a == b)),
            (InterpretValue::Boolean(a), InterpretValue::Boolean(b)) => {
                Ok(InterpretValue::Boolean(a == b))
            }
            (InterpretValue::String(a), InterpretValue::String(b)) => {
                Ok(InterpretValue::Boolean(a == b))
            }
            x => anyhow::bail!("Equality comparison is not supported for given value types {x:?}"),
        }
    }

    pub fn neq(&self, other: &InterpretValue) -> anyhow::Result<InterpretValue> {
        match (self, other) {
            (InterpretValue::I8(a), InterpretValue::I8(b)) => Ok(InterpretValue::Boolean(a != b)),
            (InterpretValue::I16(a), InterpretValue::I16(b)) => Ok(InterpretValue::Boolean(a != b)),
            (InterpretValue::I32(a), InterpretValue::I32(b)) => Ok(InterpretValue::Boolean(a != b)),
            (InterpretValue::I64(a), InterpretValue::I64(b)) => Ok(InterpretValue::Boolean(a != b)),
            (InterpretValue::ISize(a), InterpretValue::ISize(b)) => {
                Ok(InterpretValue::Boolean(a != b))
            }
            (InterpretValue::U8(a), InterpretValue::U8(b)) => Ok(InterpretValue::Boolean(a != b)),
            (InterpretValue::U16(a), InterpretValue::U16(b)) => Ok(InterpretValue::Boolean(a != b)),
            (InterpretValue::U32(a), InterpretValue::U32(b)) => Ok(InterpretValue::Boolean(a != b)),
            (InterpretValue::U64(a), InterpretValue::U64(b)) => Ok(InterpretValue::Boolean(a != b)),
            (InterpretValue::USize(a), InterpretValue::USize(b)) => {
                Ok(InterpretValue::Boolean(a != b))
            }
            (InterpretValue::F32(a), InterpretValue::F32(b)) => Ok(InterpretValue::Boolean(a != b)),
            (InterpretValue::F64(a), InterpretValue::F64(b)) => Ok(InterpretValue::Boolean(a != b)),
            (InterpretValue::Boolean(a), InterpretValue::Boolean(b)) => {
                Ok(InterpretValue::Boolean(a != b))
            }
            (InterpretValue::String(a), InterpretValue::String(b)) => {
                Ok(InterpretValue::Boolean(a != b))
            }
            x => {
                anyhow::bail!("Inequality comparison is not supported for given value types {x:?}")
            }
        }
    }

    pub fn gt(&self, other: &InterpretValue) -> anyhow::Result<InterpretValue> {
        match (self, other) {
            (InterpretValue::I8(a), InterpretValue::I8(b)) => Ok(InterpretValue::Boolean(a > b)),
            (InterpretValue::I16(a), InterpretValue::I16(b)) => Ok(InterpretValue::Boolean(a > b)),
            (InterpretValue::I32(a), InterpretValue::I32(b)) => Ok(InterpretValue::Boolean(a > b)),
            (InterpretValue::I64(a), InterpretValue::I64(b)) => Ok(InterpretValue::Boolean(a > b)),
            (InterpretValue::ISize(a), InterpretValue::ISize(b)) => {
                Ok(InterpretValue::Boolean(a > b))
            }
            (InterpretValue::U8(a), InterpretValue::U8(b)) => Ok(InterpretValue::Boolean(a > b)),
            (InterpretValue::U16(a), InterpretValue::U16(b)) => Ok(InterpretValue::Boolean(a > b)),
            (InterpretValue::U32(a), InterpretValue::U32(b)) => Ok(InterpretValue::Boolean(a > b)),
            (InterpretValue::U64(a), InterpretValue::U64(b)) => Ok(InterpretValue::Boolean(a > b)),
            (InterpretValue::USize(a), InterpretValue::USize(b)) => {
                Ok(InterpretValue::Boolean(a > b))
            }
            (InterpretValue::F32(a), InterpretValue::F32(b)) => Ok(InterpretValue::Boolean(a > b)),
            (InterpretValue::F64(a), InterpretValue::F64(b)) => Ok(InterpretValue::Boolean(a > b)),
            x => anyhow::bail!(
                "Greater-than comparison is not supported for given value types {x:?}"
            ),
        }
    }

    pub fn lt(&self, other: &InterpretValue) -> anyhow::Result<InterpretValue> {
        match (self, other) {
            (InterpretValue::I8(a), InterpretValue::I8(b)) => Ok(InterpretValue::Boolean(a < b)),
            (InterpretValue::I16(a), InterpretValue::I16(b)) => Ok(InterpretValue::Boolean(a < b)),
            (InterpretValue::I32(a), InterpretValue::I32(b)) => Ok(InterpretValue::Boolean(a < b)),
            (InterpretValue::I64(a), InterpretValue::I64(b)) => Ok(InterpretValue::Boolean(a < b)),
            (InterpretValue::ISize(a), InterpretValue::ISize(b)) => {
                Ok(InterpretValue::Boolean(a < b))
            }
            (InterpretValue::U8(a), InterpretValue::U8(b)) => Ok(InterpretValue::Boolean(a < b)),
            (InterpretValue::U16(a), InterpretValue::U16(b)) => Ok(InterpretValue::Boolean(a < b)),
            (InterpretValue::U32(a), InterpretValue::U32(b)) => Ok(InterpretValue::Boolean(a < b)),
            (InterpretValue::U64(a), InterpretValue::U64(b)) => Ok(InterpretValue::Boolean(a < b)),
            (InterpretValue::USize(a), InterpretValue::USize(b)) => {
                Ok(InterpretValue::Boolean(a < b))
            }
            (InterpretValue::F32(a), InterpretValue::F32(b)) => Ok(InterpretValue::Boolean(a < b)),
            (InterpretValue::F64(a), InterpretValue::F64(b)) => Ok(InterpretValue::Boolean(a < b)),
            x => anyhow::bail!("Less-than comparison is not supported for given value types {x:?}"),
        }
    }

    pub fn gte(&self, other: &InterpretValue) -> anyhow::Result<InterpretValue> {
        match (self, other) {
            (InterpretValue::I8(a), InterpretValue::I8(b)) => Ok(InterpretValue::Boolean(a >= b)),
            (InterpretValue::I16(a), InterpretValue::I16(b)) => Ok(InterpretValue::Boolean(a >= b)),
            (InterpretValue::I32(a), InterpretValue::I32(b)) => Ok(InterpretValue::Boolean(a >= b)),
            (InterpretValue::I64(a), InterpretValue::I64(b)) => Ok(InterpretValue::Boolean(a >= b)),
            (InterpretValue::ISize(a), InterpretValue::ISize(b)) => {
                Ok(InterpretValue::Boolean(a >= b))
            }
            (InterpretValue::U8(a), InterpretValue::U8(b)) => Ok(InterpretValue::Boolean(a >= b)),
            (InterpretValue::U16(a), InterpretValue::U16(b)) => Ok(InterpretValue::Boolean(a >= b)),
            (InterpretValue::U32(a), InterpretValue::U32(b)) => Ok(InterpretValue::Boolean(a >= b)),
            (InterpretValue::U64(a), InterpretValue::U64(b)) => Ok(InterpretValue::Boolean(a >= b)),
            (InterpretValue::USize(a), InterpretValue::USize(b)) => {
                Ok(InterpretValue::Boolean(a >= b))
            }
            (InterpretValue::F32(a), InterpretValue::F32(b)) => Ok(InterpretValue::Boolean(a >= b)),
            (InterpretValue::F64(a), InterpretValue::F64(b)) => Ok(InterpretValue::Boolean(a >= b)),
            x => anyhow::bail!(
                "Greater-than-or-equal comparison is not supported for given value types {x:?}"
            ),
        }
    }

    pub fn lte(&self, other: &InterpretValue) -> anyhow::Result<InterpretValue> {
        match (self, other) {
            (InterpretValue::I8(a), InterpretValue::I8(b)) => Ok(InterpretValue::Boolean(a <= b)),
            (InterpretValue::I16(a), InterpretValue::I16(b)) => Ok(InterpretValue::Boolean(a <= b)),
            (InterpretValue::I32(a), InterpretValue::I32(b)) => Ok(InterpretValue::Boolean(a <= b)),
            (InterpretValue::I64(a), InterpretValue::I64(b)) => Ok(InterpretValue::Boolean(a <= b)),
            (InterpretValue::ISize(a), InterpretValue::ISize(b)) => {
                Ok(InterpretValue::Boolean(a <= b))
            }
            (InterpretValue::U8(a), InterpretValue::U8(b)) => Ok(InterpretValue::Boolean(a <= b)),
            (InterpretValue::U16(a), InterpretValue::U16(b)) => Ok(InterpretValue::Boolean(a <= b)),
            (InterpretValue::U32(a), InterpretValue::U32(b)) => Ok(InterpretValue::Boolean(a <= b)),
            (InterpretValue::U64(a), InterpretValue::U64(b)) => Ok(InterpretValue::Boolean(a <= b)),
            (InterpretValue::USize(a), InterpretValue::USize(b)) => {
                Ok(InterpretValue::Boolean(a <= b))
            }
            (InterpretValue::F32(a), InterpretValue::F32(b)) => Ok(InterpretValue::Boolean(a <= b)),
            (InterpretValue::F64(a), InterpretValue::F64(b)) => Ok(InterpretValue::Boolean(a <= b)),
            _ => anyhow::bail!(
                "Less-than-or-equal comparison is not supported for given value types"
            ),
        }
    }

    pub fn and(&self, other: &InterpretValue) -> anyhow::Result<InterpretValue> {
        match (self, other) {
            (InterpretValue::Boolean(a), InterpretValue::Boolean(b)) => {
                Ok(InterpretValue::Boolean(*a && *b))
            }
            _ => anyhow::bail!("Logical AND is not supported for given value types"),
        }
    }

    pub fn or(&self, other: &InterpretValue) -> anyhow::Result<InterpretValue> {
        match (self, other) {
            (InterpretValue::Boolean(a), InterpretValue::Boolean(b)) => {
                Ok(InterpretValue::Boolean(*a || *b))
            }
            _ => anyhow::bail!("Logical OR is not supported for given value types"),
        }
    }

    pub fn not(&self) -> anyhow::Result<InterpretValue> {
        match self {
            InterpretValue::I8(a) => Ok(InterpretValue::I8(!a)),
            InterpretValue::I16(a) => Ok(InterpretValue::I16(!a)),
            InterpretValue::I32(a) => Ok(InterpretValue::I32(!a)),
            InterpretValue::I64(a) => Ok(InterpretValue::I64(!a)),
            InterpretValue::ISize(a) => Ok(InterpretValue::ISize(!a)),
            InterpretValue::U8(a) => Ok(InterpretValue::U8(!a)),
            InterpretValue::U16(a) => Ok(InterpretValue::U16(!a)),
            InterpretValue::U32(a) => Ok(InterpretValue::U32(!a)),
            InterpretValue::U64(a) => Ok(InterpretValue::U64(!a)),
            InterpretValue::USize(a) => Ok(InterpretValue::USize(!a)),
            InterpretValue::Boolean(a) => Ok(InterpretValue::Boolean(!a)),
            _ => anyhow::bail!("NOT is not supported for given value type"),
        }
    }

    pub fn bitand(&self, other: &InterpretValue) -> anyhow::Result<InterpretValue> {
        match (self, other) {
            (InterpretValue::I8(a), InterpretValue::I8(b)) => Ok(InterpretValue::I8(a & b)),
            (InterpretValue::I16(a), InterpretValue::I16(b)) => Ok(InterpretValue::I16(a & b)),
            (InterpretValue::I32(a), InterpretValue::I32(b)) => Ok(InterpretValue::I32(a & b)),
            (InterpretValue::I64(a), InterpretValue::I64(b)) => Ok(InterpretValue::I64(a & b)),
            (InterpretValue::ISize(a), InterpretValue::ISize(b)) => {
                Ok(InterpretValue::ISize(a & b))
            }
            (InterpretValue::U8(a), InterpretValue::U8(b)) => Ok(InterpretValue::U8(a & b)),
            (InterpretValue::U16(a), InterpretValue::U16(b)) => Ok(InterpretValue::U16(a & b)),
            (InterpretValue::U32(a), InterpretValue::U32(b)) => Ok(InterpretValue::U32(a & b)),
            (InterpretValue::U64(a), InterpretValue::U64(b)) => Ok(InterpretValue::U64(a & b)),
            (InterpretValue::USize(a), InterpretValue::USize(b)) => {
                Ok(InterpretValue::USize(a & b))
            }
            _ => anyhow::bail!("Bitwise AND is not supported for given value types"),
        }
    }

    pub fn bitor(&self, other: &InterpretValue) -> anyhow::Result<InterpretValue> {
        match (self, other) {
            (InterpretValue::I8(a), InterpretValue::I8(b)) => Ok(InterpretValue::I8(a | b)),
            (InterpretValue::I16(a), InterpretValue::I16(b)) => Ok(InterpretValue::I16(a | b)),
            (InterpretValue::I32(a), InterpretValue::I32(b)) => Ok(InterpretValue::I32(a | b)),
            (InterpretValue::I64(a), InterpretValue::I64(b)) => Ok(InterpretValue::I64(a | b)),
            (InterpretValue::ISize(a), InterpretValue::ISize(b)) => {
                Ok(InterpretValue::ISize(a | b))
            }
            (InterpretValue::U8(a), InterpretValue::U8(b)) => Ok(InterpretValue::U8(a | b)),
            (InterpretValue::U16(a), InterpretValue::U16(b)) => Ok(InterpretValue::U16(a | b)),
            (InterpretValue::U32(a), InterpretValue::U32(b)) => Ok(InterpretValue::U32(a | b)),
            (InterpretValue::U64(a), InterpretValue::U64(b)) => Ok(InterpretValue::U64(a | b)),
            (InterpretValue::USize(a), InterpretValue::USize(b)) => {
                Ok(InterpretValue::USize(a | b))
            }
            _ => anyhow::bail!("Bitwise OR is not supported for given value types"),
        }
    }

    pub fn neg(&self) -> anyhow::Result<InterpretValue> {
        match self {
            InterpretValue::I8(a) => Ok(InterpretValue::I8(-a)),
            InterpretValue::I16(a) => Ok(InterpretValue::I16(-a)),
            InterpretValue::I32(a) => Ok(InterpretValue::I32(-a)),
            InterpretValue::I64(a) => Ok(InterpretValue::I64(-a)),
            InterpretValue::ISize(a) => Ok(InterpretValue::ISize(-a)),
            InterpretValue::F32(a) => Ok(InterpretValue::F32(-a)),
            InterpretValue::F64(a) => Ok(InterpretValue::F64(-a)),
            _ => anyhow::bail!("Negation is not supported for given value type"),
        }
    }

    pub fn _ref(&self) -> anyhow::Result<InterpretValue> {
        match self {
            InterpretValue::I8(a) => Ok(InterpretValue::Pointer(
                &raw const a as usize,
                Type::Numeric(NumericType::I8),
            )),
            InterpretValue::I16(a) => Ok(InterpretValue::Pointer(
                &raw const a as usize,
                Type::Numeric(NumericType::I16),
            )),
            InterpretValue::I32(a) => Ok(InterpretValue::Pointer(
                &raw const a as usize,
                Type::Numeric(NumericType::I32),
            )),
            InterpretValue::I64(a) => Ok(InterpretValue::Pointer(
                &raw const a as usize,
                Type::Numeric(NumericType::I64),
            )),
            InterpretValue::ISize(a) => Ok(InterpretValue::Pointer(
                &raw const a as usize,
                Type::Numeric(NumericType::ISize),
            )),
            InterpretValue::U8(a) => Ok(InterpretValue::Pointer(
                &raw const a as usize,
                Type::Numeric(NumericType::U8),
            )),
            InterpretValue::U16(a) => Ok(InterpretValue::Pointer(
                &raw const a as usize,
                Type::Numeric(NumericType::U16),
            )),
            InterpretValue::U32(a) => Ok(InterpretValue::Pointer(
                &raw const a as usize,
                Type::Numeric(NumericType::U32),
            )),
            InterpretValue::U64(a) => Ok(InterpretValue::Pointer(
                &raw const a as usize,
                Type::Numeric(NumericType::U64),
            )),
            InterpretValue::USize(a) => Ok(InterpretValue::Pointer(
                &raw const a as usize,
                Type::Numeric(NumericType::USize),
            )),
            InterpretValue::F32(a) => Ok(InterpretValue::Pointer(
                &raw const a as usize,
                Type::Numeric(NumericType::F32),
            )),
            InterpretValue::F64(a) => Ok(InterpretValue::Pointer(
                &raw const a as usize,
                Type::Numeric(NumericType::F64),
            )),
            InterpretValue::Boolean(a) => Ok(InterpretValue::Pointer(
                &raw const a as usize,
                Type::Boolean,
            )),
            InterpretValue::String(a) => Ok(InterpretValue::Pointer(
                &raw const a as usize,
                Type::Named("String".to_string()),
            )),
            _ => anyhow::bail!("Reference is not supported for given value type"),
        }
    }

    pub fn _deref(&self) -> anyhow::Result<InterpretValue> {
        unsafe {
            match self {
                InterpretValue::Pointer(ptr, ty) => match ty {
                    Type::Boolean => Ok(InterpretValue::Boolean(*(*ptr as *mut bool))),
                    Type::Numeric(n) => match n {
                        NumericType::I8 => Ok(InterpretValue::I8(*(*ptr as *mut i8))),
                        NumericType::I16 => Ok(InterpretValue::I16(*(*ptr as *mut i16))),
                        NumericType::I32 => Ok(InterpretValue::I32(*(*ptr as *mut i32))),
                        NumericType::I64 => Ok(InterpretValue::I64(*(*ptr as *mut i64))),
                        NumericType::ISize => Ok(InterpretValue::ISize(*(*ptr as *mut isize))),
                        NumericType::U8 => Ok(InterpretValue::U8(*(*ptr as *mut u8))),
                        NumericType::U16 => Ok(InterpretValue::U16(*(*ptr as *mut u16))),
                        NumericType::U32 => Ok(InterpretValue::U32(*(*ptr as *mut u32))),
                        NumericType::U64 => Ok(InterpretValue::U64(*(*ptr as *mut u64))),
                        NumericType::USize => Ok(InterpretValue::USize(*(*ptr as *mut usize))),
                        NumericType::F32 => Ok(InterpretValue::F32(*(*ptr as *mut f32))),
                        NumericType::F64 => Ok(InterpretValue::F64(*(*ptr as *mut f64))),
                    },
                    Type::Named(n) => match n.as_str() {
                        "String" => Ok(InterpretValue::String((*(*ptr as *mut String)).clone())),
                        _ => anyhow::bail!("Dereference is not supported for given value type"),
                    },
                    _ => anyhow::bail!("Dereference is not supported for given value type"),
                },
                _ => anyhow::bail!("Dereference is not supported for given value type"),
            }
        }
    }

    pub fn as_string(&self) -> String {
        self.to_string()
    }

    pub fn as_integer(&self) -> isize {
        match self {
            InterpretValue::I8(v) => *v as isize,
            InterpretValue::I16(v) => *v as isize,
            InterpretValue::I32(v) => *v as isize,
            InterpretValue::I64(v) => *v as isize,
            InterpretValue::ISize(v) => *v,
            InterpretValue::U8(v) => *v as isize,
            InterpretValue::U16(v) => *v as isize,
            InterpretValue::U32(v) => *v as isize,
            InterpretValue::U64(v) => *v as isize,
            InterpretValue::USize(v) => *v as isize,
            _ => 0,
        }
    }
}

#[derive(Clone)]
pub enum Function {
    Interpreted {
        params: Vec<Param>,
        body: Box<Statement>,
    },
    Native {
        func: Arc<dyn NativeFunction>,
    },
}

pub trait NativeFunction: Send + Sync + 'static {
    fn call(&self, args: Vec<InterpretValue>) -> anyhow::Result<InterpretValue>;
}

impl<F> NativeFunction for F
where
    F: Fn(Vec<InterpretValue>) -> anyhow::Result<InterpretValue> + Send + Sync + 'static,
{
    fn call(&self, args: Vec<InterpretValue>) -> anyhow::Result<InterpretValue> {
        (self)(args)
    }
}

pub struct Environment {
    scopes: Vec<HashMap<String, InterpretValue>>,
    functions: HashMap<String, Function>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            scopes: vec![HashMap::new()],
            functions: HashMap::new(),
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn set(&mut self, name: String, value: InterpretValue) -> anyhow::Result<()> {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, value);
            Ok(())
        } else {
            anyhow::bail!("No scope available to set variable");
        }
    }

    fn get(&self, name: &str) -> anyhow::Result<InterpretValue> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Ok(value.clone());
            }
        }
        anyhow::bail!("Variable '{}' not found", name);
    }

    fn get_mut(&mut self, name: &str) -> anyhow::Result<&mut InterpretValue> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(value) = scope.get_mut(name) {
                return Ok(value);
            }
        }
        anyhow::bail!("Variable '{}' not found", name);
    }

    fn update(&mut self, name: String, value: InterpretValue) -> anyhow::Result<()> {
        *self.get_mut(&name)? = value;
        Ok(())
    }

    fn define_function(
        &mut self,
        name: String,
        params: Vec<Param>,
        body: Box<Statement>,
    ) -> anyhow::Result<()> {
        if self.functions.contains_key(&name) {
            anyhow::bail!("Function '{}' is already defined", name);
        }
        self.functions
            .insert(name, Function::Interpreted { params, body });
        Ok(())
    }

    fn define_rust_function<F>(&mut self, name: String, func: F) -> anyhow::Result<()>
    where
        F: Fn(Vec<InterpretValue>) -> anyhow::Result<InterpretValue> + Send + Sync + 'static,
    {
        if self.functions.contains_key(&name) {
            anyhow::bail!("Function '{}' already defined", name);
        }

        self.functions.insert(
            name,
            Function::Native {
                func: Arc::new(func),
            },
        );

        Ok(())
    }

    fn get_function(&self, name: &str) -> anyhow::Result<&Function> {
        if let Some(function) = self.functions.get(name) {
            Ok(function)
        } else {
            anyhow::bail!("Function '{}' not found", name);
        }
    }
}

#[derive(Debug, Clone)]
pub enum ControlFlow {
    None,
    Return(InterpretValue),
    Break,
    Continue,
}

pub struct Interpreter {
    env: Environment,
}

macro_rules! native_func {
    ($env:expr, $name:ident) => {
        $env.define_rust_function(
            stringify!($name).to_string(),
            crate::native_functions::$name,
        )
        .unwrap();
    };
}

impl Interpreter {
    pub fn new() -> Self {
        let mut env = Environment::new();
        native_func!(env, print);
        native_func!(env, exit);
        Interpreter { env }
    }

    fn exec_stmt(&mut self, statement: &Statement) -> anyhow::Result<ControlFlow> {
        match &statement.stmt {
            Stmt::Let { name, value, ty } => {
                let (val, _) = self.eval_expr(value)?;
                if let Some(ty) = ty {
                    let inferred_ty = match &val {
                        InterpretValue::I8(_) => Type::Numeric(NumericType::I8),
                        InterpretValue::I16(_) => Type::Numeric(NumericType::I16),
                        InterpretValue::I32(_) => Type::Numeric(NumericType::I32),
                        InterpretValue::I64(_) => Type::Numeric(NumericType::I64),
                        InterpretValue::ISize(_) => Type::Numeric(NumericType::ISize),
                        InterpretValue::U8(_) => Type::Numeric(NumericType::U8),
                        InterpretValue::U16(_) => Type::Numeric(NumericType::U16),
                        InterpretValue::U32(_) => Type::Numeric(NumericType::U32),
                        InterpretValue::U64(_) => Type::Numeric(NumericType::U64),
                        InterpretValue::USize(_) => Type::Numeric(NumericType::USize),
                        InterpretValue::F32(_) => Type::Numeric(NumericType::F32),
                        InterpretValue::F64(_) => Type::Numeric(NumericType::F64),
                        InterpretValue::Boolean(_) => Type::Boolean,
                        InterpretValue::String(_) => Type::Named("String".to_string()),
                        InterpretValue::Pointer(_, t) => Type::Pointer(Box::new(t.clone())),
                        InterpretValue::Void => Type::Void,
                    };
                    anyhow::ensure!(
                        &inferred_ty == ty,
                        "Type mismatch: expected {ty:?}, got {inferred_ty:?}",
                    );
                }
                self.env.set(name.clone(), val)?;
                Ok(ControlFlow::None)
            }
            Stmt::Func {
                name, params, body, ..
            } => {
                self.env
                    .define_function(name.clone(), params.clone(), body.clone())?;
                Ok(ControlFlow::None)
            }
            Stmt::Extern { name, params, .. } => {
                self.env.define_function(
                    name.clone(),
                    params.clone(),
                    Box::new(Statement {
                        stmt: Stmt::Scope { statements: vec![] },
                        location: Location::default(),
                    }),
                )?;
                Ok(ControlFlow::None)
            }

            Stmt::Expr(expr) => {
                let (_, flow) = self.eval_expr(expr)?;
                Ok(flow)
            }
            Stmt::Scope { statements } => {
                self.env.push_scope();
                let mut result = ControlFlow::None;
                for stmt in statements {
                    result = self.exec_stmt(stmt)?;
                    match result {
                        ControlFlow::Return(_) | ControlFlow::Break | ControlFlow::Continue => {
                            break;
                        }
                        ControlFlow::None => {}
                    }
                }
                self.env.pop_scope();
                Ok(result)
            }
            Stmt::While { condition, body } => {
                loop {
                    let (cond, _) = self.eval_expr(condition)?;
                    if matches!(cond, InterpretValue::Boolean(false)) {
                        break;
                    }
                    match self.exec_stmt(body)? {
                        ControlFlow::Break => break,
                        ControlFlow::Continue => continue,
                        ControlFlow::Return(val) => return Ok(ControlFlow::Return(val)),
                        ControlFlow::None => {}
                    }
                }
                Ok(ControlFlow::None)
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let (cond, _) = self.eval_expr(condition)?;
                if matches!(cond, InterpretValue::Boolean(true)) {
                    self.env.push_scope();
                    let mut result = ControlFlow::None;
                    for stmt in then_branch {
                        result = self.exec_stmt(stmt)?;
                        match result {
                            ControlFlow::Return(_) | ControlFlow::Break | ControlFlow::Continue => {
                                break;
                            }
                            ControlFlow::None => {}
                        }
                    }
                    self.env.pop_scope();
                    Ok(result)
                } else if let Some(else_branch) = else_branch {
                    self.env.push_scope();
                    let mut result = ControlFlow::None;
                    for stmt in else_branch {
                        result = self.exec_stmt(stmt)?;
                        match result {
                            ControlFlow::Return(_) | ControlFlow::Break | ControlFlow::Continue => {
                                break;
                            }
                            ControlFlow::None => {}
                        }
                    }
                    self.env.pop_scope();
                    Ok(result)
                } else {
                    Ok(ControlFlow::None)
                }
            }
            Stmt::Return { value } => {
                let val = if let Some(expr) = value {
                    let (v, _) = self.eval_expr(expr)?;
                    v
                } else {
                    InterpretValue::Void
                };
                Ok(ControlFlow::Return(val))
            }
            Stmt::Break => Ok(ControlFlow::Break),
            Stmt::Continue => Ok(ControlFlow::Continue),
            Stmt::Semicolon => Ok(ControlFlow::None),
        }
    }

    fn eval_expr(&mut self, expr: &Expr) -> anyhow::Result<(InterpretValue, ControlFlow)> {
        match expr {
            Expr::Literal(lit) => Ok((
                InterpretValue::from_literal(lit.clone())?, // ! TODO: FIX ME
                ControlFlow::None,
            )),
            Expr::Variable(name) => Ok((self.env.get(name)?, ControlFlow::None)),
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let (left_val, _) = self.eval_expr(left)?;
                let (right_val, _) = self.eval_expr(right)?;
                let result = match operator {
                    Operator::Plus => left_val.add(&right_val),
                    Operator::Minus => left_val.sub(&right_val),
                    Operator::Asterisk => left_val.mul(&right_val),
                    Operator::Slash => left_val.div(&right_val),
                    Operator::Percent => left_val.rem(&right_val),
                    Operator::Equals => left_val.eq(&right_val),
                    Operator::NotEquals => left_val.neq(&right_val),
                    Operator::Greater => left_val.gt(&right_val),
                    Operator::Less => left_val.lt(&right_val),
                    Operator::GreaterEquals => left_val.gte(&right_val),
                    Operator::LessEquals => left_val.lte(&right_val),
                    Operator::LogicalAnd => left_val.and(&right_val),
                    Operator::LogicalOr => left_val.or(&right_val),
                    Operator::Ampersand => left_val.bitand(&right_val),
                    Operator::Pipe => left_val.bitor(&right_val),
                    _ => anyhow::bail!("Unknown binary operator '{:?}'", operator),
                }?;
                Ok((result, ControlFlow::None))
            }
            Expr::Unary { operator, operand } => {
                let (operand_val, _) = self.eval_expr(operand)?;
                let result = match operator {
                    Operator::Minus => operand_val.neg(),
                    Operator::Exclem => operand_val.not(),
                    Operator::Asterisk => operand_val._deref(),
                    Operator::Ampersand => operand_val._ref(),
                    _ => anyhow::bail!("Unknown unary operator '{:?}'", operator),
                }?;
                Ok((result, ControlFlow::None))
            }
            Expr::Assignment { target, value } => {
                let (val, _) = self.eval_expr(value)?;
                let target = if let Expr::Variable(name) = target.as_ref() {
                    name.clone()
                } else if let Expr::Unary { operator, operand } = target.as_ref() {
                    if *operator == Operator::Asterisk {
                        let (op, _) = self.eval_expr(operand)?;
                        if let InterpretValue::Pointer(ptr, ty) = &op {
                            unsafe {
                                match ty {
                                    // ! TODO: FIX ME
                                    Type::Boolean => {
                                        *(*ptr as *mut bool) = match val {
                                            InterpretValue::Boolean(b) => b,
                                            _ => anyhow::bail!("Type mismatch in assignment"),
                                        }
                                    }
                                    Type::Numeric(n) => match n {
                                        NumericType::I8 => {
                                            *(*ptr as *mut i8) = match val {
                                                InterpretValue::I8(v) => v,
                                                _ => anyhow::bail!("Type mismatch in assignment"),
                                            }
                                        }
                                        NumericType::I16 => {
                                            *(*ptr as *mut i16) = match val {
                                                InterpretValue::I16(v) => v,
                                                _ => anyhow::bail!("Type mismatch in assignment"),
                                            }
                                        }
                                        NumericType::I32 => {
                                            *(*ptr as *mut i32) = match val {
                                                InterpretValue::I32(v) => v,
                                                _ => anyhow::bail!("Type mismatch in assignment"),
                                            }
                                        }
                                        NumericType::I64 => {
                                            *(*ptr as *mut i64) = match val {
                                                InterpretValue::I64(v) => v,
                                                _ => anyhow::bail!("Type mismatch in assignment"),
                                            }
                                        }
                                        NumericType::ISize => {
                                            *(*ptr as *mut isize) = match val {
                                                InterpretValue::ISize(v) => v,
                                                _ => anyhow::bail!("Type mismatch in assignment"),
                                            }
                                        }
                                        NumericType::U8 => {
                                            *(*ptr as *mut u8) = match val {
                                                InterpretValue::U8(v) => v,
                                                _ => anyhow::bail!("Type mismatch in assignment"),
                                            }
                                        }
                                        NumericType::U16 => {
                                            *(*ptr as *mut u16) = match val {
                                                InterpretValue::U16(v) => v,
                                                _ => anyhow::bail!("Type mismatch in assignment"),
                                            }
                                        }
                                        NumericType::U32 => {
                                            *(*ptr as *mut u32) = match val {
                                                InterpretValue::U32(v) => v,
                                                _ => anyhow::bail!("Type mismatch in assignment"),
                                            }
                                        }
                                        NumericType::U64 => {
                                            *(*ptr as *mut u64) = match val {
                                                InterpretValue::U64(v) => v,
                                                _ => anyhow::bail!("Type mismatch in assignment"),
                                            }
                                        }
                                        NumericType::USize => {
                                            *(*ptr as *mut usize) = match val {
                                                InterpretValue::USize(v) => v,
                                                _ => anyhow::bail!("Type mismatch in assignment"),
                                            }
                                        }
                                        NumericType::F32 => {
                                            *(*ptr as *mut f32) = match val {
                                                InterpretValue::F32(v) => v,
                                                _ => anyhow::bail!("Type mismatch in assignment"),
                                            }
                                        }
                                        NumericType::F64 => {
                                            *(*ptr as *mut f64) = match val {
                                                InterpretValue::F64(v) => v,
                                                _ => anyhow::bail!("Type mismatch in assignment"),
                                            }
                                        }
                                    },
                                    Type::Named(n) => match n.as_str() {
                                        "String" => {
                                            *(*ptr as *mut String) = match val {
                                                InterpretValue::String(v) => v,
                                                _ => anyhow::bail!("Type mismatch in assignment"),
                                            }
                                        }
                                        _ => anyhow::bail!(
                                            "Dereference is not supported for given value type"
                                        ),
                                    },
                                    _ => anyhow::bail!(
                                        "Dereference is not supported for given value type"
                                    ),
                                }
                            }
                        }
                        return Ok((op, ControlFlow::None));
                    } else {
                        anyhow::bail!("Invalid assignment target");
                    }
                } else {
                    anyhow::bail!("Invalid assignment target");
                };
                self.env.update(target, val)?;
                Ok((InterpretValue::Void, ControlFlow::None))
            }
            Expr::FunctionCall { name, arguments } => self.call_func(name, arguments),
        }
    }

    pub fn call_func(
        &mut self,
        name: &str,
        arguments: &[Expr],
    ) -> anyhow::Result<(InterpretValue, ControlFlow)> {
        let function = self.env.get_function(name)?.clone();

        match function {
            Function::Native { func } => {
                let mut args = Vec::new();
                for arg_expr in arguments {
                    let (arg_value, _) = self.eval_expr(arg_expr)?;
                    args.push(arg_value);
                }
                let result = func.call(args)?;
                Ok((result, ControlFlow::None))
            }
            Function::Interpreted { params, body } => {
                if arguments.len() != params.len() {
                    anyhow::bail!(
                        "Function '{}' expected {} arguments but got {}",
                        name,
                        params.len(),
                        arguments.len()
                    );
                }
                self.env.push_scope();
                for (param, arg_expr) in params.iter().zip(arguments.iter()) {
                    let (arg_value, _) = self.eval_expr(arg_expr)?;
                    self.env.set(param.name.clone(), arg_value)?;
                }
                let control_flow = match &body.stmt {
                    Stmt::Scope { statements } => {
                        let mut result = ControlFlow::None;
                        for stmt in statements {
                            result = self.exec_stmt(stmt)?;
                            match result {
                                ControlFlow::Return(_)
                                | ControlFlow::Break
                                | ControlFlow::Continue => break,
                                ControlFlow::None => {}
                            }
                        }
                        result
                    }
                    _ => self.exec_stmt(&body)?,
                };

                self.env.pop_scope();
                match control_flow {
                    ControlFlow::Return(val) => Ok((val, ControlFlow::None)),
                    _ => Ok((InterpretValue::Void, ControlFlow::None)),
                }
            }
        }
    }

    pub fn interpret(&mut self, statements: &[Statement]) -> anyhow::Result<ControlFlow> {
        let mut control_flow = ControlFlow::None;
        for stmt in statements {
            control_flow = self
                .exec_stmt(stmt)
                .map_err(|e| anyhow::anyhow!("{} at {:?}", e, stmt.location))?;
        }
        Ok(control_flow)
    }
}
