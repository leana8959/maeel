use crate::enums::vmtype::VMType;
use std::ops::Mul;

/// Mul operation trait implementation for all types
impl Mul for VMType {
    type Output = VMType;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMType::Integer(m), VMType::Integer(n)) => VMType::Integer(m * n),
            (VMType::Float(x), VMType::Float(y)) => VMType::Float(x * y),
            (VMType::Integer(n), VMType::Float(x)) => VMType::Float(n as f64 * x),
            (VMType::Float(x), VMType::Integer(n)) => VMType::Float(x * n as f64),
            (a, b) => panic!("can't mul {a:?} and {b:?}"),
        }
    }
}
