use std::fmt::Display;

use Value::*;

#[allow(clippy::enum_variant_names)]
/// Value represents a runtime value in Lox.
#[derive(Debug)]
pub enum Value {
	Nil,
	Boolean(bool),
	Number(f64),
	StringLiteral(String),
}

impl Display for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Value::Nil => write!(f, "nil"),
			Value::Boolean(b) => write!(f, "{b}"),
			Value::Number(n) => {
				if n.is_finite() && n.fract() == 0.0 {
					write!(f, "{}", *n as i64)
				} else {
					write!(f, "{n}")
				}
			}
			Value::StringLiteral(s) => write!(f, "\"{s}\""),
		}
	}
}

impl Value {
	/// Performs a binary operation between two values.
	pub fn binary_op(&self, op: &crate::scanner::TokenType, right: &Self) -> Option<Value> {
		use crate::scanner::TokenType::*;

		let value = match op {
			Plus => self.plus(right)?,
			Minus => self.minus(right)?,
			Star => self.star(right)?,
			Slash => self.slash(right)?,
			Greater => return self.greater(right).map(Boolean),
			GreaterEqual => return self.greater_equal(right).map(Boolean),
			Less => return self.less(right).map(Boolean),
			LessEqual => return self.less_equal(right).map(Boolean),
			EqualEqual => return self.equal(right).map(Boolean),
			BangEqual => return self.bang_equal(right).map(Boolean),
			_ => return None,
		};
		Some(value)
	}

	/// Determines if the value is considered "true" in a boolean context.
	pub fn to_bool(&self) -> bool {
		match self {
			Value::Nil => false,
			Value::Boolean(b) => *b,
			Value::StringLiteral(s) => !s.is_empty(),
			Value::Number(n) => *n != 0.0,
		}
	}

	/// Tries to add two values together.
	pub fn plus(&self, other: &Self) -> Option<Value> {
		match (self, other) {
			(Number(l), Number(r)) => Some(Number(l + r)),
			(StringLiteral(l), StringLiteral(r)) => Some(StringLiteral(format!("{l}{r}"))),
			(StringLiteral(l), Number(r)) => Some(StringLiteral(format!("{l}{r}"))),
			(Number(l), StringLiteral(r)) => Some(StringLiteral(format!("{l}{r}"))),
			_ => None,
		}
	}

	/// Tries to subtract two values.
	pub fn minus(&self, other: &Self) -> Option<Value> {
		match (self, other) {
			(Number(l), Number(r)) => Some(Number(l - r)),
			_ => None,
		}
	}

	/// Tries to multiply two values together.
	pub fn star(&self, other: &Self) -> Option<Value> {
		match (self, other) {
			(Number(l), Number(r)) => Some(Number(l * r)),
			_ => None,
		}
	}

	/// Tries to divide two values.
	pub fn slash(&self, other: &Self) -> Option<Value> {
		match (self, other) {
			(Number(l), Number(r)) => {
				if *r == 0.0 {
					return None;
				}
				Some(Number(l / r))
			}
			_ => None,
		}
	}

	/// Tries to compare two values for greater-than.
	pub fn greater(&self, other: &Self) -> Option<bool> {
		match (self, other) {
			(Number(l), Number(r)) => Some(l > r),
			_ => None,
		}
	}

	/// Tries to compare two values for greater than or equal.
	pub fn greater_equal(&self, other: &Self) -> Option<bool> {
		match (self, other) {
			(Number(l), Number(r)) => Some(l >= r),
			_ => None,
		}
	}

	/// Tries to compare if self is less than other.
	pub fn less(&self, other: &Self) -> Option<bool> {
		match (self, other) {
			(Number(l), Number(r)) => Some(l < r),
			_ => None,
		}
	}

	/// Tries to compare if self is less than or equal to other.
	pub fn less_equal(&self, other: &Self) -> Option<bool> {
		match (self, other) {
			(Number(l), Number(r)) => Some(l <= r),
			_ => None,
		}
	}

	/// Tries to compare two values for equality.
	pub fn equal(&self, other: &Self) -> Option<bool> {
		Some(match (self, other) {
			(Nil, Nil) => true,
			(Boolean(l), Boolean(r)) => l == r,
			(Number(l), Number(r)) => l == r,
			(StringLiteral(l), StringLiteral(r)) => l == r,
			(Nil, _) => false,
			(_, Nil) => false,

			// 不允许比较的情况
			_ => return None,
		})
	}

	/// Tries to compare two values for inequality.
	pub fn bang_equal(&self, other: &Self) -> Option<bool> { Some(!self.equal(other)?) }
}
