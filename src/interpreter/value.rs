use std::fmt::Display;

use Value::*;

use crate::interpreter::callable::CallableValue;

#[allow(clippy::enum_variant_names)]
#[derive(Debug)]
/// Value represents a runtime value in Lox.
pub(crate) enum Value {
	Nil,
	Boolean(bool),
	Number(f64),
	StringValue(String),
	Callable(CallableValue),
}

impl Display for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Nil => write!(f, "nil"),
			Boolean(b) => write!(f, "{b}"),
			Number(n) => {
				if n.is_finite() && n.fract() == 0.0 {
					write!(f, "{}", *n as i64)
				} else {
					write!(f, "{n}")
				}
			}
			StringValue(s) => write!(f, "{s}"),
			Callable(CallableValue { name, parameters, body: _, closure: _ }) => {
				write!(f, "<fn {name}(")?;
				let mut first = true;
				for param in parameters.iter() {
					if !first {
						write!(f, ", ")?;
					}
					first = false;
					write!(f, "{}", param.lexeme)?;
				}
				write!(f, ")>")
			}
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
			Nil => false,
			Boolean(b) => *b,
			StringValue(s) => !s.is_empty(),
			Number(n) => *n != 0.0,
			Callable { .. } => true,
		}
	}

	/// Tries to add two values together.
	pub fn plus(&self, other: &Self) -> Option<Value> {
		match (self, other) {
			(Number(l), Number(r)) => Some(Number(l + r)),
			(StringValue(l), StringValue(r)) => Some(StringValue(format!("{l}{r}"))),
			(StringValue(l), Number(r)) => Some(StringValue(format!("{l}{r}"))),
			(Number(l), StringValue(r)) => Some(StringValue(format!("{l}{r}"))),
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
			(StringValue(l), StringValue(r)) => l == r,
			(Nil, _) => false,
			(_, Nil) => false,
			_ => return None,
		})
	}

	/// Tries to compare two values for inequality.
	pub fn bang_equal(&self, other: &Self) -> Option<bool> { Some(!self.equal(other)?) }
}
