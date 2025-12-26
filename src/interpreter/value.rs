use std::fmt::Display;

use Value::*;

/// Value represents a runtime value in Lox.
#[derive(Debug)]
pub(super) enum Value {
	Null,
	Bool(bool),
	Num(f64),
	Str(String),
}

impl Display for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Value::Null => write!(f, "nil"),
			Value::Bool(b) => write!(f, "{b}"),
			Value::Num(n) => {
				if n.is_finite() && n.fract() == 0.0 {
					write!(f, "{}", *n as i64)
				} else {
					write!(f, "{n}")
				}
			}
			Value::Str(s) => write!(f, "\"{s}\""),
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
			Greater => return self.greater(right).map(Bool),
			GreaterEqual => return self.greater_equal(right).map(Bool),
			Less => return self.less(right).map(Bool),
			LessEqual => return self.less_equal(right).map(Bool),
			EqualEqual => return self.equal(right).map(Bool),
			BangEqual => return self.bang_equal(right).map(Bool),
			_ => return None,
		};
		Some(value)
	}

	/// Determines if the value is considered "true" in a boolean context.
	pub fn to_bool(&self) -> bool {
		match self {
			Value::Null => false,
			Value::Bool(b) => *b,
			Value::Str(s) => !s.is_empty(),
			Value::Num(n) => *n != 0.0,
		}
	}

	/// Tries to add two values together.
	pub fn plus(&self, other: &Self) -> Option<Value> {
		match (self, other) {
			(Num(l), Num(r)) => Some(Num(l + r)),
			(Str(l), Str(r)) => Some(Str(format!("{l}{r}"))),
			(Str(l), Num(r)) => Some(Str(format!("{l}{r}"))),
			(Num(l), Str(r)) => Some(Str(format!("{l}{r}"))),
			_ => None,
		}
	}

	/// Tries to subtract two values.
	pub fn minus(&self, other: &Self) -> Option<Value> {
		match (self, other) {
			(Num(l), Num(r)) => Some(Num(l - r)),
			_ => None,
		}
	}

	/// Tries to multiply two values together.
	pub fn star(&self, other: &Self) -> Option<Value> {
		match (self, other) {
			(Num(l), Num(r)) => Some(Num(l * r)),
			_ => None,
		}
	}

	/// Tries to divide two values.
	pub fn slash(&self, other: &Self) -> Option<Value> {
		match (self, other) {
			(Num(l), Num(r)) => {
				if *r == 0.0 {
					return None;
				}
				Some(Num(l / r))
			}
			_ => None,
		}
	}

	/// Tries to compare two values for greater-than.
	pub fn greater(&self, other: &Self) -> Option<bool> {
		match (self, other) {
			(Num(l), Num(r)) => Some(l > r),
			_ => None,
		}
	}

	/// Tries to compare two values for greater than or equal.
	pub fn greater_equal(&self, other: &Self) -> Option<bool> {
		match (self, other) {
			(Num(l), Num(r)) => Some(l >= r),
			_ => None,
		}
	}

	/// Tries to compare if self is less than other.
	pub fn less(&self, other: &Self) -> Option<bool> {
		match (self, other) {
			(Num(l), Num(r)) => Some(l < r),
			_ => None,
		}
	}

	/// Tries to compare if self is less than or equal to other.
	pub fn less_equal(&self, other: &Self) -> Option<bool> {
		match (self, other) {
			(Num(l), Num(r)) => Some(l <= r),
			_ => None,
		}
	}

	/// Tries to compare two values for equality.
	pub fn equal(&self, other: &Self) -> Option<bool> {
		Some(match (self, other) {
			(Null, Null) => true,
			(Bool(l), Bool(r)) => l == r,
			(Num(l), Num(r)) => l == r,
			(Str(l), Str(r)) => l == r,
			(Null, _) => false,
			(_, Null) => false,

			// 不允许比较的情况
			_ => return None,
		})
	}

	/// Tries to compare two values for inequality.
	pub fn bang_equal(&self, other: &Self) -> Option<bool> { Some(!self.equal(other)?) }
}
