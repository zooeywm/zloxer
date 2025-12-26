use std::fmt::Display;

use Expression::*;
use Value::*;

use crate::{LoxError, error::interpreter::InterpreterError, parser::expression::{Expression, LiteralValue::*}, scanner::TokenType::*};

/// Value represents a runtime value in Lox.
#[derive(Debug)]
enum Value {
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
	/// Determines if the value is considered "true" in a boolean context.
	fn is_true(&self) -> bool {
		match self {
			Value::Null => false,
			Value::Bool(b) => *b,
			Value::Str(s) => !s.is_empty(),
			Value::Num(n) => *n != 0.0,
		}
	}
}

/// Interpreter interprets and evaluates Lox expressions.
pub struct Interpreter;

impl<'a> Interpreter {
	/// Interpret the given expression and print the result.
	pub fn interpret(&'a self, expr: Expression<'a>) -> Result<(), LoxError> {
		let value = self.evaluate(expr)?;
		println!("{value}");
		Ok(())
	}

	/// Evaluate the given expression and return its value.
	fn evaluate(&'a self, expr: Expression<'a>) -> Result<Value, InterpreterError> {
		let value = match expr {
			Literal(lit) => match lit {
				NilLiteral => Null,
				Boolean(b) => Bool(b),
				Number(n) => Num(n),
				StringLiteral2(s) => Str(s.to_string()),
			},
			Unary { operator, right } => {
				let right_value = self.evaluate(*right)?;
				match (&operator.r#type, &right_value) {
					(Minus, Num(n)) => Num(-n),
					(Bang, v) => Bool(!v.is_true()),
					_ => {
						return Err(InterpreterError::UnaryOperationError(format!(
							"line {}: {operator:?} {right_value:?}",
							operator.line
						)));
					}
				}
			}
			Binary { left, operator, right } => {
				let left_value = self.evaluate(*left)?;
				let right_value = self.evaluate(*right)?;
				match (&left_value, &operator.r#type, &right_value) {
					(Num(l), Plus, Num(r)) => Num(l + r),
					(Str(l), Plus, Str(r)) => Str(format!("{}{}", l, r)),
					(Num(l), Minus, Num(r)) => Num(l - r),
					(Num(l), Star, Num(r)) => Num(l * r),
					(Num(l), Slash, Num(r)) => Num(l / r),
					(Num(l), Greater, Num(r)) => Bool(l > r),
					(Num(l), GreaterEqual, Num(r)) => Bool(l >= r),
					(Num(l), Less, Num(r)) => Bool(l < r),
					(Num(l), LessEqual, Num(r)) => Bool(l <= r),
					// equal
					(Num(l), EqualEqual, Num(r)) => Bool(l == r),
					(Bool(l), EqualEqual, Bool(r)) => Bool(l == r),
					(Null, EqualEqual, Null) => Bool(true),
					(Null, EqualEqual, _) => Bool(false),
					(_, EqualEqual, Null) => Bool(false),
					// !equal
					(Num(l), BangEqual, Num(r)) => Bool(l != r),
					(Bool(l), BangEqual, Bool(r)) => Bool(l != r),
					(Null, BangEqual, Null) => Bool(false),
					(Null, BangEqual, _) => Bool(true),
					(_, BangEqual, Null) => Bool(true),
					_ => {
						return Err(InterpreterError::BinaryOperationError(format!(
							"line {}: {left_value} {} {right_value}",
							operator.line, operator.lexeme
						)));
					}
				}
			}
			Grouping(inner) => self.evaluate(*inner)?,
			Comma { left, right } => {
				self.evaluate(*left)?;
				self.evaluate(*right)?
			}
			Ternary { condition, then_branch, else_branch } => {
				let condition_value = self.evaluate(*condition)?;
				if condition_value.is_true() { self.evaluate(*then_branch)? } else { self.evaluate(*else_branch)? }
			}
		};
		Ok(value)
	}
}
