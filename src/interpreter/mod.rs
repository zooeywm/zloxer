//! Lox expression interpreter.
//!
//! The interpreter walks the abstract syntax tree (AST) produced by the parser,
//! recursively evaluating each expression and computing its runtime value.
//!
//! # Expression Types
//!
//! - **Literals**: `nil`, booleans, numbers, strings
//! - **Unary**: `-` (negation), `!` (logical NOT)
//! - **Binary**: `+`, `-`, `*`, `/`, comparisons, equality
//! - **Grouping**: Parenthesized expressions
//! - **Comma**: Sequence operator (evaluates left, returns right)
//! - **Ternary**: Conditional operator `?:`

mod value;

use Expression::*;
use value::Value::*;

use crate::{LoxError, error::interpreter::InterpreterError, interpreter::value::Value, parser::expression::{Expression, LiteralValue::*}, scanner::TokenType::*};

/// Interpreter that evaluates Lox expressions.
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
		Ok(match expr {
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
					(Bang, v) => Bool(!v.to_bool()),
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
				left_value.binary_op(&operator.r#type, &right_value).ok_or(InterpreterError::BinaryOperationError(
					format!("line {}: {left_value} {} {right_value}", operator.line, operator.lexeme),
				))?
			}
			Grouping(inner) => self.evaluate(*inner)?,
			Comma { left, right } => {
				self.evaluate(*left)?;
				self.evaluate(*right)?
			}
			Ternary { condition, then_branch, else_branch } => {
				let condition_value = self.evaluate(*condition)?;
				if condition_value.to_bool() { self.evaluate(*then_branch)? } else { self.evaluate(*else_branch)? }
			}
		})
	}
}
