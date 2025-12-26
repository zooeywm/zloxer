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

use Expression::{Comma as CommaExpression, *};
use value::Value;

use crate::{LoxError, error::interpreter::InterpreterError, parser::expression::{Expression, LiteralValue::{self, *}}, scanner::TokenType::*, statement::Statement};

/// Interpreter that evaluates Lox expressions.
pub struct Interpreter;

impl<'a> Interpreter {
	pub fn interpret(&self, statements: Vec<Statement<'a>>) -> Result<(), InterpreterError> {
		for statement in statements {
			self.interpret_statement(statement)?;
		}
		Ok(())
	}

	fn interpret_statement(&self, statement: Statement<'a>) -> Result<(), InterpreterError> {
		match statement {
			Statement::Expression(expression) => {
				self.evaluate(*expression)?;
			}
			Statement::Print(expression) => {
				let value = self.evaluate(*expression)?;
				println!("{value}");
			}
		}
		Ok(())
	}

	/// Interpret the given expression and print the result.
	#[allow(dead_code)]
	pub fn interpret_expression(&'a self, expr: Expression<'a>) -> Result<(), LoxError> {
		let value = self.evaluate(expr)?;
		println!("{value}");
		Ok(())
	}

	/// Evaluate the given expression and return its value.
	fn evaluate(&'a self, expr: Expression<'a>) -> Result<Value, InterpreterError> {
		Ok(match expr {
			Literal(lit) => match lit {
				LiteralValue::Nil => Value::Nil,
				Boolean(b) => Value::Boolean(b),
				LiteralValue::Number(n) => Value::Number(n),
				StringLiteral(s) => Value::StringLiteral(s.to_string()),
			},
			Unary { operator, right } => {
				let right_value = self.evaluate(*right)?;
				match (&operator.r#type, &right_value) {
					(Minus, Value::Number(n)) => Value::Number(-n),
					(Bang, v) => Value::Boolean(!v.to_bool()),
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
			CommaExpression { left, right } => {
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
