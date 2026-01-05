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

pub(crate) mod callable;
pub(crate) mod value;

use std::{rc::Rc, time::{SystemTime, UNIX_EPOCH}};

use Expression::{Comma as CommaExpression, *};
use value::Value;

use crate::{LoxError, environment::Environment, error::interpreter::InterpreterError, interpreter::callable::{CallableType, CallableValue}, parser::expression::{Expression, LiteralValue::{self, *}}, scanner::{Token, TokenType::*}, statement::Statement, utils::RcCell};

/// Interpreter that evaluates Lox expressions.
pub struct Interpreter {
	environment: Box<Environment>,
}

impl Interpreter {
	pub fn new() -> Self {
		// Define the "clock" native function
		let mut environment = Environment::new();
		let closure = Box::new(|_args: &[RcCell<Value>]| {
			println!("Native function 'clock' called.");
			let now = SystemTime::now().duration_since(UNIX_EPOCH).expect("Time went backwards");
			Value::Number(now.as_secs_f64())
		});
		let callable_value = CallableValue::new_native("clock", Rc::new(vec![]), closure);
		environment.define_native("clock", Value::Callable(callable_value));
		Self { environment: Box::new(environment) }
	}

	pub fn interpret_statements(&mut self, statements: &[Statement]) -> Result<(), InterpreterError> {
		for statement in statements {
			self.interpret_statement(statement)?;
		}
		Ok(())
	}

	fn interpret_statement(&mut self, statement: &Statement) -> Result<(), InterpreterError> {
		match statement {
			Statement::Expression(expression) => {
				self.evaluate(expression)?;
			}
			Statement::Print(expression) => {
				let value = self.evaluate(expression)?;
				let value = value.borrow();
				println!("{value}");
			}
			Statement::VarDeclaration { name_token, initializer } => {
				let value = if let Some(initializer_expr) = initializer {
					self.evaluate(initializer_expr)?
				} else {
					RcCell::new(Value::Nil)
				};
				self.environment.define(name_token, value);
			}
			Statement::Block(statements) => {
				self.interpret_block(statements, Environment::new())?;
			}
			Statement::If { condition, then_branch, else_branch } => {
				let condition_value = self.evaluate(condition)?;
				let condition_value = condition_value.borrow();
				if condition_value.to_bool() {
					self.interpret_statement(then_branch)?
				} else if let Some(else_branch) = else_branch {
					self.interpret_statement(else_branch)?
				}
			}
			Statement::While { condition, body } => {
				while self.evaluate(condition)?.borrow().to_bool() {
					match self.interpret_statement(body) {
						Ok(_) => {}
						Err(InterpreterError::Break) => break,
						Err(e) => return Err(e),
					}
				}
			}
			Statement::Break => {
				// Return Break error to signal loop termination
				return Err(InterpreterError::Break);
			}
			Statement::FunctionDeclaration { name_token, parameters, body } => {
				let callable = CallableValue::new_lox(name_token.lexeme, parameters.clone(), body.clone());
				let value = Value::Callable(callable);
				self.environment.define(name_token, RcCell::new(value));
			}
			Statement::Return(expression) => {
				let value = if let Some(expression) = expression {
					self.evaluate(expression)?
				} else {
					RcCell::new(Value::Nil)
				};
				return Err(InterpreterError::Return(value));
			}
		}
		Ok(())
	}

	fn interpret_block(
		&mut self,
		statements: &[Statement],
		new_environment: Environment,
	) -> Result<(), InterpreterError> {
		// Take the current environment and create a new one with it as the outer
		let current_env = std::mem::take(&mut self.environment);
		*self.environment = new_environment.set_outer(current_env);

		let result = self.interpret_statements(statements);

		// Restore the original environment by taking the inner environment
		if let Some(outer) = self.environment.outer.take() {
			self.environment = outer;
		}
		result
	}

	/// Interpret the given expression and print the result.
	pub fn interpret_expression(&mut self, expr: Expression) -> Result<(), LoxError> {
		let value = self.evaluate(&expr)?;
		let value = value.borrow();
		println!("{value}");
		Ok(())
	}

	/// Evaluate the given expression and return its value.
	fn evaluate(&mut self, expr: &Expression) -> Result<RcCell<Value>, InterpreterError> {
		Ok(match expr {
			Literal(lit) => RcCell::new(match lit {
				LiteralValue::Nil => Value::Nil,
				Boolean(b) => Value::Boolean(*b),
				LiteralValue::Number(n) => Value::Number(*n),
				StringLiteral(s) => Value::StringValue(s.to_string()),
			}),
			Unary { operator, right } => {
				let right_value = self.evaluate(right)?;
				let right_value = right_value.borrow();
				RcCell::new(match (&operator.r#type, &*right_value) {
					(Minus, Value::Number(n)) => Value::Number(-n),
					(Bang, v) => Value::Boolean(!v.to_bool()),
					_ => {
						return Err(InterpreterError::UnaryOperationError(format!(
							"line {}: {operator:?} {right_value:?}",
							operator.line
						)));
					}
				})
			}
			Binary { left, operator, right } => {
				let left_value = self.evaluate(left)?;
				let left_value = left_value.borrow();
				let right_value = self.evaluate(right)?;
				let right_value = right_value.borrow();
				RcCell::new(left_value.binary_op(&operator.r#type, &right_value).ok_or(
					InterpreterError::BinaryOperationError(format!(
						"line {}: {left_value} {} {right_value}",
						operator.line, operator.lexeme
					)),
				)?)
			}
			Grouping(inner) => self.evaluate(inner)?,
			CommaExpression { left, right } => {
				self.evaluate(left)?;
				self.evaluate(right)?
			}
			Ternary { condition, then_branch, else_branch } => {
				let condition_value = self.evaluate(condition)?;
				if condition_value.borrow().to_bool() {
					self.evaluate(then_branch)?
				} else {
					self.evaluate(else_branch)?
				}
			}
			Variable(token) => self.environment.get(token).ok_or_else(|| {
				InterpreterError::UndefinedVariable(format!("line {}: '{}'", token.line, token.lexeme))
			})?,
			Assign { target, value } => {
				let value = self.evaluate(value)?;
				self.environment.assign(target, value.clone())?;
				value
			}
			Logical { left, operator, right } => {
				let left_value = self.evaluate(left)?;
				match operator.r#type {
					And => {
						if !left_value.borrow().to_bool() {
							left_value
						} else {
							self.evaluate(right)?
						}
					}
					Or => {
						if left_value.borrow().to_bool() {
							left_value
						} else {
							self.evaluate(right)?
						}
					}
					_ => {
						return Err(InterpreterError::LogicalOperationError(format!(
							"line {}: {:?}",
							operator.line, operator.r#type
						)));
					}
				}
			}
			Call { callee, paren, arguments } => {
				let callee_value = self.evaluate(callee)?;
				let callee_value = &*callee_value.borrow();
				let mut arg_values = Vec::new();
				for arg in arguments {
					arg_values.push(self.evaluate(arg)?);
				}
				self.call(callee_value, paren, &arg_values)?
			}
		})
	}

	fn call(
		&mut self,
		value: &Value,
		paren: &Token,
		args: &[RcCell<Value>],
	) -> Result<RcCell<Value>, InterpreterError> {
		match value {
			Value::Callable(CallableValue { name, parameters, body }) => {
				if args.len() != parameters.len() {
					return Err(InterpreterError::ArgumentError(format!(
						"line {}: Function {name} Expected {} arguments but got {}.",
						paren.line,
						parameters.len(),
						args.len()
					)));
				}
				Ok(match body {
					CallableType::Native(func) => RcCell::new(func(args)),
					CallableType::Lox(statements) => {
						let mut environment = Environment::new();
						for (i, parameter) in parameters.iter().enumerate() {
							environment.define(parameter, args[i].clone());
						}
						match self.interpret_block(statements, environment) {
							Ok(_) => RcCell::new(Value::Nil),
							Err(InterpreterError::Return(value)) => value,
							Err(e) => return Err(e),
						}
					}
				})
			}
			_ => Err(InterpreterError::NotCallable(format!("line {}: '{value}'", paren.line))),
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::{parser::Parser, scanner::Scanner};

	fn run(input: &'static str) -> Result<Value, InterpreterError> {
		let scanner = Scanner::new(input);
		let tokens = scanner.scan_tokens().unwrap();
		let parser = Parser::new(tokens);
		let statements = parser.parse_statements().unwrap();
		let mut interpreter = Interpreter::new();

		// Execute all statements
		for statement in statements {
			interpreter.interpret_statement(&statement)?;
		}

		Ok(Value::Nil)
	}

	#[test]
	fn test_variable_declaration() {
		// Test variable declaration
		let result = run("var x = 10;");
		assert!(result.is_ok());

		// Test variable reference
		let scanner = Scanner::new("var x = 10; print x;");
		let tokens = scanner.scan_tokens().unwrap();
		let parser = Parser::new(tokens);
		let statements = parser.parse_statements().unwrap();
		let mut interpreter = Interpreter::new();

		assert!(interpreter.interpret_statements(&statements).is_ok());
	}

	#[test]
	fn test_variable_assignment() {
		let scanner = Scanner::new("var x = 10; x = 20; print x;");
		let tokens = scanner.scan_tokens().unwrap();
		let parser = Parser::new(tokens);
		let statements = parser.parse_statements().unwrap();
		let mut interpreter = Interpreter::new();

		// Test assignment and print
		assert!(interpreter.interpret_statements(&statements).is_ok());
	}

	#[test]
	fn test_undefined_variable() {
		let scanner = Scanner::new("print undefined_var;");
		let tokens = scanner.scan_tokens().unwrap();
		let parser = Parser::new(tokens);
		let statements = parser.parse_statements().unwrap();
		let mut interpreter = Interpreter::new();

		// Test undefined variable should cause error
		assert!(interpreter.interpret_statements(&statements).is_err());
	}
}
