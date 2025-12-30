use std::collections::HashMap;

use crate::{error::interpreter::InterpreterError, interpreter::value::Value, scanner::Token};

#[derive(Default)]
pub struct Environment<'a> {
	variables: HashMap<&'a str, Value>,
	pub outer: Option<Box<Environment<'a>>>,
}

impl<'a> Environment<'a> {
	pub fn new(outer: Option<Box<Environment<'a>>>) -> Self { Self { variables: HashMap::new(), outer } }

	/// A variable statement doesn’t just define a new variable, it can also be
	/// used to redefine an existing variable.
	pub fn define(&mut self, token: Token<'a>, value: Value) { self.variables.insert(token.lexeme, value); }

	pub fn get(&self, token: &Token<'a>) -> Option<&Value> {
		self.variables.get(token.lexeme).or_else(|| self.outer.as_ref().and_then(|env| env.get(token)))
	}

	/// Assign a value to an existing variable.
	pub fn assign(&mut self, token: &Token<'a>, value: Value) -> Result<(), InterpreterError> {
		self
			.variables
			.get_mut(token.lexeme)
			.map(|v| *v = value)
			.ok_or_else(|| InterpreterError::UndefinedVariable(format!("line {}: '{}'", token.line, token.lexeme)))
	}
}
