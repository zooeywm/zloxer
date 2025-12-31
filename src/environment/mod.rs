use std::collections::HashMap;

use crate::{error::interpreter::InterpreterError, interpreter::value::Value, scanner::Token};

#[derive(Default)]
pub struct Environment {
	variables: HashMap<&'static str, Value>,
	pub outer: Option<Box<Environment>>,
}

impl Environment {
	pub fn new(outer: Option<Box<Environment>>) -> Self { Self { variables: HashMap::new(), outer } }

	/// A variable statement doesn’t just define a new variable, it can also be
	/// used to redefine an existing variable.
	pub fn define(&mut self, token: &Token, value: Value) { self.variables.insert(token.lexeme, value); }

	pub fn get(&self, token: &Token) -> Option<&Value> {
		self.variables.get(token.lexeme).or_else(|| self.outer.as_ref().and_then(|env| env.get(token)))
	}

	/// Assign a value to an existing variable.
	pub fn assign(&mut self, token: &Token, value: Value) -> Result<(), InterpreterError> {
		if let Some(v) = self.variables.get_mut(token.lexeme) {
			*v = value;
			Ok(())
		} else if let Some(outer) = self.outer.as_mut() {
			outer.assign(token, value)
		} else {
			Err(InterpreterError::UndefinedVariable(format!("line {}: '{}'", token.line, token.lexeme)))
		}
	}
}
