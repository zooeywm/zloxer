use std::collections::HashMap;

use crate::{error::interpreter::InterpreterError, interpreter::value::Value, scanner::Token, utils::RcCell};

/// Clone the RcCell, it's shallow copy
#[derive(Default, Debug, Clone)]
pub struct Environment {
	variables: HashMap<&'static str, RcCell<Value>>,
	pub outer: Option<Box<Environment>>,
	closure:   Option<RcCell<Environment>>,
}

impl Environment {
	pub fn new() -> Self { Self { variables: HashMap::new(), outer: None, closure: None } }

	pub fn set_outer(mut self, outer: Box<Environment>) -> Self {
		self.outer = Some(outer);
		self
	}

	pub fn set_closure(mut self, closure: RcCell<Environment>) -> Self {
		self.closure = Some(closure);
		self
	}

	/// A variable statement doesn’t just define a new variable, it can also be
	/// used to redefine an existing variable.
	pub fn define(&mut self, token: &Token, value: RcCell<Value>) {
		self.variables.insert(token.lexeme, value);
	}

	pub fn define_native(&mut self, name: &'static str, value: Value) {
		self.variables.insert(name, RcCell::new(value));
	}

	pub fn get(&self, token: &Token) -> Option<RcCell<Value>> {
		self
			.variables
			.get(token.lexeme)
			.cloned()
			.or_else(|| self.closure.as_ref().and_then(|env| env.borrow().get(token)))
			.or_else(|| self.outer.as_ref().and_then(|env| env.get(token)))
	}

	/// Assign a value to an existing variable.
	pub fn assign(&mut self, token: &Token, value: RcCell<Value>) -> Result<(), InterpreterError> {
		if let Some(v) = self.variables.get_mut(token.lexeme) {
			*v = value;
			Ok(())
		} else if let Some(closure) = self.closure.as_mut() {
			closure.borrow_mut().assign(token, value)
		} else if let Some(outer) = self.outer.as_mut() {
			outer.assign(token, value)
		} else {
			Err(InterpreterError::UndefinedVariable(format!("line {}: '{}'", token.line, token.lexeme)))
		}
	}
}
