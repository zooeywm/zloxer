use std::collections::HashMap;

use crate::{interpreter::value::Value, scanner::Token};

pub struct Environment<'a> {
	pub variables: HashMap<&'a str, Value>,
}

impl<'a> Environment<'a> {
	pub fn new() -> Self { Self { variables: HashMap::new() } }

	/// A variable statement doesn’t just define a new variable, it can also be
	/// used to redefine an existing variable.
	pub fn define(&mut self, token: Token<'a>, value: Value) { self.variables.insert(token.lexeme, value); }

	pub fn get(&self, token: Token<'a>) -> Option<&Value> { self.variables.get(token.lexeme) }
}
