use std::{collections::HashMap, fmt::Display};

use crate::{error::interpreter::InterpreterError, interpreter::{class::ClassValue, value::Value}, scanner::Token, utils::RcCell};

/// Fields are named bits of state stored directly in an instance. Properties
/// are the named things, that a get expression may return. Every field is
/// a property, but not every property is a field.
#[derive(Debug)]
pub struct InstanceValue {
	pub class:  RcCell<ClassValue>,
	pub fields: HashMap<&'static str, RcCell<Value>>,
}

impl InstanceValue {
	pub fn new(class: RcCell<ClassValue>) -> Self { Self { class, fields: HashMap::new() } }

	pub fn get(&self, property: &Token) -> Result<RcCell<Value>, InterpreterError> {
		self.fields.get(property.lexeme).cloned().ok_or(InterpreterError::UndefinedProperty(property.lexeme))
	}
}

impl Display for InstanceValue {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "instance of {}", self.class.borrow())
	}
}
