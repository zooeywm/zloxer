use std::{collections::HashMap, fmt::Display};

use crate::{error::interpreter::InterpreterError, interpreter::{callable::CallableValue, class::ClassValue, value::Value}, scanner::Token, utils::RcCell};

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

	pub fn get(this: RcCell<Value>, property: &Token) -> Result<RcCell<Value>, InterpreterError> {
		if let Value::Instance(instance) = &*this.borrow() {
			let value = instance
				.fields
				.get(property.lexeme)
				.cloned()
				.or_else(|| {
					instance
						.class
						.borrow()
						.methods
						.get(property.lexeme)
						.map(|method| RcCell::new(Value::Callable(method.clone())))
				})
				.ok_or(InterpreterError::UndefinedProperty(property.lexeme))?;
			Ok(match &mut *value.clone().borrow_mut() {
				Value::Callable(CallableValue { closure, .. }) => {
					closure.borrow_mut().define("this", this.clone());
					value
				}
				_ => value,
			})
		} else {
			Err(InterpreterError::GetPropertyError)
		}
	}

	pub fn set(&mut self, property: &Token, value: RcCell<Value>) {
		self.fields.insert(property.lexeme, value);
	}
}

impl Display for InstanceValue {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "instance of {}", self.class.borrow())
	}
}
