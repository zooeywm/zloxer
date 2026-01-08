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
			// The Callable value in Value form
			let value = instance
				.fields
				.get(property.lexeme)
				.cloned()
				.or_else(|| instance.class.borrow().find_method(property.lexeme))
				.ok_or(InterpreterError::UndefinedProperty(property.lexeme))?;

			if let Value::Callable(CallableValue { closure, .. }) = &*value.clone().borrow() {
				let mut closure = closure.borrow_mut();
				// If the value is callable, it is a value,
				// we add the instance itself to the method closure.
				closure.define("this", this.clone());

				if let Some(superclass) = &instance.class.borrow().superclass {
					// Also, if the class of this instance has superclass, we define the
					// superclass in the closure so that `super` keyword can be used.
					closure.define("super", superclass.clone());
				}
			}

			Ok(value)
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
