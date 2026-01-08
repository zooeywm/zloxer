use std::{collections::HashMap, fmt::Display};

use crate::{interpreter::value::Value, utils::RcCell};

#[derive(Debug)]
pub struct ClassValue {
	pub name:       &'static str,
	pub superclass: Option<RcCell<Value>>,
	pub methods:    HashMap<&'static str, RcCell<Value>>,
}

impl ClassValue {
	pub fn new(name: &'static str, methods: HashMap<&'static str, RcCell<Value>>) -> Self {
		Self { name, methods, superclass: None }
	}

	pub fn find_method(&self, name: &str) -> Option<RcCell<Value>> {
		self.methods.get(name).cloned().or_else(|| {
			if let Some(superclass) = self.superclass.as_ref() {
				if let Value::Class(superclass) = &*superclass.borrow() {
					return superclass.borrow().find_method(name);
				} else {
					unreachable!();
				}
			}
			None
		})
	}
}

impl Display for ClassValue {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { write!(f, "class <{}>", self.name) }
}
