use std::{collections::HashMap, fmt::Display};

use crate::{interpreter::value::Value, utils::RcCell};

#[derive(Debug)]
pub struct ClassValue {
	pub name:    &'static str,
	pub methods: HashMap<&'static str, RcCell<Value>>,
}

impl ClassValue {
	pub fn new(name: &'static str, methods: HashMap<&'static str, RcCell<Value>>) -> Self {
		Self { name, methods }
	}
}

impl Display for ClassValue {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { write!(f, "class <{}>", self.name) }
}
