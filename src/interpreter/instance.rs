use std::fmt::Display;

use crate::{interpreter::class::ClassValue, utils::RcCell};

#[derive(Debug)]
pub struct InstanceValue {
	pub class: RcCell<ClassValue>,
}

impl InstanceValue {
	pub fn new(class: RcCell<ClassValue>) -> Self { Self { class } }
}

impl Display for InstanceValue {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "instance of {}", self.class.borrow())
	}
}
