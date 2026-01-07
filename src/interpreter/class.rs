use std::fmt::Display;

#[derive(Debug)]
pub struct ClassValue {
	pub name: &'static str,
}

impl ClassValue {
	pub fn new(name: &'static str) -> Self { Self { name } }
}

impl Display for ClassValue {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { write!(f, "class <{}>", self.name) }
}
