use std::{cell::RefCell, fmt::Debug, rc::Rc};

use crate::{environment::Environment, interpreter::value::Value, statement::Statement};

type NativeFunction = Box<dyn Fn(&[Rc<RefCell<Value>>]) -> Value>;

#[derive(Debug)]
pub(crate) struct CallableValue {
	pub arity:   usize,
	pub body:    CallableType,
	pub closure: Environment,
}

pub(crate) enum CallableType {
	Native(NativeFunction),
	Lox(Vec<Statement>),
}

impl Debug for CallableType {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Native(_) => f.debug_tuple("Native").field(&"Function Pointer").finish(),
			Self::Lox(arg0) => f.debug_tuple("Lox").field(arg0).finish(),
		}
	}
}

impl CallableValue {
	pub fn new_lox(arity: usize, body: Vec<Statement>, closure: Environment) -> Self {
		Self { arity, body: CallableType::Lox(body), closure }
	}

	pub fn new_native(arity: usize, body: NativeFunction, closure: Environment) -> Self {
		Self { arity, body: CallableType::Native(body), closure }
	}

	pub fn call(&self, args: &[Rc<RefCell<Value>>]) -> Rc<RefCell<Value>> {
		Rc::new(RefCell::new(match &self.body {
			CallableType::Native(func) => func(args),
			CallableType::Lox(_statements) => {
				// Lox function call handling would go here
				unimplemented!("Lox function calls are not implemented yet.")
			}
		}))
	}
}
