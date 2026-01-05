use std::{fmt::Debug, rc::Rc};

use crate::{interpreter::value::Value, scanner::Token, statement::Statement, utils::RcCell};

type NativeFunction = Box<dyn Fn(&[RcCell<Value>]) -> Value>;

#[derive(Debug)]
pub(crate) struct CallableValue {
	pub name:       &'static str,
	pub parameters: Rc<Vec<Token>>,
	pub body:       CallableType,
}

pub(crate) enum CallableType {
	Native(NativeFunction),
	Lox(Rc<Vec<Statement>>),
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
	pub fn new_lox(name: &'static str, parameters: Rc<Vec<Token>>, body: Rc<Vec<Statement>>) -> Self {
		Self { name, parameters, body: CallableType::Lox(body) }
	}

	pub fn new_native(name: &'static str, parameters: Rc<Vec<Token>>, body: NativeFunction) -> Self {
		Self { name, parameters, body: CallableType::Native(body) }
	}
}
