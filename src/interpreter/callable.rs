use std::{fmt::Debug, rc::Rc, time::{SystemTime, UNIX_EPOCH}};

use crate::{environment::Environment, interpreter::value::Value, scanner::Token, statement::Statement, utils::RcCell};

type NativeFunction = Rc<dyn Fn(&[RcCell<Value>]) -> Value>;

/// Native function definitions
pub mod native {
	use super::*;

	/// Returns the number of seconds since the program started
	pub fn clock(_args: &[RcCell<Value>]) -> Value {
		let now = SystemTime::now().duration_since(UNIX_EPOCH).expect("Time went backwards");
		Value::Number(now.as_secs_f64())
	}

	/// Returns a list of all native functions
	pub fn get_all_natives() -> Vec<(&'static str, NativeFunction)> { vec![("clock", Rc::new(clock))] }
}

#[derive(Debug, Clone)]
pub(crate) struct CallableValue {
	pub name:       &'static str,
	pub parameters: Rc<Vec<Token>>,
	pub body:       CallableType,
	pub closure:    RcCell<Environment>,
}

#[derive(Clone)]
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
	pub fn new_lox(
		name: &'static str,
		parameters: Rc<Vec<Token>>,
		body: Rc<Vec<Statement>>,
		closure: RcCell<Environment>,
	) -> Self {
		Self { name, parameters, body: CallableType::Lox(body), closure }
	}

	/// Creates a new native function from a predefined native function
	pub fn from_native_function(name: &'static str, func: NativeFunction) -> Self {
		Self {
			name,
			parameters: Rc::new(Vec::new()),
			body: CallableType::Native(func),
			closure: RcCell::default(),
		}
	}
}
