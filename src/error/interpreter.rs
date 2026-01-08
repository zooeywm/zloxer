use crate::{interpreter::value::Value, utils::RcCell};

#[derive(thiserror::Error, Debug)]
/// Errors that can occur during interpretation
pub enum InterpreterError {
	/// Error for invalid unary operations
	#[error("Invalid unary operation: {0}")]
	UnaryOperationError(String),
	/// Error for invalid binary operations
	#[error("Invalid binary operation: {0}")]
	BinaryOperationError(String),
	/// Error for undefined variables
	#[error("Undefined variable: {0}")]
	UndefinedVariable(String),
	#[error("Invalid logical operation: {0}")]
	LogicalOperationError(String),
	/// Error for break statements
	#[error("Break statement")]
	Break,
	#[error("Not callable: {0}")]
	NotCallable(String),
	#[error("Argument error: {0}")]
	ArgumentError(String),
	#[error("")]
	#[allow(private_interfaces)]
	Return(RcCell<Value>),
	#[error("Cannot return a value from initializer")]
	ReturnFromInitializer,
	#[error("Cannot get property from non-instance")]
	GetPropertyError,
	#[error("Cannot set property to non-instance")]
	SetPropertyError,
	#[error("Undefined property: {0}")]
	UndefinedProperty(&'static str),
	#[error("Cannot use this out of class")]
	WrongUseOfThis,
	#[error("Cannot use super out of class")]
	WrongUseOfSuper,
	#[error("No superclass")]
	NoSuperClass,
	#[error("Class cannot inherit self")]
	CannotInheritSelf,
	#[error("Superclass not exists")]
	SuperclassNotExist,
	#[error("Method not found in super class")]
	MethodNotFoundInSuperClass,
	#[error("{0}")]
	InternalError(#[from] anyhow::Error),
}
