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
	#[error("{0}")]
	InternalError(#[from] anyhow::Error),
}
