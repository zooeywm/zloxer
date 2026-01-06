pub mod interpreter;
pub mod parser;
pub mod scanner;

/// LoxError is the top-level error type for the Lox compiler/interpreter.
#[derive(thiserror::Error, Debug)]
pub enum LoxError {
	/// Internal compiler error, should never happen
	#[error("CompilerInternalError: {0}")]
	InternalError(#[from] anyhow::Error),
	/// Scanner errors encountered during scanning
	#[error("Generated {0} scanner errors")]
	ScannerErrors(usize),
	/// Parser errors encountered during parsing
	#[error("Generated {0} parser errors")]
	ParserErrors(usize),
	/// Runtime errors encountered during interpretation
	#[error("Runtime error:\n{0}")]
	RuntimeError(#[from] interpreter::InterpreterError),
}
