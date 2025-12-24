pub mod parser;
pub mod scanner;

/// LoxError is the top-level error type for the Lox compiler/interpreter.
#[derive(thiserror::Error, Debug)]
pub enum LoxError {
	/// Internal compiler error, should never happen
	#[error("CompilerInternalError: {0}")]
	InternalError(#[from] anyhow::Error),
	#[error("Genetraed {0} scanner errors")]
	ScannerErrors(usize),
	#[error("Genetraed {0} parser errors")]
	ParserErrors(usize),
}
