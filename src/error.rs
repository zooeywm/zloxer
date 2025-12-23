pub mod parser;
pub mod scanner;

use crate::{ScanError, error::parser::ParserError};

#[derive(thiserror::Error, Debug)]
pub enum LoxError {
	#[error("CompilerInternalError{0}")]
	InternalError(#[from] anyhow::Error),
	#[error("ScannerErrors:\n{}", display_scan_errors(.0))]
	ScannerErrors(Vec<ScanError>),
	#[error("{0}")]
	ParseError(#[from] ParserError),
}

fn display_scan_errors(errors: &[ScanError]) -> String {
	errors.iter().map(|e| format!("{}", e)).collect::<Vec<String>>().join("\n")
}
