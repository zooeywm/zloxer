pub mod scanner;

use crate::{ScanError, ScannerError};

pub type LoxResult<T> = std::result::Result<T, LoxError>;
pub type ScannerResult<T> = std::result::Result<T, ScannerError>;

#[derive(thiserror::Error, Debug)]
pub enum LoxError {
	#[error("CompilerInternalError{0}")]
	InternalError(#[from] anyhow::Error),
	#[error("ScannerErrors:\n{}", display_scan_errors(.0))]
	ScannerErrors(Vec<ScanError>),
}

fn display_scan_errors(errors: &[ScanError]) -> String {
	errors.iter().map(|e| format!("{}", e)).collect::<Vec<String>>().join("\n")
}
