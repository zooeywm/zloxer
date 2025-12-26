/// Scanner related errors
#[derive(thiserror::Error, Debug)]
pub enum ScannerError {
	/// Internal compiler error, should never happen
	#[error("{0}")]
	InternalError(#[from] anyhow::Error),
	/// Errors encountered during scanning
	#[error(transparent)]
	ScanError(#[from] ScanError),
}

/// A specific scanning error with line number and type.
#[derive(thiserror::Error, Debug)]
#[error("line {line}: {type}")]
pub struct ScanError {
	/// The line number where the error occurred.
	line:   usize,
	/// The type of scanning error.
	r#type: ScanErrorType,
}

impl ScanError {
	pub fn new(line: usize, r#type: ScanErrorType) -> Self { Self { line, r#type } }
}

/// Types of scanning errors.
#[derive(Debug)]
pub enum ScanErrorType {
	/// Error for unterminated block comments.
	UnterminatedBlockComment,
	/// Error for unexpected characters.
	UnexpectedCharacter(char),
	/// Error for unterminated strings.
	UnterminatedString,
}

impl std::fmt::Display for ScanErrorType {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		use ScanErrorType::*;
		match self {
			UnterminatedBlockComment => {
				write!(f, "Unterminated block comment")
			}
			UnexpectedCharacter(c) => {
				write!(f, "Unexpected character '{c}'")
			}
			UnterminatedString => {
				write!(f, "Unterminated String")
			}
		}
	}
}
