#[derive(thiserror::Error, Debug)]
pub enum ScannerError {
	#[error("{0}")]
	InternalError(#[from] anyhow::Error),
	#[error(transparent)]
	ScanError(#[from] ScanError),
}

#[derive(thiserror::Error, Debug)]
#[error("line {line}: {type}")]
pub struct ScanError {
	line:   usize,
	r#type: ScanErrorType,
}

impl ScanError {
	pub fn new(line: usize, r#type: ScanErrorType) -> Self { Self { line, r#type } }
}

#[derive(Debug)]
pub enum ScanErrorType {
	UnterminatedBlockComment,
	UnexpectedCharacter(char),
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
