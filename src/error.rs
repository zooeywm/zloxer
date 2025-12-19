pub type Result<T> = std::result::Result<T, LoxError>;

#[derive(thiserror::Error, Debug)]
pub enum LoxError {
	#[error("CompilerInternalError{0}")]
	CompileInternalError(#[from] anyhow::Error),
	#[error("ScanErrors:\n{}", display_scan_errors(.0))]
	ScanErrors(Vec<ScanError>),
	#[error("{0}")]
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
	UnterminatedLineComment,
	UnterminatedBlockComment,
	UnexpectedCharacter(char),
	UnterminatedString,
}

impl std::fmt::Display for ScanErrorType {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		use ScanErrorType::*;
		match self {
			UnterminatedLineComment => {
				write!(f, "Unterminated line comment")
			}
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

fn display_scan_errors(errors: &[ScanError]) -> String {
	errors.iter().map(|e| format!("{}", e)).collect::<Vec<String>>().join("\n")
}
