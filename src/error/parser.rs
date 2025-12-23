#[derive(thiserror::Error, Debug)]
pub enum ParserError {
	#[error("{0}")]
	InternalError(#[from] anyhow::Error),
	#[error(transparent)]
	ParseError(#[from] ParseError),
}

#[derive(thiserror::Error, Debug)]
#[error("line {line}: {type}")]
pub struct ParseError {
	line:   usize,
	r#type: ParseErrorType,
}

impl ParseError {
	pub fn new(line: usize, r#type: ParseErrorType) -> Self { Self { line, r#type } }
}

#[derive(Debug)]
pub enum ParseErrorType {
	ExpectedExpression,
	UnterminatedParenthesis,
}

impl std::fmt::Display for ParseErrorType {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		use ParseErrorType::*;
		match self {
			ExpectedExpression => {
				write!(f, "Expected expression")
			}
			UnterminatedParenthesis => {
				write!(f, "Unterminated parenthesis")
			}
		}
	}
}