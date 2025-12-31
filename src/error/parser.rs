/// Errors that can occur during parsing.
#[derive(thiserror::Error, Debug)]
pub enum ParserError {
	/// Internal compiler error, should never happen
	#[error("{0}")]
	InternalError(#[from] anyhow::Error),
	/// Errors encountered during parsing
	#[error(transparent)]
	ParseError(#[from] ParseError),
}

/// A specific parsing error with line number and type.
#[derive(thiserror::Error, Debug)]
#[error("line {line}: {type}")]
pub struct ParseError {
	/// The line number where the error occurred.
	line:   usize,
	/// The type of parsing error.
	r#type: ParseErrorType,
}

impl ParseError {
	pub fn new(line: usize, r#type: ParseErrorType) -> Self { Self { line, r#type } }
}

/// Types of parsing errors.
#[derive(Debug)]
pub enum ParseErrorType {
	/// Error for unterminated parenthesis.
	UnterminatedParenthesis,
	/// Error for unexpected tokens.
	UnexpectedToken(String),
	/// Error for missing semicolon at the end of a statement.
	ExpectSemicolon,
	/// Error for missing left parenthesis.
	ExpectLeftParen,
	/// Error for missing right parenthesis.
	ExpectRightParen,
	/// Error for missing right brace.
	ExpectRightBrace,
	/// Error for missing variable name in variable declaration.
	ExpectVariableName,
	/// Error for invalid assignment target.
	InvalidAssignmentTarget,
}

impl std::fmt::Display for ParseErrorType {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		use ParseErrorType::*;
		match self {
			UnterminatedParenthesis => write!(f, "Unterminated parenthesis"),
			UnexpectedToken(e) => write!(f, "Unexpected token: {e}"),
			ExpectSemicolon => write!(f, "Expect ';' at the end of statement."),
			ExpectVariableName => write!(f, "Expect variable name in declaration."),
			InvalidAssignmentTarget => write!(f, "Invalid assignment target."),
			ExpectRightBrace => write!(f, "Expect '}}' to close the block."),
			ExpectLeftParen => write!(f, "Expect '(' after keyword."),
			ExpectRightParen => write!(f, "Expect ')' after expression."),
		}
	}
}
