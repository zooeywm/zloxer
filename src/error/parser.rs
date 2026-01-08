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
	/// Error for unexpected tokens.
	UnexpectedToken(String),
	/// Error for missing colon at the end of a statement.
	ExpectColon,
	/// Error for missing semicolon at the end of a statement.
	ExpectSemicolon,
	/// Error for missing left parenthesis.
	ExpectLeftParen,
	/// Error for missing right parenthesis.
	ExpectRightParen,
	/// Error for missing left brace.
	ExpectLeftBrace,
	/// Error for missing right brace.
	ExpectRightBrace,
	/// Error for missing variable name in variable declaration.
	ExpectVariableName,
	/// Error for too many arguments in function call.
	TooManyArguments,
	/// Error for too many parameters in function declaration.
	TooManyParameters,
	/// Error for invalid assignment target.
	InvalidAssignmentTarget,
	/// Error for missing property name in instance member get.
	ExpectPropertyName,
	/// Error for missing class name in class define.
	ExpectClassName,
	/// Error for missing super class name in class define.
	ExpectSuperClassName,
	/// Error for missing function name in function_call.
	ExpectFunctionNameOnCall,
}

impl std::fmt::Display for ParseErrorType {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		use ParseErrorType::*;
		match self {
			UnexpectedToken(e) => write!(f, "Unexpected token: {e}"),
			ExpectColon => write!(f, "Expect ':' at the end of statement."),
			ExpectSemicolon => write!(f, "Expect ';' at the end of statement."),
			ExpectVariableName => write!(f, "Expect variable name in declaration."),
			InvalidAssignmentTarget => write!(f, "Invalid assignment target."),
			ExpectLeftBrace => write!(f, "Expect '{{' to close the block."),
			ExpectRightBrace => write!(f, "Expect '}}' to close the block."),
			ExpectLeftParen => write!(f, "Expect '(' after keyword."),
			ExpectRightParen => write!(f, "Expect ')' after expression."),
			TooManyArguments => write!(f, "Cannot have more than 255 arguments."),
			TooManyParameters => write!(f, "Cannot have more than 255 parameters."),
			ExpectPropertyName => write!(f, "Expect property name after '.'."),
			ExpectClassName => write!(f, "Expect class name."),
			ExpectSuperClassName => write!(f, "Expect superclass name."),
			ExpectFunctionNameOnCall => write!(f, "Expect function name."),
		}
	}
}
