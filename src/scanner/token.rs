/// A token produced by the scanner
#[derive(Debug, Clone)]
pub(crate) struct Token<'a> {
	pub r#type: TokenType<'a>,
	pub lexeme: &'a str,
	pub line:   usize,
}

impl<'a> Token<'a> {
	pub fn new(r#type: TokenType<'a>, lexeme: &'a str, line: usize) -> Self { Self { r#type, lexeme, line } }
}

/// The different types of tokens in Lox, The copying is lightweight
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TokenType<'a> {
	/// New Line Character `\n`.
	NewLine,
	/// Empty Character: ` `, `\r`, `\t`.
	EmptyChar,
	/// Comment `//` or /* ... */
	Comment,
	/// Left parenthesis `(`.
	LeftParen,
	/// Right parenthesis `)`.
	RightParen,
	/// Left brace `{`.
	LeftBrace,
	/// Right brace `}`.
	RightBrace,
	/// Comma `,`.
	Comma,
	/// Question mark `?`.
	Question,
	/// Colon `:`.
	Colon,
	/// Dot `.`.
	Dot,
	/// Minus `-`.
	Minus,
	/// Plus `+`.
	Plus,
	/// Semicolon `;`.
	Semicolon,
	/// Slash `/`.
	Slash,
	/// Asterisk `*`.
	Star,
	/// Bang `!`.
	Bang,
	/// Bang equal `!=`.
	BangEqual,
	/// Equal `=`.
	Equal,
	/// Equal equal `==`.
	EqualEqual,
	/// Greater than `>`.
	Greater,
	/// Greater than or equal `>=`.
	GreaterEqual,
	/// Less than `<`.
	Less,
	/// Less than or equal `<=`.
	LessEqual,
	/// Identifier, e.g. variable or function name.
	Identifier(&'a str),
	/// String literal, e.g. `"hello"`.
	StringLiteral(&'a str),
	/// Number literal, e.g. `123.45`.
	NumberLiteral(f64),
	/// Logical AND keyword.
	And,
	/// Class keyword.
	Class,
	/// Else keyword.
	Else,
	/// Boolean literal `false`.
	False,
	/// Function keyword.
	Fun,
	/// For loop keyword.
	For,
	/// If statement keyword.
	If,
	/// Nil literal (null equivalent).
	Nil,
	/// Logical OR keyword.
	Or,
	/// Print statement keyword.
	Print,
	/// Return statement keyword.
	Return,
	/// Super keyword (for inheritance).
	Super,
	/// This keyword (current instance reference).
	This,
	/// Boolean literal `true`.
	True,
	/// Variable declaration keyword.
	Var,
	/// While loop keyword.
	While,
	/// End of file/input.
	Eof,
}

impl<'a> TokenType<'a> {
	pub fn is_ignored(&self) -> bool {
		matches!(self, TokenType::EmptyChar | TokenType::NewLine | TokenType::Comment)
	}

	pub fn keyword_or_identifier(value: &'a str) -> Self {
		match value {
			"and" => TokenType::And,
			"class" => TokenType::Class,
			"else" => TokenType::Else,
			"false" => TokenType::False,
			"for" => TokenType::For,
			"fun" => TokenType::Fun,
			"if" => TokenType::If,
			"nil" => TokenType::Nil,
			"or" => TokenType::Or,
			"print" => TokenType::Print,
			"return" => TokenType::Return,
			"super" => TokenType::Super,
			"this" => TokenType::This,
			"true" => TokenType::True,
			"var" => TokenType::Var,
			"while" => TokenType::While,
			_ => TokenType::Identifier(value),
		}
	}
}
