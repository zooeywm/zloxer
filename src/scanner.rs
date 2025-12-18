//! Represents a lexical token in the Lox language.
//!
//! Keywords are part of the shape of the language’s grammar, so the parser
//! often has code like, “If the next token is `while` then do . . . ” That
//! means the parser wants to know not just that it has a lexeme for some
//! identifier, but that it has a reserved word, and which keyword it is.
//!
//! The parser could categorize tokens from the raw lexeme by comparing the
//! strings, but that’s slow and kind of ugly. Instead, at the point that we
//! recognize a lexeme, we also remember which kind of lexeme it represents. We
//! have a different type for each keyword, operator, bit of punctuation, and
//! literal type.
//!
//! There are lexemes for literal values—numbers and strings and the like. Since
//! the scanner has to walk each character in the literal to correctly identify
//! it, it can also convert that textual representation of a value to the living
//! runtime object that will be used by the interpreter later.
//!
//! The rules that determine how a particular language groups characters into
//! lexemes are called its `lexical grammar`.

use TokenType::*;
use anyhow::Context;

#[derive(Default)]
pub struct Scanner<'a> {
	/// User input source code
	source:  &'a str,
	/// Lexed tokens
	tokens:  Vec<Token<'a>>,
	/// Points to the first character in the lexeme being scanned
	start:   usize,
	/// Points at the character currently being considered
	current: usize,
	/// Tracks what source line `current` is on so we can produce tokens that know
	/// their location.
	line:    usize,
}

#[derive(Debug, Default)]
pub struct Token<'a> {
	r#type: TokenType,
	lexeme: &'a str,
	line:   usize,
}

impl<'a> Scanner<'a> {
	pub fn new(source: &'a str) -> Self { Self { source, ..Default::default() } }

	pub fn scan_tokens(&'a mut self) -> crate::Result<&'a [Token<'a>]> {
		while !self.is_at_end() {
			// We are at the beginning of the next lexeme.
			self.start = self.current;
			self.scan_token()?;
		}
		self.tokens.push(Token { r#type: Eof, lexeme: "", line: self.line });
		Ok(self.tokens.as_slice())
	}

	fn is_at_end(&self) -> bool { self.current >= self.source.len() }

	fn scan_token(&mut self) -> crate::Result<()> {
		let next_char = self.source.chars().nth(self.current).context("Scan out of bound")?;
		self.current += 1;
		let r#type = TokenType::try_from(next_char)?;
		let lexeme =
			self.source.get(self.start..self.current).context("slice indices must be on char boundaries")?;
		let line = self.line;
		self.tokens.push(Token { r#type, lexeme, line });
		Ok(())
	}
}

#[derive(Debug, Default)]
pub enum TokenType {
	#[default]
	Unknown,
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
	Identifier(String),
	/// String literal, e.g. `"hello"`.
	StringLiteral(String),
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

impl TryFrom<char> for TokenType {
	type Error = anyhow::Error;

	fn try_from(value: char) -> Result<Self, Self::Error> {
		Ok(match value {
			'(' => LeftParen,
			')' => RightParen,
			'{' => LeftBrace,
			'}' => RightBrace,
			',' => Comma,
			'.' => Dot,
			'-' => Minus,
			'+' => Plus,
			';' => Semicolon,
			'/' => Slash,
			'*' => Star,
			_ => anyhow::bail!("Unsupported character '{value}' for TokenType conversion"),
		})
	}
}
