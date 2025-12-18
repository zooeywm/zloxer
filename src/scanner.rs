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

use crate::error::ScanErrors;

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

#[derive(Debug)]
pub struct Token<'a> {
	r#type: TokenType<'a>,
	lexeme: &'a str,
	line:   usize,
}

impl<'a> Scanner<'a> {
	pub fn new(source: &'a str) -> Self { Self { source, ..Default::default() } }

	pub fn scan_tokens(&'a mut self) -> crate::Result<&'a [Token<'a>]> {
		let mut scan_errors = ScanErrors::new();
		while !self.is_at_end() {
			// We are at the beginning of the next lexeme.
			self.start = self.current;
			if let Err(e) = self.scan_token() {
				scan_errors.push(self.line, e.to_string());
			}
		}
		if !scan_errors.is_empty() {
			return Err(scan_errors.into());
		}
		self.tokens.push(Token { r#type: Eof, lexeme: "", line: self.line });
		Ok(self.tokens.as_slice())
	}

	fn is_at_end(&self) -> bool { self.current >= self.source.len() }

	fn scan_token(&mut self) -> crate::Result<()> {
		let next_char = self.advance()?;
		#[rustfmt::skip]
		let r#type = match next_char {
			'(' => LeftParen,
			')' => RightParen,
			'{' => LeftBrace,
			'}' => RightBrace,
			',' => Comma,
			'.' => Dot,
			'-' => Minus,
			'+' => Plus,
			';' => Semicolon,
			'*' => Star,
			'!' => if self.match_next('=')? { BangEqual } else { Bang },
			'=' => if self.match_next('=')? { EqualEqual } else { Equal },
			'<' => if self.match_next('=')? { LessEqual } else { Less },
			'>' => if self.match_next('=')? { GreaterEqual } else { Greater },
            '/' => if self.match_next('/')? {
                        while self.peek()? != '\n' && !self.is_at_end() { self.advance()?; }
                        Comment
                    } else { Slash },
            ' ' => EmptyChar,
            '\r'=> EmptyChar,
            '\t'=> EmptyChar,
            '\n'=> { self.line += 1; NewLine }
            '"' => StringLiteral(self.string()?),
			_ => {
				return Err(format!("Unsupported character '{next_char}' for TokenType conversion").into());
			}
		};
		let lexeme =
			self.source.get(self.start..self.current).context("slice indices must be on char boundaries")?;
		let line = self.line;
		if !r#type.is_ignored() {
			self.tokens.push(Token { r#type, lexeme, line });
		}
		Ok(())
	}

	fn match_next(&mut self, expected: char) -> crate::Result<bool> {
		if self.is_at_end() {
			return Ok(false);
		}
		if self.source.chars().nth(self.current).context("Scan out of bound")? != expected {
			return Ok(false);
		}
		self.current += 1;
		Ok(true)
	}

	fn advance(&mut self) -> crate::Result<char> {
		let ch = self.source.chars().nth(self.current).context("Scan out of bound")?;
		self.current += 1;
		Ok(ch)
	}

	fn peek(&self) -> crate::Result<char> {
		if self.is_at_end() {
			return Ok('\0');
		}
		Ok(self.source.chars().nth(self.current).context("Scan out of bound")?)
	}

	fn string(&mut self) -> crate::Result<&'a str> {
		while self.peek()? != '"' && !self.is_at_end() {
			if self.peek()? == '\n' {
				self.line += 1
			}
			self.advance()?;
		}

		if self.is_at_end() {
			return Err("Unterminated String".to_string().into());
		}

		self.advance()?; // The closing "

		let ret = self
			.source
			.get(self.start + 1..self.current - 1)
			.context("Slice indices must be on char boundaries")?;
		Ok(ret)
	}
}

#[derive(Debug)]
pub enum TokenType<'a> {
	/// New Line Character `\n`.
	NewLine,
	/// Empty Character: ` `, `\r`, `\t`.
	EmptyChar,
	/// Comment `//`
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

impl TokenType<'_> {
	pub fn is_ignored(&self) -> bool {
		matches!(self, TokenType::EmptyChar | TokenType::NewLine | TokenType::Comment)
	}
}
