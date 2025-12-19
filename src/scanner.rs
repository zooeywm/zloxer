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
//!
//! We can’t easily detect a `reserved word` until we’ve reached the end of what
//! might instead be an identifier, this is `maximal munch`.

use std::{collections::HashMap, iter::Peekable, str::CharIndices};

use TokenType::*;
use anyhow::Context;

use crate::error::{LoxError::*, ScanError, ScanErrorType::*};

/// A scanner for Lox source code
pub struct Scanner<'a> {
	/// User input source code
	source:      &'a str,
	/// User input source code
	source_iter: Peekable<CharIndices<'a>>,
	/// Points at the beginning of the current lexeme
	start:       usize,
	/// Points at the character currently being considered
	cursor:      usize,
	/// Tracks what source line `current` is on so we can produce tokens that know
	/// their location.
	line:        usize,
	/// Lexed tokens
	tokens:      Vec<Token<'a>>,
	/// Reserved keywords in Lox
	keywords:    HashMap<&'a str, TokenType<'a>>,
}

/// A token produced by the scanner
#[derive(Debug)]
pub struct Token<'a> {
	r#type: TokenType<'a>,
	lexeme: &'a str,
	line:   usize,
}

impl<'a> Scanner<'a> {
	pub fn new(source: &'a str) -> Self {
		let keywords = HashMap::from([
			("and", And),
			("class", Class),
			("else", Else),
			("false", False),
			("for", For),
			("fun", Fun),
			("if", If),
			("nil", Nil),
			("or", Or),
			("print", Print),
			("return", Return),
			("super", Super),
			("this", This),
			("true", True),
			("var", Var),
			("while", While),
		]);
		let source_iter = source.char_indices().peekable();

		Self { source, source_iter, keywords, tokens: vec![], start: 0, cursor: 0, line: 1 }
	}

	/// Scan all tokens from the source code
	pub fn scan_tokens(&'a mut self) -> crate::Result<&'a [Token<'a>]> {
		let mut scan_errors = vec![];
		self.start = 0;
		self.cursor = 0;
		while let Some(&(index, _)) = self.source_iter.peek() {
			// We are at the beginning of the next lexeme.
			self.start = index;
			self.cursor = index;
			match self.scan_token() {
				Ok(_) => {}
				Err(ScanError(e)) => {
					scan_errors.push(e);
				}
				Err(CompileInternalError(e)) => {
					return Err(e.into());
				}
				Err(ScanErrors(e)) => return Err(anyhow::anyhow!("This shall never happen: {e:?}").into()),
			}
		}
		if !scan_errors.is_empty() {
			return Err(ScanErrors(scan_errors));
		}
		self.tokens.push(Token { r#type: Eof, lexeme: "", line: self.line });
		Ok(self.tokens.as_slice())
	}

	/// Scan a single token from the source code
	fn scan_token(&mut self) -> crate::Result<()> {
		let next_char = self.advance().context("Unexpected EOF")?;
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
			'!' => if self.match_next('=') { BangEqual } else { Bang },
			'=' => if self.match_next('=') { EqualEqual } else { Equal },
			'<' => if self.match_next('=') { LessEqual } else { Less },
			'>' => if self.match_next('=') { GreaterEqual } else { Greater },
            '/' => if self.match_next('/') {
                while self.peek().is_some_and(|c| c!= '\n') { self.advance(); }
                Comment
            } else if self.match_next('*') {
                let mut closed = false;
                while let Some(c) = self.peek() {
                    if c == '*' && { self.advance(); self.peek().is_some_and(|c| c == '/') } {
                        self.advance(); // consume '/'
                        closed = true;
                        break;
                    }
                    if c == '\n' { self.line += 1; }
                    self.advance();
                }
                if closed { Comment } else { return Err(ScanError::new(self.line, UnterminatedLineComment).into()) }
            } else { Slash },
            ' ' | '\r' | '\t' => EmptyChar,
            '\n'=> { self.line += 1; NewLine }
            '"' => self.string()?,
            c @ char if c.is_ascii_digit() => self.number()?,
            c @ char if c.is_ascii_alphabetic() || c == '_' => self.identifier(),
            _ => return Err(ScanError::new(self.line, UnexpectedCharacter(next_char)).into()),
		};

		if !r#type.is_ignored() {
			let lexeme = &self.source[self.start..self.cursor];
			self.tokens.push(Token { r#type, lexeme, line: self.line });
		}

		Ok(())
	}
}

impl<'a> Scanner<'a> {
	// /// Check if
	// fn is_at_end(&self) -> bool { self.current >= self.source.chars().count() }
	//
	/// Match the next character if it is the expected one
	fn match_next(&mut self, expected: char) -> bool {
		matches!(self.peek(), Some(c) if c == expected && { self.advance(); true })
	}

	/// Advance to the next character
	fn advance(&mut self) -> Option<char> {
		let (i, c) = self.source_iter.next()?;
		self.cursor = i + c.len_utf8();
		Some(c)
	}

	/// Peek the current character
	fn peek(&mut self) -> Option<char> { self.source_iter.peek().map(|&(_, c)| c) }

	/// Scan a string literal
	fn string(&mut self) -> crate::Result<TokenType<'a>> {
		while let Some(c) = self.peek() {
			if c == '"' {
				break;
			}
			if c == '\n' {
				self.line += 1
			}
			self.advance();
		}

		self.peek().ok_or(ScanError::new(self.line, UnterminatedString))?;
		self.advance(); // The closing "
		let value = &self.source[self.start + 1..self.cursor - 1];
		Ok(StringLiteral(value))
	}

	/// Scan a number literal
	fn number(&mut self) -> crate::Result<TokenType<'a>> {
		while self.peek().is_some_and(|c| c.is_ascii_digit()) {
			self.advance();
		}

		// Look for a fractional part.
		if self.peek() == Some('.') {
			let mut iter_clone = self.source_iter.clone();
			iter_clone.next();
			if iter_clone.peek().is_some_and(|(_, c)| c.is_ascii_digit()) {
				self.advance();
				while self.peek().is_some_and(|c| c.is_ascii_digit()) {
					self.advance();
				}
			}
		}
		let s = &self.source[self.start..self.cursor];
		Ok(NumberLiteral(s.parse().context("Failed to parse number literal")?))
	}

	/// Scan an identifier or keyword
	fn identifier(&mut self) -> TokenType<'a> {
		while self.peek().is_some_and(|c| c.is_ascii_alphanumeric() || c == '_') {
			self.advance();
		}
		let text = &self.source[self.start..self.cursor];
		self.keywords.get(text).cloned().unwrap_or(TokenType::Identifier(text))
	}
}

/// The different types of tokens in Lox, The copying is lightweight
#[derive(Debug, Clone)]
pub enum TokenType<'a> {
	Undefined,
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
	pub fn is_ignored(&self) -> bool { matches!(self, TokenType::EmptyChar | TokenType::NewLine) }
}
