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
//! lexemes are called its `Lexical grammar`.
//!
//! We can’t easily detect a `reserved word` until we’ve reached the end of what
//! might instead be an identifier, this is `maximal munch`.
mod token;

use std::{iter::Peekable, str::CharIndices};

use TokenType::*;
use anyhow::Context;
pub(crate) use token::*;

use crate::{LoxError, ScanError, ScanErrorType, ScannerError};

/// A scanner for Lox source code
pub(crate) struct Scanner<'a> {
	/// User input source code
	source:      &'a str,
	/// User input source code iterator
	source_iter: Peekable<CharIndices<'a>>,
	/// Points at the beginning of the current lexeme
	start:       usize,
	/// Points at the character currently being considered
	cursor:      usize,
	/// Tracks what source line `current` is on so we can produce tokens that know
	/// their location.
	line:        usize,
}

impl<'a> Scanner<'a> {
	pub fn new(source: &'a str) -> Self {
		let source_iter = source.char_indices().peekable();

		Self { source, source_iter, start: 0, cursor: 0, line: 1 }
	}

	/// Scan all tokens from the source code
	pub fn scan_tokens(&'a mut self) -> Result<Vec<Token<'a>>, LoxError> {
		let mut tokens = Vec::new();
		let mut error_count = 0;
		while let Some(&(index, _)) = self.source_iter.peek() {
			// We are at the beginning of the next lexeme.
			self.start = index;
			self.cursor = self.start;
			match self.scan_token(&mut tokens) {
				Err(ScannerError::ScanError(e)) => {
					eprintln!("Scan error: {}", e);
					error_count += 1;
				}
				Err(ScannerError::InternalError(e)) => {
					return Err(e.into());
				}
				Ok(_) => {}
			}
		}
		if error_count != 0 {
			return Err(LoxError::ScannerErrors(error_count));
		}
		tokens.push(Token::new(Eof, "", self.line));
		Ok(tokens)
	}

	/// Scan a single token from the source code
	fn scan_token(&mut self, tokens: &mut Vec<Token<'a>>) -> Result<(), ScannerError> {
		let next_char = self.advance().context("Unexpected EOF")?;
		#[rustfmt::skip]
		let r#type = match next_char {
			'(' => LeftParen,
			')' => RightParen,
			'{' => LeftBrace,
			'}' => RightBrace,
			',' => Comma,
			'?' => Question,
			':' => Colon,
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
                    if c == '*' && self.peek_second().is_some_and(|c| c == '/') {
                        self.advance(); // consume '*'
                        self.advance(); // consume '/'
                        closed = true;
                        break;
                    }
                    if c == '\n' { self.line += 1; }
                    self.advance();
                }
                if closed { Comment } else { return Err(ScanError::new(self.line, ScanErrorType::UnterminatedBlockComment).into()) }
            } else { Slash },
            ' ' | '\r' | '\t' => EmptyChar,
            '\n'=> { self.line += 1; NewLine }
            '"' => self.string()?,
            c if c.is_ascii_digit() => self.number()?,
            c if c.is_ascii_alphabetic() || c == '_' => self.identifier(),
            _ => return Err(ScanError::new(self.line, ScanErrorType::UnexpectedCharacter(next_char)).into()),
		};

		if !r#type.is_ignored() {
			let lexeme = &self.source[self.start..self.cursor];
			tokens.push(Token::new(r#type, lexeme, self.line));
		}

		Ok(())
	}

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
	fn string(&mut self) -> Result<TokenType<'a>, ScannerError> {
		while let Some(c) = self.peek() {
			if c == '"' {
				break;
			}
			if c == '\n' {
				self.line += 1
			}
			self.advance();
		}

		self.peek().ok_or_else(|| ScanError::new(self.line, ScanErrorType::UnterminatedString))?;
		self.advance(); // The closing "
		let value = &self.source[self.start + 1..self.cursor - 1];
		Ok(String(value))
	}

	/// Scan a number literal
	fn number(&mut self) -> Result<TokenType<'a>, ScannerError> {
		while self.peek().is_some_and(|c| c.is_ascii_digit()) {
			self.advance();
		}

		// Look for a fractional part.
		if self.peek() == Some('.') && self.peek_second().is_some_and(|c| c.is_ascii_digit()) {
			self.advance(); // consume '.'
			while self.peek().is_some_and(|c| c.is_ascii_digit()) {
				self.advance();
			}
		}

		let s = &self.source[self.start..self.cursor];
		Ok(Number(s.parse().context("Failed to parse number literal")?))
	}

	/// Peek the second character ahead
	fn peek_second(&mut self) -> Option<char> {
		let mut it = self.source_iter.clone();
		it.next()?; // 跳过当前 peek 的字符
		it.peek().map(|&(_, c)| c)
	}

	/// Scan an identifier or keyword
	fn identifier(&mut self) -> TokenType<'a> {
		while self.peek().is_some_and(|c| c.is_ascii_alphanumeric() || c == '_') {
			self.advance();
		}
		let text = &self.source[self.start..self.cursor];
		TokenType::keyword_or_identifier(text)
	}
}

#[cfg(test)]
mod tests {
	use std::f64::consts::PI;

	use super::*;

	fn scan(input: &str, ok: bool) {
		let mut scanner = Scanner::new(input);
		let result = scanner.scan_tokens();
		assert!(result.is_ok() == ok);
	}

	#[test]
	fn scan_tokens() {
		scan("", true);
		scan("(", true);
		scan("(){}", true);
		scan(" ( ) ", true);
		scan("@", false);
		scan("你好", false);
		scan(r#""世界""#, true);
		scan("12345", true);
		scan(
			r#""/* Block
        注释📻
        */""#,
			true,
		);
		scan(
			r#"
            Multi
            Line
                String
            "#,
			true,
		);
		scan(r#"// Comment"#, true);
		scan("/* Unterminated comment ", false);
		scan("user", true);
		scan("return", true);
	}

	#[test]
	fn scan_operators() {
		scan("!", true);
		scan("!=", true);
		scan("=", true);
		scan("==", true);
		scan("<", true);
		scan("<=", true);
		scan(">", true);
		scan(">=", true);
		scan("-", true);
		scan("+", true);
		scan("*", true);
		scan("/", true);
		scan(";", true);
		scan(",", true);
		scan(".", true);
	}

	#[test]
	fn scan_numbers() {
		scan("0", true);
		scan("42", true);
		scan("3.14", true);
		scan("0.5", true);
		scan("123.456", true);
		scan("1.", true);
		scan(".5", true);
	}

	#[test]
	fn scan_strings() {
		scan(r#""""#, true);
		scan(r#""hello""#, true);
		scan(r#""hello world""#, true);
		scan(r#""""#, true);
		scan(r#""escaped\n\"quote\"""#, false);
		// scan( r#""unterminated string"#, false);
	}

	#[test]
	fn scan_keywords() {
		scan("and", true);
		scan("class", true);
		scan("else", true);
		scan("false", true);
		scan("for", true);
		scan("fun", true);
		scan("if", true);
		scan("nil", true);
		scan("or", true);
		scan("print", true);
		scan("return", true);
		scan("super", true);
		scan("this", true);
		scan("true", true);
		scan("var", true);
		scan("while", true);
	}

	#[test]
	fn scan_identifiers() {
		scan("x", true);
		scan("_name", true);
		scan("myVariable123", true);
		scan("snake_case", true);
		scan("CamelCase", true);
		scan("and123", true);
	}

	#[test]
	fn scan_comments() {
		scan("// single line comment", true);
		scan("// comment with ()[]{}", true);
		scan("/* block comment */", true);
		scan("/* multi\nline\ncomment */", true);
		scan("/** nested ** comment **/", true);
		scan("/** multi ** comment ***********/", true);
		scan("/* unterminated", false);
	}

	#[test]
	fn scan_whitespace() {
		scan(" ", true);
		scan("\t", true);
		scan("\r", true);
		scan("\n", true);
		scan("  \t\r\n  ", true);
	}

	#[test]
	fn scan_combined() {
		scan("1 + 2 * 3", true);
		scan("var x = 42;", true);
		scan(r#"print("hello");"#, true);
		scan("if (x < 10) { x = x + 1; }", true);
	}

	#[test]
	fn scan_multiple_tokens() {
		let mut scanner = Scanner::new("1 + 2");
		let tokens = scanner.scan_tokens().unwrap();
		assert_eq!(tokens.len(), 4);
		assert_eq!(tokens[0].r#type, Number(1.0));
		assert_eq!(tokens[1].r#type, Plus);
		assert_eq!(tokens[2].r#type, Number(2.0));
		assert_eq!(tokens[3].r#type, Eof);
	}

	#[test]
	fn scan_string_with_newlines() {
		let mut scanner = Scanner::new(
			r#""hello
world""#,
		);
		let tokens = scanner.scan_tokens().unwrap();
		assert_eq!(tokens[0].r#type, String("hello\nworld"));
	}

	#[test]
	fn scan_number_precision() {
		let mut scanner = Scanner::new("3.14159265358979323846264338327950288");
		let tokens = scanner.scan_tokens().unwrap();
		assert_eq!(tokens[0].r#type, Number(PI));
	}
}
