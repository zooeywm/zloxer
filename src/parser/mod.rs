//! The `Scanner` use `Lexical grammar`, implement `alphabet` as `Characters`,
//! `string` as `Lexme` or `Token`
//! The `Parser` use `Syntactic grammar`, implement `alphabet` as `Tokens`,
//! `string` as `Expression`
//!
//! A formal grammar’s job is to specify which strings are valid and which
//! aren’t. If we were defining a grammar for English sentences, “eggs are tasty
//! for breakfast” would be in the grammar, but “tasty breakfast for are eggs”
//! would probably not.
//!
//! Keywords: `CFG`(context-free grammar), `BNF`(Backus-Naur Form),
//! `EBNF`(Extended Backus-Naur Form)
//!
//! |Name|Operators|Associates
//! --|--|--
//! Comma|,|Left
//! Ternary|?:|Right
//! Equality|== !=|Left
//! Comparison|< > <= >=|Left
//! Term|+ -|Left
//! Factor|* /|Left
//! Unary|! -|Right
//!
//! Expression grammar:
//!
//! ``` BNF
//! expression     → comma ;
//! comma          → ternary ( "," ternary )* ;
//! ternary        → equality ( "?" expression ":" ternary )? ;
//! equality       → comparison ( ( "!=" | "==" ) comparison )* ;
//! comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
//! term           → factor ( ( "-" | "+" ) factor )* ;
//! factor         → unary ( ( "/" | "*" ) unary )* ;
//! unary          → ( "!" | "-" ) unary | primary ;
//! primary        → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
//! ```

mod expression;

use std::{convert::TryInto, iter::Peekable, vec::IntoIter};

use TokenType::*;
use anyhow::anyhow;

use crate::{LoxError, error::parser::{ParseError, ParseErrorType, ParserError}, parser::expression::Expression, scanner::{Token, TokenType}};

/// ParserError is the error type for the parser.
pub struct Parser<'a> {
	/// The tokens to parse.
	tokens:      Peekable<IntoIter<Token<'a>>>,
	error_count: usize,
}

impl<'a> Parser<'a> {
	pub fn new(tokens: Vec<Token<'a>>) -> Self {
		Self { tokens: tokens.into_iter().peekable(), error_count: 0 }
	}

	pub fn parse(&mut self) -> Result<Box<Expression<'a>>, LoxError> {
		match self.expression() {
			Ok(expr) => {
				if self.error_count > 0 {
					Err(LoxError::ParserErrors(self.error_count))
				} else {
					Ok(expr)
				}
			}
			Err(ParserError::InternalError(e)) => Err(e.into()),
			Err(ParserError::ParseError(_)) => Err(LoxError::ParserErrors(self.error_count)),
		}
	}

	/// Parse comma expressions.
	fn expression(&mut self) -> Result<Box<Expression<'a>>, ParserError> { self.comma() }

	fn comma(&mut self) -> Result<Box<Expression<'a>>, ParserError> {
		let mut expr = self.ternary()?;
		while matches!(self.peek()?.r#type, Comma) {
			self.advance()?;
			expr = Expression::comma(expr, self.ternary()?)
		}
		Ok(expr)
	}

	fn ternary(&mut self) -> Result<Box<Expression<'a>>, ParserError> {
		let condition = self.equality()?;
		if matches!(self.peek()?.r#type, Question) {
			self.advance()?;
			let then_branch = self.expression()?;
			if !matches!(self.peek()?.r#type, Colon) {
				return Err(
					ParseError::new(self.peek()?.line, ParseErrorType::UnexpectedToken(":".to_string())).into(),
				);
			}
			self.advance()?;
			let else_branch = self.ternary()?;
			return Ok(Expression::ternary(condition, then_branch, else_branch));
		}
		Ok(condition)
	}

	/// Parse equality expressions.
	fn equality(&mut self) -> Result<Box<Expression<'a>>, ParserError> {
		let mut expression = self.comparison()?;
		while matches!(self.peek()?.r#type, BangEqual | EqualEqual) {
			expression = Expression::binary(expression, self.advance()?, self.comparison()?)
		}
		Ok(expression)
	}

	/// Parse comparison expressions.
	fn comparison(&mut self) -> Result<Box<Expression<'a>>, ParserError> {
		let mut expression = self.term()?;
		while matches!(self.peek()?.r#type, Greater | GreaterEqual | Less | LessEqual) {
			expression = Expression::binary(expression, self.advance()?, self.comparison()?)
		}
		Ok(expression)
	}

	/// Parse term expressions.
	fn term(&mut self) -> Result<Box<Expression<'a>>, ParserError> {
		let mut expression = self.factor()?;
		while matches!(self.peek()?.r#type, Minus | Plus) {
			expression = Expression::binary(expression, self.advance()?, self.factor()?)
		}
		Ok(expression)
	}

	/// Parse factor expressions.
	fn factor(&mut self) -> Result<Box<Expression<'a>>, ParserError> {
		let mut expression = self.unary()?;
		while matches!(self.peek()?.r#type, Slash | Star) {
			expression = Expression::binary(expression, self.advance()?, self.unary()?)
		}
		Ok(expression)
	}

	/// Parse unary expressions.
	fn unary(&mut self) -> Result<Box<Expression<'a>>, ParserError> {
		if matches!(self.peek()?.r#type, Bang | Minus) {
			return Ok(Expression::unary(self.advance()?, self.unary()?));
		}
		self.primary()
	}

	/// Parse primary expressions.
	fn primary(&mut self) -> Result<Box<Expression<'a>>, ParserError> {
		let token = self.peek()?;
		match &token.r#type {
			False | True | Nil | NumberLiteral(_) | StringLiteral(_) => {
				let token = self.advance()?;
				return Ok(Box::new(token.try_into()?));
			}
			LeftParen => {
				self.advance()?; // consume '('
				let expr = self.expression()?;

				if !matches!(self.peek()?.r#type, RightParen) {
					return Err(ParseError::new(self.peek()?.line, ParseErrorType::UnterminatedParenthesis).into());
				}
				self.advance()?; // consume ')'

				Ok(Expression::grouping(expr))
			}
			_ => {
				let err_string = token.lexeme.to_string();
				let error = ParseError::new(self.peek()?.line, ParseErrorType::UnexpectedToken(err_string));
				self.report(&error);
				self.advance()?; // consume unexpected token
				Err(error.into())
			}
		}
	}

	/// Advance to the next token.
	fn advance(&mut self) -> Result<Token<'a>, ParserError> {
		self.tokens.next().ok_or_else(|| anyhow!("Unexpected EOF").into())
	}

	/// Peek at the current token.
	fn peek(&mut self) -> Result<&Token<'a>, ParserError> {
		self.tokens.peek().ok_or_else(|| anyhow!("Unexpected EOF").into())
	}

	fn report(&mut self, error: &ParseError) {
		self.error_count += 1;
		eprintln!("{error}");
	}

	fn synchronize(&mut self) -> Result<(), ParserError> {
		self.advance()?;
		while let Ok(token) = self.peek() {
			if matches!(token.r#type, Class | Fun | Var | For | If | While | Print | Return) {
				return Ok(());
			}
			self.advance()?;
		}
		Ok(())
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::scanner::Scanner;

	fn parse(input: &str, equals: &str) {
		let mut scanner = Scanner::new(input);
		let tokens = scanner.scan_tokens().unwrap();
		let mut parser = Parser::new(tokens);
		let ast = parser.parse().unwrap();
		assert_eq!(ast.to_string(), equals);
	}

	#[test]
	fn parse_expressions() {
		parse("3 + 4 * (-2 - 1)", "(+ 3 (* 4 (group (- (- 2) 1))))");
		parse("1 + 2 * 3 / 4 - 5", "(- (+ 1 (/ (* 2 3) 4)) 5)");
		parse("8 + 800.3 * 123 / 65 - (2 + 3)", "(- (+ 8 (/ (* 800.3 123) 65)) (group (+ 2 3)))");
	}

	#[test]
	fn parse_comparison() {
		parse("1 < 2", "(< 1 2)");
		parse("1 <= 2", "(<= 1 2)");
		parse("1 > 2", "(> 1 2)");
		parse("1 >= 2", "(>= 1 2)");
		parse("1 < 2 < 3", "(< 1 (< 2 3))");
	}

	#[test]
	fn parse_equality() {
		parse("1 == 2", "(== 1 2)");
		parse("1 != 2", "(!= 1 2)");
		parse("1 == 2 == 3", "(== (== 1 2) 3)");
		parse("1 != 2 == 3", "(== (!= 1 2) 3)");
	}

	#[test]
	fn parse_unary() {
		parse("-123", "(- 123)");
		parse("!true", "(! true)");
		parse("-(-123)", "(- (group (- 123)))");
		parse("!!true", "(! (! true))");
		parse("-1 + 2", "(+ (- 1) 2)");
	}

	#[test]
	fn parse_literals() {
		parse("42", "42");
		parse("3.14", "3.14");
		parse("\"hello\"", "\"hello\"");
		parse("true", "true");
		parse("false", "false");
		parse("nil", "nil");
	}

	#[test]
	fn parse_grouping() {
		parse("(1 + 2) * 3", "(* (group (+ 1 2)) 3)");
		parse("1 * (2 + 3)", "(* 1 (group (+ 2 3)))");
		parse("((1))", "(group (group 1))");
	}

	#[test]
	fn parse_complex() {
		parse("1 + 2 == 3", "(== (+ 1 2) 3)");
		parse("1 + 2 != 3 - 4", "(!= (+ 1 2) (- 3 4))");
		parse("!(1 < 2)", "(! (group (< 1 2)))");
		parse("-(1 + 2)", "(- (group (+ 1 2)))");
		parse("1 + 2 * 3 < 4 - 5 / 6", "(< (+ 1 (* 2 3)) (- 4 (/ 5 6)))");
	}

	#[test]
	fn parse_comma() {
		parse("1, 2", "(, 1 2)");
		parse("1, 2, 3", "(, (, 1 2) 3)");
		parse("1 + 2, 3 * 4", "(, (+ 1 2) (* 3 4))");
		parse("(1, 2), 3", "(, (group (, 1 2)) 3)");
	}

	#[test]
	fn parse_ternary() {
		parse("1 ? 2 : 3", "(? 1 : 2 3)");
		parse("1 == 2 ? 3 : 4", "(? (== 1 2) : 3 4)");
		parse("1 ? 2 ? 3 : 4 : 5", "(? 1 : (? 2 : 3 4) 5)");
		parse("1, 2 ? 3 : 4", "(, 1 (? 2 : 3 4))");
		parse("(1 ? 2 : 3) + 4", "(+ (group (? 1 : 2 3)) 4)");
	}
}
