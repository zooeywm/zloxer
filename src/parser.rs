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
//! Equality|== ! =|Left
//! Comparison|< > <= >=|Left
//! Term|+ -|Left
//! Factor|* /|Left
//! Unary|! -|Right
//!
//! Expression grammar:
//!
//! ``` BNF
//! expression     → equality ;
//! equality       → comparison ( ( "!=" | "==" ) comparison )* ;
//! comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
//! term           → factor ( ( "-" | "+" ) factor )* ;
//! factor         → unary ( ( "/" | "*" ) unary )* ;
//! unary          → ( "!" | "-" ) unary | primary ;
//! primary        → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
//! ```

mod expression;

use std::{convert::TryInto, iter::Peekable, vec::IntoIter};

use anyhow::anyhow;

use crate::{error::parser::{ParseError, ParseErrorType, ParserError}, parser::expression::{Expression, LiteralValue}, scanner::{Token, TokenType}};

pub struct Parser<'a> {
	tokens: Peekable<IntoIter<Token<'a>>>,
}

impl<'a> Parser<'a> {
	pub fn new(tokens: Vec<Token<'a>>) -> Self { Self { tokens: tokens.into_iter().peekable() } }

	fn expression(&mut self) -> Result<Box<Expression<'a>>, ParserError> { self.equality() }

	fn equality(&mut self) -> Result<Box<Expression<'a>>, ParserError> {
		let mut expression = self.comparison()?;

		while matches!(self.peek()?.r#type, TokenType::BangEqual | TokenType::EqualEqual) {
			let operator = self.advance()?;
			let left = expression;
			let right = self.comparison()?.boxed();
			expression = Expression::Binary { left, operator, right }.boxed();
		}

		Ok(expression)
	}

	fn comparison(&mut self) -> Result<Box<Expression<'a>>, ParserError> {
		let mut expression = self.term()?;

		while matches!(
			self.peek()?.r#type,
			TokenType::Greater | TokenType::GreaterEqual | TokenType::Less | TokenType::LessEqual
		) {
			let operator = self.advance()?;
			let left = expression;
			let right = self.term()?.boxed();
			expression = Expression::Binary { left, operator, right }.boxed();
		}

		Ok(expression)
	}

	fn term(&mut self) -> Result<Box<Expression<'a>>, ParserError> {
		let mut expression = self.factor()?;

		while matches!(self.peek()?.r#type, TokenType::Minus | TokenType::Plus) {
			let operator = self.advance()?;
			let left = expression;
			let right = self.factor()?.boxed();
			expression = Expression::Binary { left, operator, right }.boxed();
		}

		Ok(expression)
	}

	fn factor(&mut self) -> Result<Box<Expression<'a>>, ParserError> {
		let mut expression = self.unary()?;

		while matches!(self.peek()?.r#type, TokenType::Slash | TokenType::Star) {
			let operator = self.advance()?;
			let left = expression;
			let right = self.unary()?;
			expression = Expression::Binary { left, operator, right }.boxed();
		}

		Ok(expression)
	}

	fn unary(&mut self) -> Result<Box<Expression<'a>>, ParserError> {
		if matches!(self.peek()?.r#type, TokenType::Bang | TokenType::Minus) {
			let operator = self.advance()?;
			let right = self.unary()?;
			return Ok(Expression::Unary { operator, right }.boxed());
		}
		self.primary()
	}

	fn primary(&mut self) -> Result<Box<Expression<'a>>, ParserError> {
		let token = self.peek()?;
		match token.r#type {
			TokenType::False
			| TokenType::True
			| TokenType::Nil
			| TokenType::NumberLiteral(_)
			| TokenType::StringLiteral(_) => {
				let token = self.advance()?;
				return Ok(Expression::Literal(token.try_into()?).boxed());
			}
			TokenType::LeftParen => {
				self.advance()?; // consume '('
				let expr = self.expression()?;

				if !matches!(self.peek()?.r#type, TokenType::RightParen) {
					return Err(ParseError::new(self.peek()?.line, ParseErrorType::UnterminatedParenthesis).into());
				}
				self.advance()?; // consume ')'

				Ok(Expression::Grouping(expr).boxed())
			}
			_ => Err(ParseError::new(token.line, ParseErrorType::ExpectedExpression).into()),
		}
	}

	fn advance(&mut self) -> Result<Token<'a>, ParserError> {
		self.tokens.next().ok_or_else(|| anyhow!("Unexpected EOF").into())
	}

	fn peek(&mut self) -> Result<&Token<'a>, ParserError> {
		self.tokens.peek().ok_or_else(|| anyhow!("Unexpected EOF").into())
	}
}
