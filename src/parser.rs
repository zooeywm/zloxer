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

use std::{iter::Peekable, vec::IntoIter};

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

		while let Some(operator) = self.match_tokens(&[TokenType::BangEqual, TokenType::EqualEqual])? {
			let left = expression;
			let right = self.comparison()?.boxed();
			expression = Expression::Binary { left, operator, right }.boxed();
		}

		Ok(expression)
	}

	fn comparison(&mut self) -> Result<Box<Expression<'a>>, ParserError> {
		let mut expression = self.term()?;

		while let Some(operator) = self.match_tokens(&[
			TokenType::Greater,
			TokenType::GreaterEqual,
			TokenType::Less,
			TokenType::LessEqual,
		])? {
			let left = expression;
			let right = self.term()?.boxed();
			expression = Expression::Binary { left, operator, right }.boxed();
		}

		Ok(expression)
	}

	fn term(&mut self) -> Result<Box<Expression<'a>>, ParserError> {
		let mut expression = self.factor()?;

		while let Some(operator) = self.match_tokens(&[TokenType::Minus, TokenType::Plus])? {
			let left = expression;
			let right = self.factor()?.boxed();
			expression = Expression::Binary { left, operator, right }.boxed();
		}

		Ok(expression)
	}

	fn factor(&mut self) -> Result<Box<Expression<'a>>, ParserError> {
		let mut expression = self.unary()?;

		while let Some(operator) = self.match_tokens(&[TokenType::Slash, TokenType::Star])? {
			let left = expression;
			let right = self.unary()?;
			expression = Expression::Binary { left, operator, right }.boxed();
		}

		Ok(expression)
	}

	fn unary(&mut self) -> Result<Box<Expression<'a>>, ParserError> {
		if let Some(operator) = self.match_tokens(&[TokenType::Bang, TokenType::Minus])? {
			let right = self.unary()?;
			return Ok(Expression::Unary { operator, right }.boxed());
		}

		self.primary()
	}

	/// 补全 primary
	fn primary(&mut self) -> Result<Box<Expression<'a>>, ParserError> {
		if self.match_tokens(&[TokenType::False])?.is_some() {
			return Ok(Expression::Literal(LiteralValue::Boolean(false)).boxed());
		}
		if self.match_tokens(&[TokenType::True])?.is_some() {
			return Ok(Expression::Literal(LiteralValue::Boolean(true)).boxed());
		}
		if self.match_tokens(&[TokenType::Nil])?.is_some() {
			return Ok(Expression::Literal(LiteralValue::Nil).boxed());
		}

		if let TokenType::NumberLiteral(n) = self.peek()?.r#type {
			self.advance()?;
			return Ok(Expression::Literal(LiteralValue::Number(n)).boxed());
		}

		if let TokenType::StringLiteral(s) = self.peek()?.r#type {
			self.advance()?;
			return Ok(Expression::Literal(LiteralValue::String(s)).boxed());
		}

		if self.match_tokens(&[TokenType::LeftParen])?.is_some() {
			let expr = self.expression()?;
			if self.match_tokens(&[TokenType::RightParen])?.is_none() {
				return Err(ParseError::new(self.peek()?.line, ParseErrorType::UnterminatedParenthesis).into());
			}
			return Ok(Expression::Grouping(expr).boxed());
		}
		let token = self.peek()?;
		Err(ParseError::new(token.line, ParseErrorType::ExpectedExpression).into())
	}

	fn match_tokens(&mut self, types: &[TokenType<'a>]) -> Result<Option<Token<'a>>, ParserError> {
		for token_type in types {
			if self.check(token_type)? {
				return Ok(Some(self.advance()?));
			}
		}
		Ok(None)
	}

	fn check(&mut self, token_type: &TokenType) -> Result<bool, ParserError> {
		match self.tokens.peek() {
			Some(token) => Ok(&token.r#type == token_type),
			None => Err(anyhow!("Unexpected EOF").into()),
		}
	}

	fn advance(&mut self) -> Result<Token<'a>, ParserError> {
		self.tokens.next().ok_or_else(|| anyhow!("Unexpected EOF").into())
	}

	fn peek(&mut self) -> Result<&Token<'a>, ParserError> {
		self.tokens.peek().ok_or_else(|| anyhow!("Unexpected EOF").into())
	}
}
