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

use crate::{parser::expression::Expression, scanner::{Token, TokenType}};

pub struct Parser<'a> {
	tokens:  Vec<Token<'a>>,
	current: usize,
}

impl<'a> Parser<'a> {
	pub fn new(tokens: Vec<Token<'a>>) -> Self { Self { tokens, current: 0 } }

	fn expression(&mut self) -> Expression<'a> { self.equality() }

	fn equality(&mut self) -> Expression<'a> {
		let mut expression = self.comparison();

		while self.match_tokens(&[TokenType::BangEqual, TokenType::EqualEqual]) {
			let operator = self.previous().clone();
			let right = self.comparison();
			expression = Expression::Binary { left: Box::new(expression), operator, right: Box::new(right) };
		}

		expression
	}

	fn comparison(&mut self) -> Expression<'a> {
		let mut expression = self.term();

		while self.match_tokens(&[
			TokenType::Greater,
			TokenType::GreaterEqual,
			TokenType::Less,
			TokenType::LessEqual,
		]) {
			let operator = self.previous().clone();
			let right = self.term();
			expression = Expression::Binary { left: Box::new(expression), operator, right: Box::new(right) };
		}

		expression
	}

	fn term(&mut self) -> Expression<'a> {
		let mut expression = self.factor();

		while self.match_tokens(&[TokenType::Minus, TokenType::Plus]) {
			let operator = self.previous().clone();
			let right = self.factor();
			expression = Expression::Binary { left: Box::new(expression), operator, right: Box::new(right) };
		}

		expression
	}

	fn factor(&mut self) -> Expression<'a> {
		let mut expression = self.unary();

		while self.match_tokens(&[TokenType::Slash, TokenType::Star]) {
			let operator = self.previous().clone();
			let right = self.unary();
			expression = Expression::Binary { left: Box::new(expression), operator, right: Box::new(right) };
		}

		expression
	}

	fn unary(&mut self) -> Expression<'a> {
		if self.match_tokens(&[TokenType::Bang, TokenType::Minus]) {
			let operator = self.previous().clone();
			let right = self.unary();
			return Expression::Unary { operator, right: Box::new(right) };
		}

		self.primary()
	}

	/// 补全 primary
	fn primary(&mut self) -> Expression<'a> {
		if self.match_tokens(&[TokenType::False]) {
			return Expression::Literal(crate::parser::expression::LiteralValue::Boolean(false));
		}
		if self.match_tokens(&[TokenType::True]) {
			return Expression::Literal(crate::parser::expression::LiteralValue::Boolean(true));
		}
		if self.match_tokens(&[TokenType::Nil]) {
			return Expression::Literal(crate::parser::expression::LiteralValue::Nil);
		}

		if let TokenType::NumberLiteral(n) = self.peek().r#type {
			self.advance();
			return Expression::Literal(crate::parser::expression::LiteralValue::Number(n));
		}

		if let TokenType::StringLiteral(s) = self.peek().r#type {
			self.advance();
			return Expression::Literal(crate::parser::expression::LiteralValue::String(s));
		}

		if self.match_tokens(&[TokenType::LeftParen]) {
			let expr = self.expression();
			self.match_tokens(&[TokenType::RightParen]);
			return Expression::Grouping { expression: Box::new(expr) };
		}
		panic!("Expected expression.");
	}

	fn match_tokens(&mut self, types: &[TokenType<'a>]) -> bool {
		for token_type in types {
			if self.check(token_type) {
				self.advance();
				return true;
			}
		}
		false
	}

	fn check(&self, token_type: &TokenType) -> bool {
		if self.is_at_end() {
			return false;
		}

		&self.peek().r#type == token_type
	}

	fn advance(&mut self) -> &Token<'a> {
		if !self.is_at_end() {
			self.current += 1;
		}
		self.previous()
	}

	fn is_at_end(&self) -> bool { self.peek().r#type == TokenType::Eof }

	fn peek(&self) -> &Token<'a> { &self.tokens[self.current] }

	fn previous(&self) -> &Token<'a> { &self.tokens[self.current - 1] }
}
