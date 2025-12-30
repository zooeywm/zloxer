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
//! Lox Expression grammar:
//!
//! ``` BNF
//! program        -> declaration* EOF ;
//! declaration    -> varDecl | statement ;
//! varDecl        -> "var" IDENTIFIER ( "=" expression )? ";" ;
//! statement      -> exprStmt | printStmt | block ;
//! block          → "{" declaration* "}" ;
//! exprStmt       -> expression ";" ;
//! printStmt      -> "print" expression ";" ;
//! expression     -> comma ;
//! comma          -> assignment ( "," assignment )* ;
//! assignment     -> IDENTIFIER "=" assignment | ternary ;
//! ternary        -> equality ( "?" expression ":" ternary )? ;
//! equality       -> comparison ( ( "!=" | "==" ) comparison )* ;
//! comparison     -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
//! term           -> factor ( ( "-" | "+" ) factor )* ;
//! factor         -> unary ( ( "/" | "*" ) unary )* ;
//! unary          -> ( "!" | "-" ) unary | primary ;
//! primary        -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" | IDENTIFIER ;
//! ```

pub(crate) mod expression;

use std::{convert::TryInto, iter::Peekable, vec::IntoIter};

use TokenType::*;
use anyhow::anyhow;

use crate::{LoxError, error::parser::{ParseError, ParseErrorType, ParserError}, parser::expression::Expression, scanner::{Token, TokenType}, statement::Statement};

/// ParserError is the error type for the parser.
pub struct Parser {
	/// The tokens to parse.
	tokens:      Peekable<IntoIter<Token>>,
	/// The number of errors encountered during parsing.
	error_count: usize,
}

impl Parser {
	pub fn new(tokens: Vec<Token>) -> Self { Self { tokens: tokens.into_iter().peekable(), error_count: 0 } }

	pub fn parse(mut self) -> Result<Vec<Statement>, LoxError> {
		let mut statements = Vec::new();
		while let Ok(token) = self.peek() {
			if matches!(token.r#type, TokenType::Eof) {
				break;
			}
			match self.parse_declaration() {
				Ok(stmt) => statements.push(stmt),
				Err(ParserError::InternalError(e)) => return Err(e.into()),
				Err(ParserError::ParseError(e)) => {
					self.synchronize(&e)?;
					continue;
				}
			}
		}
		if self.error_count > 0 { Err(LoxError::ParserErrors(self.error_count)) } else { Ok(statements) }
	}

	fn parse_declaration(&mut self) -> Result<Statement, ParserError> {
		if matches!(self.peek()?.r#type, TokenType::Var) {
			self.var_declaration()
		} else {
			self.parse_statement()
		}
	}

	fn var_declaration(&mut self) -> Result<Statement, ParserError> {
		self.advance()?; // consume 'var'
		let name_token = self.advance()?;

		if !matches!(name_token.r#type, TokenType::Identifier(_)) {
			return Err(ParseError::new(name_token.line, ParseErrorType::ExpectVariableName).into());
		}

		let initializer = if matches!(self.peek()?.r#type, TokenType::Equal) {
			self.advance()?; // consume '='
			Some(*self.expression()?)
		} else {
			None
		};

		if !matches!(self.peek()?.r#type, TokenType::Semicolon) {
			return Err(ParseError::new(self.peek()?.line, ParseErrorType::ExpectSemicolon).into());
		}
		self.advance()?; // consume ';'

		Ok(Statement::VarDeclaration { name_token, initializer })
	}

	fn parse_statement(&mut self) -> Result<Statement, ParserError> {
		if matches!(self.peek()?.r#type, TokenType::Print) {
			self.advance()?; // consume 'print'
			let expression = *self.expression()?;
			if !matches!(self.peek()?.r#type, Semicolon) {
				return Err(ParseError::new(self.peek()?.line, ParseErrorType::ExpectSemicolon).into());
			}
			self.advance()?; // consume ';'
			Ok(Statement::Print(expression))
		} else if matches!(self.peek()?.r#type, TokenType::LeftBrace) {
			self.advance()?; // consume '{'
			let mut statements = Vec::new();
			while !matches!(self.peek()?.r#type, TokenType::RightBrace | TokenType::Eof) {
				statements.push(self.parse_declaration()?);
			}
			// 检查是否真的找到了 RightBrace
			if !matches!(self.peek()?.r#type, TokenType::RightBrace) {
				return Err(ParseError::new(self.peek()?.line, ParseErrorType::ExpectRightBrace).into());
			}
			self.advance()?; // consume '}'
			Ok(Statement::Block(statements))
		} else {
			let expression = *self.expression()?;
			if !matches!(self.peek()?.r#type, Semicolon) {
				return Err(ParseError::new(self.peek()?.line, ParseErrorType::ExpectSemicolon).into());
			}
			self.advance()?; // consume ';'
			Ok(Statement::Expression(expression))
		}
	}

	/// Parse the tokens into an expression.
	#[allow(dead_code)]
	pub fn parse_expression(mut self) -> Result<Expression, LoxError> {
		match self.expression() {
			Ok(expr) => {
				if self.error_count > 0 {
					Err(LoxError::ParserErrors(self.error_count))
				} else {
					Ok(*expr)
				}
			}
			Err(ParserError::InternalError(e)) => Err(e.into()),
			Err(ParserError::ParseError(_)) => Err(LoxError::ParserErrors(self.error_count)),
		}
	}

	/// Parse comma expressions.
	fn expression(&mut self) -> Result<Box<Expression>, ParserError> { self.comma() }

	/// Parse comma expressions.
	fn comma(&mut self) -> Result<Box<Expression>, ParserError> {
		let mut expr = self.assignment()?;

		while matches!(self.peek()?.r#type, Comma) {
			self.advance()?;
			expr = Expression::comma(expr, self.assignment()?)
		}
		Ok(expr)
	}

	fn assignment(&mut self) -> Result<Box<Expression>, ParserError> {
		let expr = self.ternary()?;

		if matches!(self.peek()?.r#type, Equal) {
			let equals = self.advance()?;
			if let Expression::Variable(name_token) = *expr {
				let value = self.assignment()?;
				return Ok(Expression::assign(name_token, value));
			} else {
				return Err(ParseError::new(equals.line, ParseErrorType::InvalidAssignmentTarget).into());
			}
		}

		Ok(expr)
	}

	/// Parse ternary expressions.
	fn ternary(&mut self) -> Result<Box<Expression>, ParserError> {
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
	fn equality(&mut self) -> Result<Box<Expression>, ParserError> {
		let mut expression = self.comparison()?;
		while matches!(self.peek()?.r#type, BangEqual | EqualEqual) {
			expression = Expression::binary(expression, self.advance()?, self.comparison()?)
		}
		Ok(expression)
	}

	/// Parse comparison expressions.
	fn comparison(&mut self) -> Result<Box<Expression>, ParserError> {
		let mut expression = self.term()?;
		while matches!(self.peek()?.r#type, Greater | GreaterEqual | Less | LessEqual) {
			expression = Expression::binary(expression, self.advance()?, self.comparison()?)
		}
		Ok(expression)
	}

	/// Parse term expressions.
	fn term(&mut self) -> Result<Box<Expression>, ParserError> {
		let mut expression = self.factor()?;
		while matches!(self.peek()?.r#type, Minus | Plus) {
			expression = Expression::binary(expression, self.advance()?, self.factor()?)
		}
		Ok(expression)
	}

	/// Parse factor expressions.
	fn factor(&mut self) -> Result<Box<Expression>, ParserError> {
		let mut expression = self.unary()?;
		while matches!(self.peek()?.r#type, Slash | Star) {
			expression = Expression::binary(expression, self.advance()?, self.unary()?)
		}
		Ok(expression)
	}

	/// Parse unary expressions.
	fn unary(&mut self) -> Result<Box<Expression>, ParserError> {
		if matches!(self.peek()?.r#type, Bang | Minus) {
			return Ok(Expression::unary(self.advance()?, self.unary()?));
		}
		self.primary()
	}

	/// Parse primary expressions.
	fn primary(&mut self) -> Result<Box<Expression>, ParserError> {
		let token = self.peek()?;
		match &token.r#type {
			False | True | Nil | Number(_) | String(_) | Identifier(_) => {
				let token = self.advance()?;
				Ok(Box::new(token.try_into()?))
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
				let error = ParseError::new(token.line, ParseErrorType::UnexpectedToken(err_string));
				self.synchronize(&error)?;
				self.advance()?; // consume unexpected token
				Err(error.into())
			}
		}
	}

	/// Advance to the next token.
	fn advance(&mut self) -> anyhow::Result<Token> {
		self.tokens.next().ok_or_else(|| anyhow!("Unexpected EOF"))
	}

	/// Peek at the current token.
	fn peek(&mut self) -> anyhow::Result<&Token> { self.tokens.peek().ok_or_else(|| anyhow!("Unexpected EOF")) }

	/// Synchronize the parser after an error.
	#[allow(dead_code)]
	fn synchronize(&mut self, error: &ParseError) -> anyhow::Result<()> {
		self.error_count += 1;
		eprintln!("{error}");
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

	fn parse(input: &'static str, equals: &str) {
		let scanner = Scanner::new(input);
		let tokens = scanner.scan_tokens().unwrap();
		let parser = Parser::new(tokens);
		let ast = parser.parse_expression().unwrap();
		assert_eq!(ast.to_string(), equals);
	}

	// Expression tests using parse() function
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

	#[test]
	fn parse_all_operators() {
		parse(
			"!-1 < 2 ? 3 + 4 * 5 : 6 - 7 / 8, 9 == 10 != 11",
			"(, (? (< (! (- 1)) 2) : (+ 3 (* 4 5)) (- 6 (/ 7 8))) (!= (== 9 10) 11))",
		);
		parse(
			"(1 + 2) * 3 < 4 ? 5, 6 : 7 == 8 ? 9 : 10",
			"(? (< (* (group (+ 1 2)) 3) 4) : (, 5 6) (? (== 7 8) : 9 10))",
		);
	}

	// Variable-related tests using parse() function
	#[test]
	fn parse_variable_expressions() {
		// Variable names as identifiers
		parse("x", "x");
		parse("myVar", "myVar");
		parse("_underscore_var", "_underscore_var");

		// Expressions with variables
		parse("x + y", "(+ x y)");
		parse("x * 2", "(* x 2)");
		parse("(x + y) * z", "(* (group (+ x y)) z)");

		// Complex expressions with variables
		parse("x + y * z / w - v", "(- (+ x (/ (* y z) w)) v)");
		parse("x == y ? a : b", "(? (== x y) : a b)");

		// Variable initializers
		parse("42", "42"); // For "var x = 42;"
		parse("1 + 2 * 3", "(+ 1 (* 2 3))"); // For "var x = 1 + 2 * 3;"
		parse("true", "true"); // For "var x = true;"
		parse("\"hello\"", "\"hello\""); // For "var x = \"hello\";"
	}
}
