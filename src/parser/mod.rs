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
//! program        -> declaration* Eof
//! declaration    -> class_decl | fun_decl | var_decl | statement
//! class_decl     -> "class" Identifier "{" function* "}"
//! statement      -> print | return | block | break | if | while | for | expr_stmt
//! fun_decl       -> "fun" function
//! function       -> Identifier "(" parameters? ")" block
//! parameters     -> Identifier ("," Identifier)*
//! expr_stmt      -> expression ";"
//! print          -> "print" expression ";"
//! return         -> "return" expression? ";"
//! block          -> "{" declaration* "}"
//! break          -> "break" ";"
//! if             -> "if" "(" expression ")" statement ("else" statement)?
//! while          -> "while" "(" expression ")" statement
//! for            -> "for" "(" (var_decl | expr_stmt | ";")
//!                     expression? ";" expression? ")" statement
//! var_decl       -> "var" Identifier ("=" expression)? ";"
//! expression     -> comma
//! comma          -> assignment ("," assignment)*
//! // `call` here represents a potential l-value, validated during AST construction
//! assignment     -> call "=" assignment | ternary
//! ternary        -> logic_or ("?" expression ":" ternary)?
//! logic_or       -> logid_and ("or" logic_and)*
//! logid_and      -> equality ("and" equality)*
//! equality       -> comparison (("!=" | "==") comparison)*
//! comparison     -> term ((">" | ">=" | "<" | "<=") term)*
//! term           -> factor (("-" | "+") factor)*
//! factor         -> unary (("/" | "*") unary)*
//! unary          -> ("!" | "-") unary | call
//! call           -> primary (function_call | get_call)*
//! function_call  -> "(" arguments? ")"
//! get_call       -> "." Identifier
//! // Use assignment instead of expression to avoid parsing comma expressions in arguments
//! arguments      -> assignment ( "," assignment )*
//! primary        -> NUMBER | String | True | False | Nil | "(" expression ")" | Identifier
//! ```

pub(crate) mod expression;

use std::{convert::TryInto, iter::Peekable, rc::Rc, vec::IntoIter};

use TokenType::*;
use anyhow::Context;

use crate::{LoxError, error::parser::{ParseError, ParseErrorType, ParserError}, parser::expression::{Expression, LiteralValue}, scanner::{Token, TokenType}, statement::{Function, Statement}};

/// ParserError is the error type for the parser.
pub struct Parser {
	/// The tokens to parse.
	tokens:       Peekable<IntoIter<Token>>,
	/// The number of errors encountered during parsing.
	error_count:  usize,
	current_line: usize,
}

impl Parser {
	pub fn new(tokens: Vec<Token>) -> Self {
		Self { tokens: tokens.into_iter().peekable(), error_count: 0, current_line: 0 }
	}

	/// program -> declaration* Eof
	pub fn program(mut self) -> Result<Vec<Statement>, LoxError> {
		let mut statements = Vec::new();
		while let Ok(token) = self.peek() {
			if matches!(token.r#type, TokenType::Eof) {
				break;
			}
			match self.declaration() {
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

	/// declaration -> class_decl | fun_decl | var_decl | statement
	#[inline]
	fn declaration(&mut self) -> Result<Statement, ParserError> {
		match self.peek()?.r#type {
			TokenType::Class => self.class_decl(),
			TokenType::Fun => self.fun_decl(),
			TokenType::Var => self.var_decl(),
			_ => self.statement(),
		}
	}

	/// class_decl -> "class" Identifier "{" function* "}"
	#[inline]
	fn class_decl(&mut self) -> Result<Statement, ParserError> {
		self.current_line = self.peek()?.line;
		self.advance()?; // consume "class"
		let next = self.peek()?;
		if !matches!(next.r#type, Identifier(_)) {
			return Err(ParseError::new(self.current_line, ParseErrorType::ExpectClassName).into());
		}
		let name_token = self.advance()?; // class name
		self.consume(LeftBrace)?;
		let mut methods = vec![];
		while !self.check(&TokenType::RightBrace)? {
			methods.push(self.function()?);
		}

		self.consume(RightBrace)?;
		Ok(Statement::ClassDecl { name_token, methods })
	}

	/// statement -> print | return | block | break | if | while | for | expr_stmt
	#[inline]
	fn statement(&mut self) -> Result<Statement, ParserError> {
		use TokenType::*;

		self.current_line = self.peek()?.line;

		match &self.peek()?.r#type {
			Print => self.print(),
			Return => self.r#return(),
			LeftBrace => self.block(),
			Break => self.r#break(),
			If => self.r#if(),
			While => self.r#while(),
			For => self.r#for(),
			_ => self.expr_stmt(),
		}
	}

	/// fun_decl -> "fun" function
	#[inline]
	fn fun_decl(&mut self) -> Result<Statement, ParserError> {
		self.current_line = self.peek()?.line;
		self.advance()?; // consume 'fun'
		self.function().map(Statement::FunDecl)
	}

	/// function -> Identifier "(" parameters? ")" block
	#[inline]
	fn function(&mut self) -> Result<Function, ParserError> {
		let next = self.peek()?;
		if !matches!(next.r#type, Identifier(_)) {
			return Err(ParseError::new(self.current_line, ParseErrorType::ExpectFunctionNameOnCall).into());
		}
		let name_token = self.advance()?; // function name Identifier
		self.consume(LeftParen)?;
		let parameters = Rc::new(self.parameters(self.current_line)?);
		self.consume(RightParen)?;
		self.consume(LeftBrace)?;
		let mut body_statements = Vec::new();
		while !self.check(&RightBrace)? {
			body_statements.push(self.declaration()?);
		}
		self.consume(RightBrace)?;
		Ok(Function { name_token, parameters, body: Rc::new(body_statements) })
	}

	/// parameters -> Identifier ("," Identifier)*
	#[inline]
	fn parameters(&mut self, line: usize) -> Result<Vec<Token>, ParserError> {
		let mut parameters = Vec::new();
		if !self.check(&RightParen)? {
			loop {
				if parameters.len() >= 255 {
					return Err(ParseError::new(line, ParseErrorType::TooManyParameters).into());
				}
				let param_token = self.advance()?; // parameter
				if !matches!(param_token.r#type, TokenType::Identifier(_)) {
					return Err(ParseError::new(line, ParseErrorType::ExpectVariableName).into());
				}
				parameters.push(param_token);
				if !self.check(&Comma)? {
					break;
				}
				self.advance()?; // consume ','
			}
		}
		if !self.check(&RightParen)? {
			return Err(ParseError::new(line, ParseErrorType::ExpectRightParen).into());
		}
		Ok(parameters)
	}

	/// expr_stmt -> expression ";"
	#[inline]
	fn expr_stmt(&mut self) -> Result<Statement, ParserError> {
		let expr = *self.expression()?;
		self.consume(Semicolon)?;
		Ok(Statement::Expression(expr))
	}

	/// print -> "print" expression ";"
	#[inline]
	fn print(&mut self) -> Result<Statement, ParserError> {
		self.advance()?; // consume "print"
		let expr = *self.expression()?;
		self.consume(Semicolon)?;
		Ok(Statement::Print(expr))
	}

	/// return -> "return" expression? ";"
	#[inline]
	fn r#return(&mut self) -> Result<Statement, ParserError> {
		self.advance()?; // consume "return"
		let value = (!self.check(&Semicolon)?).then(|| self.expression()).transpose()?;
		self.consume(Semicolon)?;
		Ok(Statement::Return(value))
	}

	/// block -> "{" declaration* "}"
	#[inline]
	fn block(&mut self) -> Result<Statement, ParserError> {
		self.advance()?; // consume '{'
		let mut statements = Vec::new();
		while !self.check(&RightBrace)? {
			statements.push(self.declaration()?);
		}
		self.consume(RightBrace)?;
		Ok(Statement::Block(statements))
	}

	/// break -> "break" ";"
	#[inline]
	fn r#break(&mut self) -> Result<Statement, ParserError> {
		self.advance()?; // consume "break"
		self.consume(Semicolon)?;
		Ok(Statement::Break)
	}

	/// if -> "if" "(" expression ")" statement ("else" statement)?
	#[inline]
	fn r#if(&mut self) -> Result<Statement, ParserError> {
		self.advance()?; // consume "if"
		self.consume(LeftParen)?;
		let condition = *self.expression()?;
		self.consume(RightParen)?;

		let then_branch = Box::new(self.statement()?);
		let else_branch = self
			.check(&Else)?
			.then(|| -> Result<_, ParserError> {
				self.advance()?; // consume "else"
				Ok(Box::new(self.statement()?))
			})
			.transpose()?;

		Ok(Statement::If { condition, then_branch, else_branch })
	}

	/// while -> "while" "(" expression ")" statement
	#[inline]
	fn r#while(&mut self) -> Result<Statement, ParserError> {
		self.advance()?; // consume "while"
		self.consume(LeftParen)?;
		let condition = *self.expression()?;
		self.consume(RightParen)?;
		let body = Box::new(self.statement()?);
		Ok(Statement::While { condition, body })
	}

	/// for -> "for" "(" (var_decl | expr_stmt | ";")
	///          expression? ";" expression? ")" statement
	#[inline]
	fn r#for(&mut self) -> Result<Statement, ParserError> {
		self.advance()?; // consume "for"
		self.consume(LeftParen)?; // consume '('

		let initializer = match &self.peek()?.r#type {
			Semicolon => {
				self.advance()?;
				None
			}
			Var => Some(self.var_decl()?),
			_ => Some(self.statement()?),
		};

		let condition = if !self.check(&Semicolon)? { Some(*self.expression()?) } else { None };
		self.consume(Semicolon)?;

		let increment = if !self.check(&RightParen)? { Some(*self.expression()?) } else { None };
		self.consume(RightParen)?;

		let mut body = self.statement()?;
		if let Some(inc) = increment {
			body = Statement::Block(vec![body, Statement::Expression(inc)]);
		}

		let condition = condition.unwrap_or(Expression::Literal(LiteralValue::Boolean(true)));
		body = Statement::While { condition, body: Box::new(body) };

		if let Some(init) = initializer {
			body = Statement::Block(vec![init, body]);
		}

		Ok(body)
	}

	/// var_decl -> "var" Identifier ("=" expression)? ";"
	#[inline]
	fn var_decl(&mut self) -> Result<Statement, ParserError> {
		self.current_line = self.peek()?.line;
		self.advance()?; // consume 'var'
		if !matches!(self.peek()?.r#type, TokenType::Identifier(_)) {
			return Err(ParseError::new(self.current_line, ParseErrorType::ExpectVariableName).into());
		}
		let name_token = self.advance()?; // variable name Identifier

		let initializer = if self.check(&Equal)? {
			self.advance()?; // consume '='
			Some(*self.expression()?)
		} else {
			None
		};

		self.consume(Semicolon)?;

		Ok(Statement::VarDeclaration { name_token, initializer })
	}

	/// expression -> comma
	#[inline]
	fn expression(&mut self) -> Result<Box<Expression>, ParserError> { self.comma() }

	/// comma -> assignment ("," assignment)*
	#[inline]
	fn comma(&mut self) -> Result<Box<Expression>, ParserError> {
		let mut expr = self.assignment()?;

		while self.check(&Comma)? {
			self.advance()?;
			expr = Expression::comma(expr, self.assignment()?)
		}
		Ok(expr)
	}

	/// `call` here represents a potential l-value, validated during AST
	/// construction
	/// assignment -> call "=" assignment | ternary
	#[inline]
	fn assignment(&mut self) -> Result<Box<Expression>, ParserError> {
		let expr = self.ternary()?;

		if self.check(&Equal)? {
			self.advance()?; // consume '='
			if let Expression::Variable(name_token) = *expr {
				let value = self.assignment()?;
				return Ok(Expression::assign(name_token, value));
			} else {
				return Err(ParseError::new(self.current_line, ParseErrorType::InvalidAssignmentTarget).into());
			}
		}

		Ok(expr)
	}

	/// ternary -> logic_or ("?" expression ":" ternary)?
	#[inline]
	fn ternary(&mut self) -> Result<Box<Expression>, ParserError> {
		let condition = self.logic_or()?;

		if self.check(&Question)? {
			self.advance()?;
			let then_branch = self.expression()?;
			self.consume(Colon)?;
			let else_branch = self.ternary()?;
			return Ok(Expression::ternary(condition, then_branch, else_branch));
		}
		Ok(condition)
	}

	/// logic_or -> logic_and ("or" logic_and)*
	#[inline]
	fn logic_or(&mut self) -> Result<Box<Expression>, ParserError> {
		let mut expression = self.logic_and()?;
		while self.check(&Or)? {
			expression = Expression::logical(expression, self.advance()?, self.logic_and()?)
		}
		Ok(expression)
	}

	/// logic_and -> equality ("and" equality)*
	#[inline]
	fn logic_and(&mut self) -> Result<Box<Expression>, ParserError> {
		let mut expression = self.equality()?;
		while self.check(&And)? {
			expression = Expression::logical(expression, self.advance()?, self.equality()?)
		}
		Ok(expression)
	}

	/// equality -> comparison (("!=" | "==") comparison)*
	#[inline]
	fn equality(&mut self) -> Result<Box<Expression>, ParserError> {
		let mut expression = self.comparison()?;
		while matches!(self.peek()?.r#type, BangEqual | EqualEqual) {
			expression = Expression::binary(expression, self.advance()?, self.comparison()?)
		}
		Ok(expression)
	}

	/// comparison -> term ((">" | ">=" | "<" | "<=") term)*
	#[inline]
	fn comparison(&mut self) -> Result<Box<Expression>, ParserError> {
		let mut expression = self.term()?;
		while matches!(self.peek()?.r#type, Greater | GreaterEqual | Less | LessEqual) {
			expression = Expression::binary(expression, self.advance()?, self.comparison()?)
		}
		Ok(expression)
	}

	/// term -> factor (("-" | "+") factor)*
	#[inline]
	fn term(&mut self) -> Result<Box<Expression>, ParserError> {
		let mut expression = self.factor()?;
		while matches!(self.peek()?.r#type, Minus | Plus) {
			expression = Expression::binary(expression, self.advance()?, self.factor()?)
		}
		Ok(expression)
	}

	/// factor -> unary (("/" | "*") unary)*
	#[inline]
	fn factor(&mut self) -> Result<Box<Expression>, ParserError> {
		let mut expression = self.unary()?;
		while matches!(self.peek()?.r#type, Slash | Star) {
			expression = Expression::binary(expression, self.advance()?, self.unary()?)
		}
		Ok(expression)
	}

	/// unary -> ("!" | "-") unary | call
	#[inline]
	fn unary(&mut self) -> Result<Box<Expression>, ParserError> {
		if matches!(self.peek()?.r#type, Bang | Minus) {
			return Ok(Expression::unary(self.advance()?, self.unary()?));
		}
		self.call()
	}

	/// call -> primary (function_call | get_call)*
	#[inline]
	fn call(&mut self) -> Result<Box<Expression>, ParserError> {
		let mut expr = self.primary()?;

		loop {
			expr = match self.peek()?.r#type {
				LeftParen => self.function_call(expr)?,
				Dot => self.get_call(expr)?,
				_ => break,
			}
		}

		Ok(expr)
	}

	/// function_call -> "(" arguments? ")"
	#[inline]
	fn function_call(&mut self, expr: Box<Expression>) -> Result<Box<Expression>, ParserError> {
		self.advance()?; // consume '('
		let mut arguments = Vec::new();
		if !self.check(&RightParen)? {
			loop {
				if arguments.len() >= 255 {
					return Err(ParseError::new(self.current_line, ParseErrorType::TooManyArguments).into());
				}
				// Use assignment() instead of expression() to avoid parsing comma expressions
				// in function arguments
				arguments.push(*self.assignment()?);
				if !self.check(&Comma)? {
					break;
				}
				self.advance()?; // consume ','
			}
		}
		self.consume(RightParen)?;

		Ok(Expression::call(expr, self.current_line, arguments))
	}

	/// get_call -> "." Identifier
	#[inline]
	fn get_call(&mut self, expr: Box<Expression>) -> Result<Box<Expression>, ParserError> {
		self.advance()?; // consume dot

		if !matches!(self.peek()?.r#type, Identifier(_)) {
			return Err(ParseError::new(self.current_line, ParseErrorType::ExpectPropertyName).into());
		}
		let property = self.advance()?; // instance
		Ok(Expression::get(expr, property))
	}

	#[allow(unused)]
	/// Parse the tokens into an expression.
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

	/// Parse primary expressions.
	#[inline]
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
				self.consume(RightParen)?;

				Ok(Expression::grouping(expr))
			}
			_ => {
				let err_string = token.lexeme.to_string();
				Err(ParseError::new(self.current_line, ParseErrorType::UnexpectedToken(err_string)).into())
			}
		}
	}

	/// Advance to the next token.
	#[inline]
	fn advance(&mut self) -> anyhow::Result<Token> { self.tokens.next().with_context(|| "Unexpected EOF") }

	/// Peek at the current token.
	#[inline]
	fn peek(&mut self) -> anyhow::Result<&Token> { self.tokens.peek().with_context(|| "Unexpected EOF") }

	/// Synchronize the parser after an error.
	#[inline]
	fn synchronize(&mut self, error: &ParseError) -> anyhow::Result<()> {
		self.error_count += 1;
		eprintln!("{error}");
		while let Ok(token) = self.peek() {
			if matches!(token.r#type, Class | Fun | Var | For | If | While | Print | Return) {
				return Ok(());
			}
			self.advance()?;
		}
		Ok(())
	}

	/// Consume next token
	#[inline]
	fn consume(&mut self, ty: TokenType) -> Result<Token, ParserError> {
		use ParseErrorType::*;
		if !self.check(&ty)? {
			let err = match ty {
				LeftBrace => ExpectLeftBrace,
				RightBrace => ExpectRightBrace,
				LeftParen => ExpectLeftParen,
				RightParen => ExpectRightParen,
				Semicolon => ExpectSemicolon,
				Colon => ExpectColon,
				_ => return Err(anyhow::anyhow!("unreachable!").into()),
			};
			return Err(ParseError::new(self.current_line, err).into());
		}
		Ok(self.advance()?)
	}

	/// Check next token
	#[inline]
	fn check(&mut self, ty: &TokenType) -> Result<bool, ParserError> { Ok(&self.peek()?.r#type == ty) }
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
