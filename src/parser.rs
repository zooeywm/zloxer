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

use Expression::*;

use crate::scanner::Token;

#[derive(Debug)]
pub enum Expression<'a> {
	Literal(LiteralValue<'a>),
	Unary { operator: Token<'a>, right: Box<Expression<'a>> },
	Binary { left: Box<Expression<'a>>, operator: Token<'a>, right: Box<Expression<'a>> },
	Grouping { expression: Box<Expression<'a>> },
}

#[derive(Debug)]
pub enum LiteralValue<'a> {
	Number(f64),
	String(&'a str),
	Boolean(bool),
	Nil,
}

impl std::fmt::Display for Expression<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Literal(lit) => write!(f, "{}", lit),
			Unary { operator, right } => write!(f, "({} {})", operator.lexeme, right),
			Binary { left, operator, right } => write!(f, "({} {} {})", operator.lexeme, left, right),
			Grouping { expression } => write!(f, "(group {})", expression),
		}
	}
}

impl std::fmt::Display for LiteralValue<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			LiteralValue::Number(n) => write!(f, "{}", n),
			LiteralValue::String(s) => write!(f, "\"{}\"", s),
			LiteralValue::Boolean(b) => write!(f, "{}", b),
			LiteralValue::Nil => write!(f, "nil"),
		}
	}
}

#[cfg(test)]
mod tests {
	use LiteralValue::*;

	use super::*;
	use crate::scanner::TokenType::*;

	#[test]
	fn parse_expressions() {
		let expression = Binary {
			left:     Box::new(Unary {
				operator: Token { r#type: Minus, lexeme: "-", line: 1 },
				right:    Box::new(Literal(Number(123.))),
			}),
			operator: Token { r#type: Star, lexeme: "*", line: 1 },
			right:    Box::new(Grouping { expression: Box::new(Literal(Number(45.67))) }),
		};

		assert_eq!("(* (- 123) (group 45.67))", expression.to_string());
        println!("expression = {}", expression);
	}
}
