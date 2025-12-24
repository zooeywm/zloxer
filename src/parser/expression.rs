//! Expression AST nodes
//! //! An `Expression` is a tree structure representing code like `-123 *
//! (45.67)` as nested nodes.

use Expression::*;

use crate::scanner::Token;

/// Expression AST nodes
#[derive(Debug)]
pub(crate) enum Expression<'a> {
	Literal(LiteralValue<'a>),
	Unary { operator: Token<'a>, right: Box<Expression<'a>> },
	Binary { left: Box<Expression<'a>>, operator: Token<'a>, right: Box<Expression<'a>> },
	Grouping(Box<Expression<'a>>),
}

impl<'a> Expression<'a> {
	// ---------- Literal ----------
	pub fn literal(value: LiteralValue<'a>) -> Box<Self> { Box::new(Expression::Literal(value)) }

	pub fn number(n: f64) -> Box<Self> { Self::literal(LiteralValue::Number(n)) }

	pub fn string(s: &'a str) -> Box<Self> { Self::literal(LiteralValue::String(s)) }

	pub fn boolean(b: bool) -> Box<Self> { Self::literal(LiteralValue::Boolean(b)) }

	pub fn nil() -> Box<Self> { Self::literal(LiteralValue::Nil) }

	// ---------- Unary ----------
	pub fn unary(operator: Token<'a>, right: Box<Self>) -> Box<Self> {
		Box::new(Expression::Unary { operator, right })
	}

	// ---------- Binary ----------
	pub fn binary(left: Box<Self>, operator: Token<'a>, right: Box<Self>) -> Box<Self> {
		Box::new(Expression::Binary { left, operator, right })
	}

	// ---------- Grouping ----------
	pub fn grouping(expr: Box<Self>) -> Box<Self> { Box::new(Expression::Grouping(expr)) }
}

/// Literal values in the AST
#[derive(Debug)]
pub(crate) enum LiteralValue<'a> {
	Number(f64),
	String(&'a str),
	Boolean(bool),
	Nil,
}

impl<'a> TryFrom<Token<'a>> for Expression<'a> {
	type Error = anyhow::Error;

	fn try_from(token: Token<'a>) -> Result<Self, Self::Error> {
		use crate::scanner::TokenType::*;

		Ok(match token.r#type {
			NumberLiteral(n) => Expression::Literal(LiteralValue::Number(n)),
			StringLiteral(s) => Expression::Literal(LiteralValue::String(s)),
			True => Expression::Literal(LiteralValue::Boolean(true)),
			False => Expression::Literal(LiteralValue::Boolean(false)),
			Nil => Expression::Literal(LiteralValue::Nil),
			_ => anyhow::bail!("Cannot convert token {:?} to Expression::Literal", token),
		})
	}
}

impl std::fmt::Display for Expression<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Literal(lit) => write!(f, "{}", lit),
			Unary { operator, right } => write!(f, "({} {})", operator.lexeme, right),
			Binary { left, operator, right } => write!(f, "({} {} {})", operator.lexeme, left, right),
			Grouping(expression) => write!(f, "(group {})", expression),
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
	use super::*;
	use crate::scanner::TokenType::*;

	#[test]
	fn parse_expressions() {
		let expression = Expression::binary(
			Expression::unary(Token::new(Minus, "-", 1), Expression::number(123.0)),
			Token::new(Star, "*", 1),
			Expression::grouping(Expression::number(45.67)),
		);

		assert_eq!("(* (- 123) (group 45.67))", expression.to_string());
	}
}
