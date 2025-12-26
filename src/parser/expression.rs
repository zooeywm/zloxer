//! Expression AST nodes
//! //! An `Expression` is a tree structure representing code like `-123 *
//! (45.67)` as nested nodes.

use Expression::*;
use LiteralValue::*;

use crate::scanner::Token;

#[allow(clippy::enum_variant_names)]
/// Expression AST nodes
#[derive(Debug)]
pub(crate) enum Expression<'a> {
	Literal(LiteralValue<'a>),
	Unary {
		operator: Token<'a>,
		right:    Box<Expression<'a>>,
	},
	Binary {
		left:     Box<Expression<'a>>,
		operator: Token<'a>,
		right:    Box<Expression<'a>>,
	},
	Grouping(Box<Expression<'a>>),
	Comma {
		left:  Box<Expression<'a>>,
		right: Box<Expression<'a>>,
	},
	Ternary {
		condition:   Box<Expression<'a>>,
		then_branch: Box<Expression<'a>>,
		else_branch: Box<Expression<'a>>,
	},
}

impl<'a> Expression<'a> {
	// ---------- Unary ----------
	pub fn unary(operator: Token<'a>, right: Box<Self>) -> Box<Self> {
		Box::new(Expression::Unary { operator, right })
	}

	// ---------- Binary ----------
	pub fn binary(left: Box<Self>, operator: Token<'a>, right: Box<Self>) -> Box<Self> {
		Box::new(Expression::Binary { left, operator, right })
	}

	pub fn comma(left: Box<Self>, right: Box<Self>) -> Box<Self> { Box::new(Expression::Comma { left, right }) }

	pub fn ternary(condition: Box<Self>, then_branch: Box<Self>, else_branch: Box<Self>) -> Box<Self> {
		Box::new(Expression::Ternary { condition, then_branch, else_branch })
	}

	// ---------- Grouping ----------
	pub fn grouping(expr: Box<Self>) -> Box<Self> { Box::new(Expression::Grouping(expr)) }
}

#[allow(clippy::enum_variant_names)]
/// Literal values in the AST
#[derive(Debug)]
pub(crate) enum LiteralValue<'a> {
	Number(f64),
	StringLiteral(&'a str),
	Boolean(bool),
	Nil,
}

impl<'a> TryFrom<Token<'a>> for Expression<'a> {
	type Error = anyhow::Error;

	fn try_from(token: Token<'a>) -> Result<Self, Self::Error> {
		use crate::scanner::TokenType::*;

		Ok(match token.r#type {
			Number(n) => Literal(LiteralValue::Number(n)),
			String(s) => Literal(StringLiteral(s)),
			True => Literal(Boolean(true)),
			False => Literal(Boolean(false)),
			Nil => Literal(LiteralValue::Nil),
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
			Comma { left, right } => write!(f, "(, {} {})", left, right),
			Ternary { condition, then_branch, else_branch } => {
				write!(f, "(? {} : {} {})", condition, then_branch, else_branch)
			}
		}
	}
}

impl std::fmt::Display for LiteralValue<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			LiteralValue::Number(n) => write!(f, "{}", n),
			LiteralValue::StringLiteral(s) => write!(f, "\"{}\"", s),
			LiteralValue::Boolean(b) => write!(f, "{}", b),
			LiteralValue::Nil => write!(f, "nil"),
		}
	}
}
