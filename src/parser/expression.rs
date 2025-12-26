//! Expression AST nodes
//! //! An `Expression` is a tree structure representing code like `-123 *
//! (45.67)` as nested nodes.

use Expression::*;

use crate::scanner::Token;

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

/// Literal values in the AST
#[derive(Debug)]
pub(crate) enum LiteralValue<'a> {
	Number(f64),
	StringLiteral2(&'a str),
	Boolean(bool),
	NilLiteral,
}

impl<'a> TryFrom<Token<'a>> for Expression<'a> {
	type Error = anyhow::Error;

	fn try_from(token: Token<'a>) -> Result<Self, Self::Error> {
		use crate::scanner::TokenType::*;

		Ok(match token.r#type {
			NumberToken(n) => Expression::Literal(LiteralValue::Number(n)),
			StringToken(s) => Expression::Literal(LiteralValue::StringLiteral2(s)),
			True => Expression::Literal(LiteralValue::Boolean(true)),
			False => Expression::Literal(LiteralValue::Boolean(false)),
			NilToken => Expression::Literal(LiteralValue::NilLiteral),
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
			LiteralValue::StringLiteral2(s) => write!(f, "\"{}\"", s),
			LiteralValue::Boolean(b) => write!(f, "{}", b),
			LiteralValue::NilLiteral => write!(f, "nil"),
		}
	}
}
