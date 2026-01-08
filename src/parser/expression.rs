//! Expression AST nodes
//! //! An `Expression` is a tree structure representing code like `-123 *
//! (45.67)` as nested nodes.

use Expression::*;
use LiteralValue::*;

use crate::scanner::Token;

#[allow(clippy::enum_variant_names)]
/// Expression AST nodes
#[derive(Debug)]
pub(crate) enum Expression {
	Literal(LiteralValue),
	Variable(Token),
	Unary { operator: Token, right: Box<Expression> },
	Binary { left: Box<Expression>, operator: Token, right: Box<Expression> },
	Logical { left: Box<Expression>, operator: Token, right: Box<Expression> },
	Call { callee: Box<Expression>, line: usize, arguments: Vec<Expression> },
	PropertyGet { instance: Box<Expression>, property: Token },
	PropertySet { instance: Box<Expression>, property: Token, value: Box<Expression> },
	This,
	Assign { target: Token, value: Box<Expression> },
	Ternary { condition: Box<Expression>, then_branch: Box<Expression>, else_branch: Box<Expression> },
	Comma { left: Box<Expression>, right: Box<Expression> },
	Grouping(Box<Expression>),
}

impl Expression {
	pub fn unary(operator: Token, right: Box<Self>) -> Box<Self> {
		Box::new(Expression::Unary { operator, right })
	}

	pub fn binary(left: Box<Self>, operator: Token, right: Box<Self>) -> Box<Self> {
		Box::new(Expression::Binary { left, operator, right })
	}

	pub fn comma(left: Box<Self>, right: Box<Self>) -> Box<Self> { Box::new(Expression::Comma { left, right }) }

	pub fn ternary(condition: Box<Self>, then_branch: Box<Self>, else_branch: Box<Self>) -> Box<Self> {
		Box::new(Expression::Ternary { condition, then_branch, else_branch })
	}

	pub fn grouping(expr: Box<Self>) -> Box<Self> { Box::new(Expression::Grouping(expr)) }

	pub fn assign(name: Token, value: Box<Self>) -> Box<Self> {
		Box::new(Expression::Assign { target: name, value })
	}

	pub fn logical(left: Box<Self>, operator: Token, right: Box<Self>) -> Box<Self> {
		Box::new(Expression::Logical { left, operator, right })
	}

	pub fn call(callee: Box<Self>, line: usize, arguments: Vec<Self>) -> Box<Self> {
		Box::new(Expression::Call { callee, line, arguments })
	}

	pub fn get(instance: Box<Self>, property: Token) -> Box<Self> {
		Box::new(Expression::PropertyGet { instance, property })
	}

	pub fn set(instance: Box<Self>, property: Token, value: Box<Self>) -> Box<Self> {
		Box::new(Expression::PropertySet { instance, property, value })
	}
}

#[allow(clippy::enum_variant_names)]
/// Literal values in the AST
#[derive(Debug)]
pub(crate) enum LiteralValue {
	Number(f64),
	StringLiteral(&'static str),
	Boolean(bool),
	Nil,
}

impl TryFrom<Token> for Expression {
	type Error = anyhow::Error;

	fn try_from(token: Token) -> Result<Self, Self::Error> {
		use crate::scanner::TokenType::*;

		Ok(match token.r#type {
			Number(n) => Literal(LiteralValue::Number(n)),
			String(s) => Literal(StringLiteral(s)),
			True => Literal(Boolean(true)),
			False => Literal(Boolean(false)),
			Nil => Literal(LiteralValue::Nil),
			Identifier(_) => Expression::Variable(token),
			_ => anyhow::bail!("Cannot convert token {:?} to Expression::Literal", token),
		})
	}
}

impl std::fmt::Display for Expression {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Literal(lit) => write!(f, "{lit}"),
			Unary { operator, right } => write!(f, "({} {right})", operator.lexeme),
			Binary { left, operator, right } => write!(f, "({} {left} {right})", operator.lexeme),
			Grouping(expression) => write!(f, "(group {expression})"),
			Comma { left, right } => write!(f, "(, {left} {right})"),
			Ternary { condition, then_branch, else_branch } => {
				write!(f, "(? {condition} : {then_branch} {else_branch})")
			}
			Variable(token) => write!(f, "{}", token.lexeme),
			Assign { target: name, value } => write!(f, "(= {} {value})", name.lexeme),
			Logical { left, operator, right } => write!(f, "({} {left} {right})", operator.lexeme),
			Call { callee, line: _, arguments } => write!(
				f,
				"(call {callee} ({}) )",
				arguments.iter().map(|arg| format!("{arg}")).collect::<Vec<String>>().join(" ")
			),
			PropertyGet { instance, property } => write!(f, "(get {instance}.{})", property.lexeme),
			PropertySet { instance, property, value } => write!(f, "(set {instance}.{}={value})", property.lexeme),
			This => write!(f, "this"),
		}
	}
}

impl std::fmt::Display for LiteralValue {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			LiteralValue::Number(n) => write!(f, "{n}"),
			LiteralValue::StringLiteral(s) => write!(f, "\"{s}\""),
			LiteralValue::Boolean(b) => write!(f, "{b}"),
			LiteralValue::Nil => write!(f, "nil"),
		}
	}
}
