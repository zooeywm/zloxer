use Expression::*;

use crate::scanner::Token;

#[derive(Debug)]
pub enum Expression<'a> {
	Literal(LiteralValue<'a>),
	Unary { operator: Token<'a>, right: Box<Expression<'a>> },
	Binary { left: Box<Expression<'a>>, operator: Token<'a>, right: Box<Expression<'a>> },
	Grouping(Box<Expression<'a>>),
}

impl<'a> Expression<'a> {
	pub fn boxed(self) -> Box<Self> { Box::new(self) }
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
		let expression = Expression::Binary {
			left:     Expression::Unary {
				operator: Token::new(Minus, "-", 1),
				right:    Expression::Literal(LiteralValue::Number(123.)).boxed(),
			}
			.boxed(),
			operator: Token::new(Star, "*", 1),
			right:    Expression::Grouping(Expression::Literal(LiteralValue::Number(45.67)).boxed()).boxed(),
		};

		assert_eq!("(* (- 123) (group 45.67))", expression.to_string());
		println!("expression = {}", expression);
	}
}
