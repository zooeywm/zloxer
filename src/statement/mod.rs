//! There is no place in the grammar where both an expression and a statement
//! are allowed. The operands of, say, `+` are always expressions, never
//! statements. The body of a `while` loop is always a statement.

use crate::parser::expression::Expression;

#[derive(Debug)]
pub enum Statement<'a> {
	Expression(Box<Expression<'a>>),
	Print(Box<Expression<'a>>),
}
