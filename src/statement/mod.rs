//! There is no place in the grammar where both an expression and a statement
//! are allowed. The operands of, say, `+` are always expressions, never
//! statements. The body of a `while` loop is always a statement.

use std::rc::Rc;

use crate::{parser::expression::Expression, scanner::Token};

/// A statement in the programming language.
#[derive(Debug)]
pub enum Statement {
	/// An expression used as a statement.
	Expression(Expression),
	FunDecl(Function),
	If {
		condition:   Expression,
		then_branch: Box<Statement>,
		else_branch: Option<Box<Statement>>,
	},
	While {
		condition: Expression,
		body:      Box<Statement>,
	},
	/// A print statement.
	Print(Expression),
	/// A return statememnt.
	Return(Option<Box<Expression>>),
	/// A block of statements.
	Block(Vec<Statement>),
	ClassDecl {
		name_token: Token,
		methods:    Vec<Function>,
	},
	/// A variable declaration statement.
	VarDeclaration {
		/// The token of the variable being declared.
		name_token:  Token,
		/// An optional initializer expression.
		initializer: Option<Expression>,
	},
	/// A break statement to exit loops.
	Break,
}

#[derive(Debug)]
pub struct Function {
	pub name_token: Token,
	pub parameters: Rc<Vec<Token>>,
	pub body:       Rc<Vec<Statement>>,
}
#[cfg(test)]
mod tests {
	use crate::scanner::Scanner;

	/// Helper function to parse a string into statements and return the count
	fn parse_statement_count(input: &'static str) -> usize {
		let scanner = Scanner::new(input);
		let tokens = scanner.scan_tokens().unwrap();
		let parser = crate::parser::Parser::new(tokens);
		parser.program().unwrap().len()
	}

	/// Helper function to parse a string and check if the first statement matches
	/// expected type
	fn parse_statement_type(input: &'static str, expected_type: &str) -> bool {
		use crate::statement::Statement;

		let scanner = Scanner::new(input);
		let tokens = scanner.scan_tokens().unwrap();
		let parser = crate::parser::Parser::new(tokens);
		let statements = parser.program().unwrap();

		match expected_type {
			"print" => matches!(statements[0], Statement::Print(_)),
			"expression" => matches!(statements[0], Statement::Expression(_)),
			"var" => matches!(statements[0], Statement::VarDeclaration { .. }),
			_ => false,
		}
	}

	/// Helper function to parse a variable declaration and check its properties
	fn parse_var_declaration(input: &'static str, name: &str, has_initializer: bool) -> bool {
		use crate::statement::Statement;

		let scanner = Scanner::new(input);
		let tokens = scanner.scan_tokens().unwrap();
		let parser = crate::parser::Parser::new(tokens);
		let statements = parser.program().unwrap();

		match &statements[0] {
			Statement::VarDeclaration { name_token, initializer } => {
				name_token.lexeme == name && initializer.is_some() == has_initializer
			}
			_ => false,
		}
	}
	#[test]
	fn test_expression_statement() {
		assert_eq!(parse_statement_count("123;"), 1);
		assert_eq!(parse_statement_count("1 + 2;"), 1);
		assert_eq!(parse_statement_count("1 + 2; 3 + 4;"), 2);
		assert!(parse_statement_type("123;", "expression"));
		assert!(parse_statement_type("x + y;", "expression"));
	}

	#[test]
	fn test_print_statement() {
		assert_eq!(parse_statement_count("print 123;"), 1);
		assert_eq!(parse_statement_count("print 1 + 2;"), 1);
		assert_eq!(parse_statement_count("print 123; print 456;"), 2);
		assert!(parse_statement_type("print 123;", "print"));
		assert!(parse_statement_type("print 1 + 2;", "print"));
	}

	#[test]
	fn test_var_declaration() {
		// Test without initializer
		assert_eq!(parse_statement_count("var x;"), 1);
		assert!(parse_statement_type("var x;", "var"));
		assert!(parse_var_declaration("var x;", "x", false));

		// Test with simple initializer
		assert_eq!(parse_statement_count("var y = 42;"), 1);
		assert!(parse_statement_type("var y = 42;", "var"));
		assert!(parse_var_declaration("var y = 42;", "y", true));

		// Test with complex initializer
		assert_eq!(parse_statement_count("var z = 1 + 2 * 3;"), 1);
		assert!(parse_statement_type("var z = 1 + 2 * 3;", "var"));
		assert!(parse_var_declaration("var z = 1 + 2 * 3;", "z", true));
	}

	#[test]
	fn test_mixed_statements() {
		// Test multiple different statement types
		assert_eq!(parse_statement_count("var x = 5; print x; x + 1;"), 3);
		assert!(parse_statement_type("var x = 5; print x; x + 1;", "var"));
		assert!(parse_statement_type("print x; x + 1;", "print"));
		assert!(parse_statement_type("x + 1;", "expression"));

		// Test variable followed by print of the variable
		let scanner = Scanner::new("var x = 5; print x;");
		let tokens = scanner.scan_tokens().unwrap();
		let parser = crate::parser::Parser::new(tokens);
		let statements = parser.program().unwrap();

		use crate::statement::Statement;
		assert!(matches!(statements[0], Statement::VarDeclaration { .. }));
		if let Statement::VarDeclaration { name_token, initializer } = &statements[0] {
			assert_eq!(name_token.lexeme, "x");
			assert!(initializer.is_some());
		}

		assert!(matches!(statements[1], Statement::Print(_)));
	}
}
