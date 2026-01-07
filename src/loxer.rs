use std::{fs::read_to_string, io::Write, path::Path};

use anyhow::Context;

use crate::{LoxError, interpreter::Interpreter, parser::Parser, scanner::Scanner};

/// Loxer is the main struct for the Lox compiler/interpreter.
pub struct Loxer {
	interpreter: Interpreter,
}

impl Loxer {
	/// Create a new Loxer instance.
	pub fn new() -> Self { Self { interpreter: Interpreter::new() } }

	/// Run a file with the given path.
	pub fn run_file<P: AsRef<Path>>(&mut self, path: P) -> Result<(), LoxError> {
		let source = read_to_string(path).context("Failed open source file")?;
		self.run_statements(source)
	}

	/// Run the REPL prompt.
	pub fn run_prompt(&mut self) {
		loop {
			let mut input = String::new();
			let stdin = std::io::stdin();
			input.clear();
			print!("> ");
			if let Err(e) = std::io::stdout().flush() {
				eprintln!("Failed flush: {e}");
			}
			match stdin.read_line(&mut input) {
				Ok(0) => {
					println!("\nExited zloxer repl");
					break;
				}
				Ok(_) => {}
				Err(e) => {
					eprintln!("Failed read line: {e}");
					continue;
				}
			}
			let first_non_ws = input.char_indices().find(|(_, c)| !c.is_whitespace()).map(|(i, _)| i);
			let result = if let Some(start_idx) = first_non_ws {
				if input[start_idx..].starts_with("var") || input[start_idx..].starts_with("print") {
					self.run_statements(input)
				} else {
					match input.chars().rev().find(|c| !c.is_whitespace()) {
						Some(_) => self.run_statements(input),
						// Some(_) => self.run_expression(input),
						None => Ok(()),
					}
				}
			} else {
				Ok(())
			};

			if let Err(e) = result {
				eprintln!("Failed run prompt: {e}");
			}
		}
	}

	fn run_statements(&mut self, source: String) -> Result<(), LoxError> {
		let leaked: &'static str = source.leak();
		let scanner = Scanner::new(leaked);
		let tokens = scanner.scan_tokens()?;
		let parser = Parser::new(tokens);
		let statements = parser.program()?;
		self.interpreter.interpret_statements(&statements)?;

		Ok(())
	}

	#[allow(unused)]
	fn run_expression(&mut self, source: String) -> Result<(), LoxError> {
		let leaked: &'static str = source.leak();
		let scanner = Scanner::new(leaked);
		let tokens = scanner.scan_tokens()?;
		let parser = Parser::new(tokens);
		let expression = parser.parse_expression()?;
		self.interpreter.interpret_expression(expression)?;

		Ok(())
	}
}

impl Default for Loxer {
	fn default() -> Self { Self::new() }
}
