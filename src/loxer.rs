use std::{fs::read_to_string, io::Write, path::Path};

use anyhow::Context;

use crate::{LoxError, interpreter::Interpreter, parser::Parser, scanner::Scanner};

/// Loxer is the main struct for the Lox compiler/interpreter.
pub struct Loxer;

impl Loxer {
	/// Create a new Loxer instance.
	pub fn run_file<P: AsRef<Path>>(&self, path: P) -> Result<(), LoxError> {
		let source = read_to_string(path).context("Failed open source file")?;
		self.run(&source)
	}

	/// Run the REPL prompt.
	pub fn run_prompt(&self) {
		let mut input = String::new();
		let stdin = std::io::stdin();
		loop {
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
			if let Err(e) = self.run(input.trim()) {
				eprintln!("Failed run prompt: {e}");
			}
		}
	}
}

impl Loxer {
	/// Run the Loxer on the given source code.
	fn run(&self, source: &str) -> Result<(), LoxError> {
		let mut scanner = Scanner::new(source);
		let tokens = scanner.scan_tokens()?;
		let mut parser = Parser::new(tokens);
		let statements = parser.parse()?;
		let interpreter = Interpreter;
		interpreter.interpret(statements)?;

		Ok(())
	}
}
