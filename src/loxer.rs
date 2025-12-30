use std::{fs::read_to_string, io::Write, path::Path};

use anyhow::Context;

use crate::{LoxError, interpreter::Interpreter, parser::Parser, scanner::Scanner};

/// Loxer is the main struct for the Lox compiler/interpreter.
pub struct Loxer {
	sources:     rpds::List<&'static str>,
	interpreter: Interpreter,
}

impl Loxer {
	/// Create a new Loxer instance.
	pub fn new() -> Self { Self { sources: rpds::List::new(), interpreter: Interpreter::new() } }

	/// Run a file with the given path.
	pub fn run_file<P: AsRef<Path>>(&mut self, path: P) -> Result<(), LoxError> {
		let source = read_to_string(path).context("Failed open source file")?;
		self.run(source)
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
			if let Err(e) = self.run(input) {
				eprintln!("Failed run prompt: {e}");
			}
		}
	}

	/// Run the Loxer on the given source code.
	fn run(&mut self, source: String) -> Result<(), LoxError> {
		// Use `String::leak()` instead of `String::into_boxed_str()` to avoid
		// reallocation
		let leaked: &'static str = source.leak();
		self.sources.push_front_mut(leaked);
		let source_ref: &str = self.sources.first().context("No source found")?;
		let scanner = Scanner::new(source_ref);
		let tokens = scanner.scan_tokens()?;
		let parser = Parser::new(tokens);
		let statements = parser.parse()?;
		self.interpreter.interpret(statements)?;

		Ok(())
	}
}

impl Default for Loxer {
	fn default() -> Self { Self::new() }
}
