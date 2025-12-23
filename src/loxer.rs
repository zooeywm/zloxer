use std::{fs::read_to_string, io::Write, path::Path};

use anyhow::Context;

use crate::scanner::Scanner;

pub struct Loxer;

impl Loxer {
	pub fn run_file<P: AsRef<Path>>(&self, path: P) -> crate::LoxResult<()> {
		let source = read_to_string(path).context("Failed open source file")?;
		self.run(&source)
	}

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
	fn run(&self, source: &str) -> crate::LoxResult<()> {
		let mut scanner = Scanner::new(source);
		let tokens = scanner.scan_tokens()?;
		println!("tokens = {:?}", tokens);
		Ok(())
	}
}
