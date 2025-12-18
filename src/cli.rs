use std::path::PathBuf;

use palc::{Parser, Subcommand};

#[derive(Parser)]
#[command(name = "zloxer", after_long_help = "This is zooeywm's lox compiler implementation.")]
pub struct Cli {
	#[command(subcommand)]
	pub mode: Mode,
}

#[derive(Subcommand, Debug)]
pub enum Mode {
	/// Input file
	File { path: PathBuf },
	/// Input prompt
	Repl,
}
