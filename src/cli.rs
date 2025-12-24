use std::path::PathBuf;

use palc::{Parser, Subcommand};

/// CLI arguments
#[derive(Parser)]
#[command(name = "zloxer", after_long_help = "This is zooeywm's lox compiler implementation.")]
pub struct Cli {
	/// Program mode
	#[command(subcommand)]
	pub mode: Mode,
}

/// Program mode
#[derive(Subcommand, Debug)]
pub enum Mode {
	/// Input file
	File { path: PathBuf },
	/// Input prompt
	Repl,
}
