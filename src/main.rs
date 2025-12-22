use palc::Parser;
use zloxer::cli::*;

fn main() {
	let loxer = zloxer::Loxer;

	match Cli::parse().mode {
		Mode::File { path } => {
			if let Err(e) = loxer.run_file(&path) {
				eprintln!("Failed run file: {e}");
			}
		}
		Mode::Repl => loxer.run_prompt(),
	}
}
