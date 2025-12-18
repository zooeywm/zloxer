use std::fmt::Display;

pub type Result<T> = std::result::Result<T, LoxError>;

#[derive(thiserror::Error, Debug)]
pub enum LoxError {
	#[error("CompilerInternalError{0}")]
	CompileInternalError(#[from] anyhow::Error),
	#[error(transparent)]
	ScanErrors(#[from] ScanErrors),
	#[error("{0}")]
	Text(&'static str),
}

#[derive(thiserror::Error, Debug)]
pub struct ScanErrors(Vec<ScanError>);

impl ScanErrors {
	pub fn new() -> Self { Self(vec![]) }

	pub fn push(&mut self, line: usize, message: impl Into<String>) {
		self.0.push(ScanError { line, message: message.into() });
	}

	pub fn is_empty(&self) -> bool { self.0.is_empty() }
}

impl Display for ScanErrors {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		writeln!(f, "There are ScanErrors:")?;
		for error in &self.0 {
			writeln!(f, "line {}: {}", error.line, error.message)?;
		}
		Ok(())
	}
}

impl From<&'static str> for LoxError {
	fn from(s: &'static str) -> Self { LoxError::Text(s) }
}

#[derive(Debug)]
struct ScanError {
	line:    usize,
	message: String,
}
