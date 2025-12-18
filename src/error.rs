pub type Result<T> = std::result::Result<T, LoxError>;

#[derive(thiserror::Error, Debug)]
pub enum LoxError {
	#[error("Compiler internal error: {0}")]
	CompileError(#[from] anyhow::Error),
	#[error(transparent)]
	ScanError(#[from] ScanError),
}

impl LoxError {
	pub fn compile_error<E: Into<anyhow::Error>>(err: E) -> Self {
		LoxError::CompileError(err.into())
	}

	pub fn scan_error(line: u32, message: String) -> Self {
		LoxError::ScanError(ScanError { line, message })
	}
}

#[derive(thiserror::Error, Debug)]
#[error("ScanError: [line {line}] Error where {message}")]
pub struct ScanError {
	line: u32,
	message: String,
}
