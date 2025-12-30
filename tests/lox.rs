#[cfg(test)]
mod tests {
	use std::path::PathBuf;

	#[test]
	fn test_lox_file() {
		let mut loxer = zloxer::Loxer::new();
		let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests").join("test.lox");
		let result = loxer.run_file(&path);
		assert!(result.is_ok());
	}
}
