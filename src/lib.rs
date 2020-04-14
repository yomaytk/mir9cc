pub fn error(message: &str) {
	eprintln!("Compile error.");
	eprintln!("{}", message);
	std::process::exit(1);
}