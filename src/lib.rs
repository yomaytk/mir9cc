pub fn error(path: Option<String>, line: usize, message: &str) {
    if let Some(p) = path {
        eprintln!("Compile error at: {}. Line: {}", p, line);
    } else {
        eprintln!("Compile error.");
    }
    eprintln!("{}", message);
    std::process::exit(1);
}
