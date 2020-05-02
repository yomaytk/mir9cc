use super::token::{*, TokenType::*};
use super::lib::*;

pub static NONE_TOKEN: Token = Token {
	ty: TokenNoSignal,
	val: 0,
	program_id: 0,
	pos: 0,
};

pub fn add_program(path: String) {
	match read_file(&path[..]) {
		Ok(content) => {
			let mut program = content;
			remove_backslash_or_crlf_newline(&mut program);
			PROGRAMS.lock().unwrap().push(program); 
		}
		Err(_) => {
			println!("failed to read file.");
			std::process::exit(1);
		}
	}
}

pub fn preprocess(program_id: usize, mut tokens: Vec<Token>) -> Vec<Token> {

	let mut poss = 0;
	let pos = &mut poss;
	let mut v = vec![];
	
	while *pos < tokens.len() {
		// #
		if !tokens[*pos].consume_ty(TokenSharp, pos) {
			let token = std::mem::replace(&mut tokens[*pos], NONE_TOKEN.clone());
			*pos += 1;
			v.push(token);
			continue;
		}
		// include
		if !tokens[*pos].consume_ty(TokenInclude, pos) {
			error(&format!("include expected: {}", &PROGRAMS.lock().unwrap()[program_id][*pos..*pos+5]));
		}
		// path
		if tokens[*pos].consume_ty(TokenString(String::new()), pos) {
			let path = tokens[*pos].getstring();
			*pos += 1;
			// input program
			add_program(path);
			let new_id = PROGRAMS.lock().unwrap().len()-1;
			let mut nv = tokenize(new_id, false);
			v.append(&mut nv);
		} else {
			error(&format!("path name expected: {}", &PROGRAMS.lock().unwrap()[program_id][*pos..*pos+5]))
		}
	}
	
	return v;
}