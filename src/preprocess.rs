use super::token::{*, TokenType::*};
use super::lib::*;
use std::collections::HashMap;

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

pub fn preprocess(mut tokens: Vec<Token>) -> Vec<Token> {

	let mut poss = 0;
	let pos = &mut poss;
	let mut v = vec![];
	let mut v2:HashMap<String, Vec<Token>> = HashMap::new();
	
	while *pos < tokens.len() {
		// #
		if !tokens[*pos].consume_ty(TokenSharp, pos) {
			let token = std::mem::replace(&mut tokens[*pos], NONE_TOKEN.clone());
			*pos += 1;
			if let TokenIdent = token.ty {
				let name = String::from(&PROGRAMS.lock().unwrap()[token.program_id][token.pos..token.pos+token.val as usize]);
				if let Some(tks) = v2.get(&name) {
					v.append(&mut tks.clone());
					continue;
				}
			}
			v.push(token);
			continue;
		}
		// define
		if tokens[*pos].consume_ty(TokenDefine, pos) {
			tokens[*pos].assert_ty(TokenIdent, pos);
			let name = String::from(&PROGRAMS.lock().unwrap()[tokens[*pos-1].program_id][tokens[*pos-1].pos..tokens[*pos-1].pos+tokens[*pos-1].val as usize]);
			let mut v3 = vec![];
			while !tokens[*pos].consume_ty(TokenNewLine, pos) {
				let token = std::mem::replace(&mut tokens[*pos], NONE_TOKEN.clone());
				v3.push(token);
				*pos += 1;
			}
			v2.insert(name, v3);
			continue;
		}
		// include
		if tokens[*pos].consume_ty(TokenInclude, pos) {
			tokens[*pos].assert_ty(TokenString(String::new()), pos);
			let path = tokens[*pos].getstring();
			*pos += 1;
			// input program
			add_program(path);
			let new_id = PROGRAMS.lock().unwrap().len()-1;
			let mut nv = tokenize(new_id, false);
			v.append(&mut nv);
			continue;
		}
		
		error(&format!("macro expected at {}...", &PROGRAMS.lock().unwrap()[tokens[*pos].program_id][tokens[*pos].pos..tokens[*pos].pos+5]));
	}
	
	return v;
}