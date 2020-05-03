use super::token::{*, TokenType::*};
use super::lib::*;
use std::collections::HashMap;

pub static NONE_TOKEN: Token = Token {
	ty: TokenNoSignal,
	val: 0,
	program_id: 0,
	pos: 0,
};

struct Context {
	pub input: Vec<Token>,
	pub output: Vec<Token>,
	pub pos: usize,
	pub defined: HashMap<String, Vec<Token>>,
	pub next: Option<Box<Context>>,
}

impl Context {
	fn new(input: Vec<Token>, next: Option<Box<Context>>) -> Self {
		Context {
			input: input,
			output: vec![],
			pos: 0,
			defined: HashMap::new(),
			next: next,
		}
	}
	fn eof(&self) -> bool {
		return self.pos == self.input.len();
	}
	fn add_ident(&mut self, token: Token) {
		let name = String::from(&PROGRAMS.lock().unwrap()[token.program_id][token.pos..token.pos+token.val as usize]);
		if let Some(tokens) = self.defined.get(&name) {
			self.output.append(&mut tokens.clone());
		} else {
			self.output.push(token);
		}
		self.pos += 1;
	}
	fn define(&mut self) {
		if let TokenIdent = self.input[self.pos].ty {
			let name = String::from(&PROGRAMS.lock().unwrap()[self.input[self.pos].program_id][self.input[self.pos].pos..self.input[self.pos].pos+self.input[self.pos].val as usize]);
			let mut v3 = vec![];
			self.pos += 1;
			loop {
				if let TokenNewLine = self.input[self.pos].ty {
					self.pos += 1;
					break;
				} else {
					let token = self.input[self.pos].clone();
					v3.push(token);
					self.pos += 1;
				}
			}
			self.defined.insert(name, v3);
		} else {
			error(&format!("identifier expected after #define"));
		}
	}
	fn include(&mut self) {
		match self.input[self.pos].ty {
			TokenString(_) => {
				let path = self.input[self.pos].getstring();
				self.pos += 1;
				// input program
				add_program(path);
				let new_id = PROGRAMS.lock().unwrap().len()-1;
				let mut nv = tokenize(new_id, false);
				self.output.append(&mut nv);
			}
			_ => {
				error(&format!("string expected after #include"));
			}
		}
	}
}

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

pub fn preprocess(tokens: Vec<Token>) -> Vec<Token> {
	
	let mut ctx = Context::new(tokens, None);

	while !ctx.eof() {
		
		// ident
		if let TokenIdent = ctx.input[ctx.pos].ty {
			let token = ctx.input[ctx.pos].clone();
			ctx.add_ident(token);
			continue;
		}
		// #
		if let TokenSharp = ctx.input[ctx.pos].ty {
			ctx.pos += 1;
		} else {
			let token = ctx.input[ctx.pos].clone();
			ctx.pos += 1;
			ctx.output.push(token);
			continue;
		}
		// define
		if let TokenDefine = ctx.input[ctx.pos].ty {
			ctx.pos += 1;
			ctx.define();
			continue;
		}
		// include
		if let TokenInclude = ctx.input[ctx.pos].ty {
			ctx.pos += 1;
			ctx.include();
			continue;
		}
		
		error(&format!("macro expected at {}...", &PROGRAMS.lock().unwrap()[ctx.input[ctx.pos].program_id][ctx.input[ctx.pos].pos..ctx.input[ctx.pos].pos+5]));
	}
	
	return ctx.output;
}