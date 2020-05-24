use super::token::{*, TokenType::*};
use super::lib::*;
use std::collections::HashMap;
use std::sync::Mutex;

pub static NONE_TOKEN: Token = Token {
	ty: TokenNoSignal,
	val: 0,
	program_id: 0,
	pos: 0,
	end: 0,
	line: 0,
};

lazy_static! {
	pub static ref PATH: Mutex<HashMap<usize, String>> = Mutex::new(HashMap::new());
}

struct Env {
	pub input: Vec<Token>,
	pub output: Vec<Token>,
	pub pos: usize,
	pub defined: HashMap<String, Macro>,
	pub next: Option<Box<Env>>,
}

impl Env {
	fn new(input: Vec<Token>, next: Option<Box<Env>>) -> Self {
		Env {
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
	fn read_until_eol(&mut self) -> Vec<Token> {
		let mut v = vec![];
		while !self.eof() {
			if self.consume_ty(TokenNewLine) {
				break;
			} else {
				v.push(self.input[self.pos].clone());
			}
			self.pos += 1;
		}
		return v;
	}
	fn consume_ty(&mut self, ty: TokenType) -> bool {
		if self.input[self.pos].ty == ty {
			self.pos += 1;
			return true;
		} else {
			match (&self.input[self.pos].ty, ty) {
				(TokenString(_), TokenString(_)) => { self.pos += 1; return true; }
				_ => { return false; }
			}
		}
	}
	fn assert_ty(&mut self, ty: TokenType) {
		// assert!(self.consume_ty(ty));
		if !self.consume_ty(ty) {
			panic!("{:?}", self.input[self.pos].ty);
		}
	}
	fn define(&mut self) {
		let name = self.ident();
		if self.consume_ty(TokenRightBrac) {
			Macro::define_funclike(self, name);
		} else {
			Macro::define_objlike(self, name);
		}
	}
	fn include(&mut self) {
		match self.input[self.pos].ty {
			TokenString(_) => {
				let path = self.input[self.pos].getstring();
				self.pos += 1;
				// input program
				add_program(path.clone());
				let program_id = PROGRAMS.lock().unwrap().len()-1;
				PATH.lock().unwrap().insert(program_id, path);
				let mut nv = tokenize(program_id, false);
				self.output.append(&mut nv);
			}
			_ => {
				error(None, 0, &format!("string expected after #include"));
			}
		}
	}
	fn ident(&mut self) -> String {
		match self.input[self.pos].ty {
			TokenIdent => {
				let name = String::from(&PROGRAMS.lock().unwrap()[self.input[self.pos].program_id][self.input[self.pos].pos..self.input[self.pos].pos+self.input[self.pos].val as usize]);
				self.pos += 1;
				return name;
			}
			_ => {
				error(get_path(self.input[self.pos].program_id), self.input[self.pos].line, &format!("macro name expected."));
				panic!("macro name expected.");
			}
		}
	}
	fn peek(&mut self) -> Token {
		return self.input[self.pos].clone();
	}
	fn read_arg(&mut self) -> Vec<Token> {
		let mut v = vec![];
		let mut level = 0;
		let mut token = self.peek();
		let program_id = token.program_id;
		let line = token.line;
		while !self.eof() {
			token = self.peek();
			if level == 0 {
				if token.ty == TokenComma || token.ty == TokenLeftBrac {
					return v;
				}
			}
			self.pos += 1;
			if token.ty == TokenRightBrac {
				level += 1;
			} else if token.ty == TokenLeftBrac {
				level -= 1;
			}
			v.push(token);
		}
		error(get_path(program_id), line, &format!("unclonsed macro arguments at {:?}...", &self.input[self.pos..self.pos+5]));
		panic!("");
	}
	fn read_args(&mut self) -> Vec<Vec<Token>> {
		let mut v = vec![];
		if self.consume_ty(TokenLeftBrac) {
			return v;
		}
		v.push(self.read_arg());
		while !self.consume_ty(TokenLeftBrac) {
			self.assert_ty(TokenComma);
			v.push(self.read_arg());
		}
		return v;
	}
	fn apply_objlike(&mut self, mut m: Macro) {
		// self.input[self.pos-1] = define identifier
		let num = self.input[self.pos-1].line;
		let program_id = self.input[self.pos-1].program_id;
		let body = std::mem::replace(&mut m.body, vec![]);
		for token in body {
			if self.add_special_macro(&token, num, program_id) {
				continue;
			}
			self.output.push(token);
		}
		return;
	}
	fn apply_funclike(&mut self, m: Macro, name: String) {
		let mut args;
		// self.input[self.pos-1] = define identifier
		let line = self.input[self.pos-1].line;
		let program_id = self.input[self.pos-1].program_id;
		
		self.assert_ty(TokenRightBrac);
		args = self.read_args();
		if args.len() != m.params.unwrap().len() {
			error(get_path(program_id), line, &format!("number of parameter does not match at {}", name));
		}
		for token in m.body {
			
			if self.add_special_macro(&token, line, program_id) {
				continue;
			}
			
			if token.ty == TokenParam(false) {
				self.output.append(&mut args[token.val as usize].clone());
				continue;
			} else if token.ty == TokenParam(true) {
				self.output.push(stringize(args[token.val as usize].clone()));
				continue;
			}
			
			self.output.push(token);
		}
	}
	fn apply(&mut self, m: Macro, name: String) {
		// OBJLIKE
		if m.ty == MacroType::ObjLike {
			self.apply_objlike(m);
			return;
		} else {
		// FUNCLIKE
			self.apply_funclike(m, name);
			return;
		}
	}
	fn add_special_macro(&mut self, token: &Token, line: usize, program_id: usize) -> bool {
		if is_ident(token, "__LINE__") {
			self.output.push(Macro::new_num(line as i32, program_id, token.pos, token.end));
			return true;
		}
		return false;
	}
}

#[derive(PartialEq, Clone, Debug)]
enum MacroType {
	ObjLike,
	FunLike,
}

impl Default for MacroType {
	fn default() -> Self {
		MacroType::ObjLike
	}
}

#[derive(Default, Clone, Debug)]
struct Macro {
	ty: MacroType,
	params: Option<Vec<String>>,
	body: Vec<Token>,
}

impl Macro {
	fn new(ty: MacroType, params: Option<Vec<String>>, body: Vec<Token>) -> Self {
		Self {
			ty, 
			params, 
			body,
		}
	}
	fn new_num(num: i32, program_id: usize, pos: usize, end: usize) -> Token {
		return Token::new(TokenNum, num, program_id, pos, end, num as usize);
	}
	fn new_param(n: i32, stringize: bool, program_id: usize, pos: usize, end: usize, line: usize) -> Token {
		return Token::new(TokenParam(stringize), n, program_id, pos, end, line);
	}
	fn define_funclike(env: &mut Env, name: String) {

		let mut params = vec![];
		loop {
			let name = env.ident();
			params.push(name);
			if env.consume_ty(TokenLeftBrac) {
				break;
			}
			env.assert_ty(TokenComma);
		}
		let body = env.read_until_eol();
		let mut m = Macro::new(MacroType::FunLike, Some(params), body);
		m.replace_macro_params();
		m.replace_hash_ident();
		env.defined.insert(name, m);
	}
	// Replaces macro parameter tokens with TK_PARAM tokens.
	fn replace_macro_params(&mut self) {
		// Associate argument with index
		let mut map_params = HashMap::new();
		let params_len = self.params.as_ref().unwrap().len();
		for i in 0..params_len {
			map_params.insert(self.params.as_ref().unwrap()[i].clone(), i);
		}
		// Replace actual argument with index
		for token in &mut self.body {
			if let TokenIdent = token.ty {
				let name = String::from(&PROGRAMS.lock().unwrap()[token.program_id][token.pos..token.pos+token.val as usize]);
				if let Some(parami) = map_params.get(&name) {
					std::mem::replace(token, Macro::new_param(*parami as i32, false, token.program_id, token.pos, token.end, token.line));
				}
			}
		}
	}
	// Replace '#' followed by a macro parameter
	fn replace_hash_ident(&mut self) {
		let mut v = vec![];
		let mut i = 0;
		loop {
			if i >= self.body.len()-1 {
				break;
			}
			match (&self.body[i].ty, &self.body[i+1].ty) {
				(TokenSharp, TokenParam(_)) => {
					v.push(Macro::new_param(self.body[i+1].val, true, self.body[i+1].program_id, self.body[i+1].pos, self.body[i+1].end, self.body[i+1].line));
					i += 1;
				}
				_ => {
					v.push(self.body[i].clone());
				}
			}
			i += 1;
		}
		if i == self.body.len()-1 {
			v.push(self.body.last().unwrap().clone());
		}
		self.body = v;
	}
	fn define_objlike(env: &mut Env, name: String) {
		let body = env.read_until_eol();
		let m = Macro::new(MacroType::ObjLike, None, body);
		env.defined.insert(name, m);
	}
	fn default_judge(&self) -> bool {
		return self.body.is_empty();
	}
}

fn stringize(tokens: Vec<Token>) -> Token {
	let mut sb = String::new();
	let start = tokens[0].pos;
	let program_id = tokens[0].program_id;
	let line = tokens[0].line;
	let mut end = start;
	for token in tokens {
		sb.push_str(&String::from(&PROGRAMS.lock().unwrap()[program_id][token.pos..token.end]));
		sb.push(' ');
		end += token.end-token.pos+1;
	}
	sb.pop();
	end -= 1;
	return Token::new(TokenString(sb), 0, program_id, start, end, line);
}

fn is_ident(token: &Token, s: &str) -> bool {
	let name = String::from(&PROGRAMS.lock().unwrap()[token.program_id][token.pos..token.end]);
	return token.ty == TokenIdent && &name == s;
}

pub fn get_path(program_id: usize) -> Option<String> {
	return Some(PATH.lock().unwrap().get(&program_id).unwrap().clone());
}

pub fn add_program(path: String) {
	match read_file(&path[..]) {
		Ok(content) => {
			let mut program = content;
			remove_backslash_or_crlf_newline(&mut program);
			PROGRAMS.lock().unwrap().push(program.clone());
		}
		Err(_) => {
			println!("failed to read file.");
			std::process::exit(1);
		}
	}
}

pub fn preprocess(tokens: Vec<Token>) -> Vec<Token> {
	
	let mut env = Env::new(tokens, None);

	while !env.eof() {
		
		// ident
		if let TokenIdent = env.input[env.pos].ty {
			let token = env.input[env.pos].clone();
			let name = String::from(&PROGRAMS.lock().unwrap()[token.program_id][token.pos..token.pos+token.val as usize]);
			let mut m: Macro = Default::default();
			env.pos += 1;
			if let Some(m2) = env.defined.get(&name) {
				m = m2.clone();
			}
			if m.default_judge() {
				env.output.push(token);
			} else {
				env.apply(m, name);
			}
			continue;
		}
		// #
		if let TokenSharp = env.input[env.pos].ty {
			env.pos += 1;
		} else {
			let token = env.input[env.pos].clone();
			env.pos += 1;
			env.output.push(token);
			continue;
		}
		// define
		if let TokenDefine = env.input[env.pos].ty {
			env.pos += 1;
			env.define();
			continue;
		}
		// include
		if let TokenInclude = env.input[env.pos].ty {
			env.pos += 1;
			env.include();
			continue;
		}
		let token = &env.input[env.pos];
		let program_id = token.program_id;
		let line = token.line;
		error(get_path(program_id), line, &format!("macro expected at {}...", &PROGRAMS.lock().unwrap()[env.input[env.pos].program_id][env.input[env.pos].pos..env.input[env.pos].pos+5]));
	}
	
	return env.output;
}