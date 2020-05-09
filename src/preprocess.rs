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
	pub defined: HashMap<String, Macro>,
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
		assert!(self.consume_ty(ty));
	}
	fn define(&mut self) {
		let name = self.ident();
		if self.consume_ty(TokenRightBrac) {
			Macro::funclike_macro(self, name);
		} else {
			Macro::objlike_macro(self, name);
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
	fn ident(&mut self) -> String {
		match self.input[self.pos].ty {
			TokenIdent => {
				let name = String::from(&PROGRAMS.lock().unwrap()[self.input[self.pos].program_id][self.input[self.pos].pos..self.input[self.pos].pos+self.input[self.pos].val as usize]);
				self.pos += 1;
				return name;
			}
			_ => {
				error(&format!("macro name expected."));
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
		let mut token;
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
		error(&format!("unclonsed macro arguments at {:?}...", &self.input[self.pos..self.pos+5]));
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
	fn apply(&mut self, mut m: Macro) {
		// OBJLIKE
		if m.ty == MacroType::ObjLike {
			let mut body = std::mem::replace(&mut m.body, vec![]);
			self.output.append(&mut body);
			return;
		}
		// FUNLIKE
		let mut args;
		self.assert_ty(TokenRightBrac);
		args = self.read_args();
		if args.len() != m.params.unwrap().len() {
			error(&format!("number of parameter does not match."));
		}
		for token in m.body {
			if token.ty == TokenParam(false) {
				self.output.append(&mut args[token.val as usize].clone());
			} else if token.ty == TokenParam(true) {
				self.output.push(stringize(args[token.val as usize].clone()));
			} else {
				self.output.push(token);
			}
		}
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
	fn new_param(n: i32, stringize: bool) -> Token {
		return Token::new(TokenParam(stringize), n, 0, 0);
	}
	fn funclike_macro(ctx: &mut Context, name: String) {

		let mut params = vec![];
		while !ctx.consume_ty(TokenLeftBrac) {
			let name = ctx.ident();
			params.push(name);
			ctx.assert_ty(TokenComma);
		}
		let body = ctx.read_until_eol();
		let mut m = Macro::new(MacroType::FunLike, Some(params), body);
		m.replace_params();
		ctx.defined.insert(name, m);
	}
	fn replace_params(&mut self) {
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
					std::mem::replace(token, Macro::new_param(*parami as i32, false));
				}
			}
		}
		let mut v = vec![];
		// Process '#' followed by a macro parameter
		let mut last = false;
		let mut i = 0;
		loop {
			if i < self.body.len()-1 {
				break;
			}
			match (&self.body[i].ty, &self.body[i+1].ty) {
				(TokenSharp, TokenParam(_)) => {
					last = true;
					v.push(Macro::new_param(self.body[i+1].val, true));
					i += 1;
				}
				_ => {
					last = false;
					v.push(self.body[i].clone());
				}
			}
			i += 1;
		}
		if !last {
			v.push(self.body.last().unwrap().clone());
		}
		self.body = v;
	}
	fn objlike_macro(ctx: &mut Context, name: String) {
		let body = ctx.read_until_eol();
		let m = Macro::new(MacroType::ObjLike, None, body);
		ctx.defined.insert(name, m);
	}
	fn default_judge(&self) -> bool {
		return self.body.is_empty();
	}
}

fn stringize(tokens: Vec<Token>) -> Token {
	let mut sb = String::new();
	let start = tokens[0].pos;
	let program_id = tokens[0].program_id;
	for token in tokens {
		sb.push_str(&String::from(&PROGRAMS.lock().unwrap()[program_id][token.pos..token.pos+token.val as usize]));
		sb.push(' ');
	}
	sb.pop();
	return Token::new(TokenString(sb), 0, program_id, start);
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
			let name = String::from(&PROGRAMS.lock().unwrap()[token.program_id][token.pos..token.pos+token.val as usize]);
			let mut m: Macro = Default::default();
			if let Some(m2) = ctx.defined.get(&name) {
				m = m2.clone();
			}
			if m.default_judge() {
				ctx.output.push(token);
			} else {
				ctx.apply(m);
			}
			ctx.pos += 1;
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