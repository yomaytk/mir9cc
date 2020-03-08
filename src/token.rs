use std::process;
use TokenType::*;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
	TokenNum,
	TokenAdd,
	TokenSub,
	TokenMul,
	TokenDiv,
	TokenRet,
	TokenSemi,
	TokenIdent,
	TokenEq,
	TokenRightBrac,
	TokenLeftBrac,
	TokenIf,
	TokenElse,
	TokenComma,
	TokenNoSignal,
	TokenEof,
}

impl From<String> for TokenType {
	fn from(s: String) -> Self {
		match &s[..] {
			"return" => { TokenRet }
			"if" => { TokenIf }
			"else" => { TokenElse }
			_ => { TokenIdent }
		}
	}
}

#[derive(Debug)]
pub struct Token<'a> {
	pub ty: TokenType,
	pub val: i32,
	pub input: &'a str,
}

impl<'a> Token<'a> {
	pub fn new(ty: TokenType, val: i32, input: &'a str) -> Token<'a> {
		Token {
			ty: ty,
			val: val,
			input: input,
		}
	}
	pub fn consume(&self, c: &str, pos: &mut usize) -> bool {
		if self.input[..self.val as usize] == *c {
			*pos += 1;
			return true;
		}
		return false;
	}
	pub fn expect(&self, c: &str, pos: &mut usize) -> bool {
		if self.consume(c, pos) {
			return true;
		}
		panic!("expect fun error: {} is expected, but got {}", c, &self.input[..self.val as usize]);
	}
	pub fn assert_ty(&self, c: &str, pos: &mut usize) {
		assert!(self.consume(c, pos));
	}
}

// return next number
fn strtol(p: &mut core::str::Chars, pos: &mut usize, c: char) -> i32 {

	let mut pp = p.clone();
	let mut num_str = String::from("");
	num_str.push(c);

	while let Some(c) = pp.next() {
		if c.is_ascii_digit() {
			num_str.push(c);
			p.next();
			*pos += 1;
			continue;
		}
		break;
	}
	num_str.parse::<i32>().unwrap()
}

// return TokenType of given character
fn signal2token (p: char) -> TokenType {
	if p == '+' { TokenAdd }
	else if p == '-' { TokenSub }
	else if p == '*' { TokenMul }
	else if p == '/' { TokenDiv }
	else if p == ';' { TokenSemi }
	else if p == '=' { TokenEq }
	else if p == '(' { TokenRightBrac }
	else if p == ')' { TokenLeftBrac }
	else if p == ',' { TokenComma }
	else { TokenNoSignal }
}

pub fn tokenize(input: &String) -> Vec<Token> {
	
	let mut tokens: Vec<Token> = vec![];
	let mut pos = 0;
	let mut p = input.chars();

	while let Some(c) = p.next() {

		// space
		if c.is_whitespace() {
			pos += 1;
			continue;
		}
		
		// operator or signal
		if signal2token(c) != TokenNoSignal {
			let token = Token::new(signal2token(c), 1, &input[pos..]);
			tokens.push(token);
			pos += 1;
			continue;
		}

		// ident
		if c.is_alphabetic() || c == '_' {
			let mut ident = String::new();
			ident.push(c);
			let mut len = 1;
			let mut pp = p.clone();
			let possub = pos;
			loop {
				if let Some(cc) = pp.next() {
					if !cc.is_alphabetic() && !cc.is_ascii_digit() && cc != '_'{
						break;
					}
					p.next();
					ident.push(cc);
					len += 1;
					pos += 1;
				}
			}
			let token = Token::new(TokenType::from(ident), len, &input[possub..]);
			tokens.push(token);
			pos += 1;
			continue;
		}
		
		// number
		if c.is_digit(10) {
			let possub = pos;
			let num = strtol(&mut p, &mut pos, c);
			let token = Token::new(TokenNum, num, &input[possub..]);
			tokens.push(token);
			pos += 1;
			continue;
		}

		eprintln!("cannot tokenize.");
		process::exit(1);
	}

	// guard
	let token = Token::new(TokenEof, 0, &input[pos..]);
	tokens.push(token);
	
	tokens
}