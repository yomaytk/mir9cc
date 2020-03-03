use std::process;
use TokenType::*;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
	TokenNum,
	TokenPlus,
	TokenMinus,
	TokenMul,
	TokenDiv,
	TokenRet,
	TokenSemi,
	TokenNoSignal,
	TokenEof,
}

impl From<String> for TokenType {
	fn from(s: String) -> Self {
		match &s[..] {
			"return" => { TokenRet }
			_ => { panic!("{} is not defined.", &s[..]); }
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
	if p == '+' { TokenPlus }
	else if p == '-' { TokenMinus }
	else if p == '*' { TokenMul }
	else if p == '/' { TokenDiv }
	else if p == ';' { TokenSemi }
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
			let token = Token::new(signal2token(c), -1, &input[pos..]);
			tokens.push(token);
			pos += 1;
			continue;
		}

		// keyword
		if c.is_alphabetic() || c == '_' {
			let mut ident = String::new();
			ident.push(c);
			loop {
				if let Some(cc) = p.next() {
					if !cc.is_alphabetic(){
						break;
					}
					ident.push(cc);
					continue;
				}
			}
			let token = Token::new(TokenType::from(ident), -1, &input[pos..]);
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