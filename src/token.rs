use TokenType::*;
use std::collections::HashMap;
use std::sync::Mutex;
use super::lib::*;

// Atomic unit in the grammar is called "token".
// For example, `123`, `"abc"` and `while` are tokens.
// The tokenizer splits an input string into tokens.
// Spaces and comments are removed by the tokenizer.

macro_rules! hash {
	( $( $t:expr),* ) => {
		{
			let mut temp_hash = HashMap::new();
			$(
				temp_hash.insert($t.0, $t.1);
			)*
			temp_hash
		}
	};
}


lazy_static! {
	pub static ref ESCAPED: Mutex<HashMap<char, char>> = Mutex::new(hash![
		// ('a', "\\a"), ('b', "\\b"), ('f', "\\f"),
		('n', '\n'), ('r', '\r'), // ('v', "\\v"),
		('t', '\t') // ('e', '\033'), ('E', '\033')
	]); 
}


pub static SIGNALS: &[Signal] = &[
	Signal::new("<<=", TokenShlEq),
	Signal::new(">>=", TokenShrEq),
	Signal::new("&&", TokenLogAnd),
	Signal::new("||", TokenLogOr),
	Signal::new("==", TokenEqEq),
	Signal::new("!=", TokenNe),
	Signal::new("->", TokenArrow),
	Signal::new("<=", TokenLe),
	Signal::new(">=", TokenGe),
	Signal::new("<<", TokenShl),
	Signal::new(">>", TokenShr),
	Signal::new("++", TokenInc),
	Signal::new("--", TokenDec),
	Signal::new("+=", TokenAddEq),
	Signal::new("-=", TokenSubEq),
	Signal::new("*=", TokenMulEq),
	Signal::new("/=", TokenDivEq),
	Signal::new("%=", TokenModEq),
	Signal::new("&=", TokenAndEq),
	Signal::new("|=", TokenOrEq),
	Signal::new("^=", TokenXorEq),
	Signal::new("+", TokenAdd),
	Signal::new("-", TokenSub),
	Signal::new("*", TokenStar),
	Signal::new("/", TokenDiv),
	Signal::new(";", TokenSemi),
	Signal::new("=", TokenEq),
	Signal::new("(", TokenRightBrac),
	Signal::new(")", TokenLeftBrac),
	Signal::new(",", TokenComma),
	Signal::new("{", TokenRightCurlyBrace),
	Signal::new("}", TokenLeftCurlyBrace),
	Signal::new("<", TokenLt),
	Signal::new(">", TokenRt),
	Signal::new("[", TokenRightmiddleBrace),
	Signal::new("]", TokenLeftmiddleBrace),
	Signal::new("&", TokenAmpersand),
	Signal::new(".", TokenDot),
	Signal::new("!", TokenNot),
	Signal::new(":", TokenColon),
	Signal::new("?", TokenQuestion),
	Signal::new("|", TokenOr),
	Signal::new("^", TokenXor),
	Signal::new("%", TokenMod),
	Signal::new("~", TokenTilde),
];

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
	TokenNum,
	TokenAdd,
	TokenSub,
	TokenStar,
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
	TokenRightCurlyBrace,
	TokenLeftCurlyBrace,
	TokenLogAnd,
	TokenLogOr,
	TokenLt,
	TokenRt,
	TokenRightmiddleBrace,
	TokenLeftmiddleBrace,
	TokenAmpersand,
	TokenSizeof,
	TokenFor,
	TokenInt,
	TokenChar,
	TokenDoubleQuo,
	TokenString(String),
	TokenEqEq,
	TokenNe,
	TokenDo,
	TokenWhile,
	TokenExtern,
	TokenAlignof,
	TokenStruct,
	TokenDot,
	TokenArrow,
	TokenTypedef,
	TokenVoid,
	TokenNot,
	TokenQuestion,
	TokenColon,
	TokenOr,
	TokenXor,
	TokenLe,
	TokenGe,
	TokenShl,
	TokenShr,
	TokenMod,
	TokenInc,
	TokenDec,
	TokenBreak,
	TokenAddEq,
	TokenSubEq,
	TokenMulEq,
	TokenDivEq,
	TokenModEq,
	TokenShlEq,
	TokenShrEq,
	TokenAndEq,
	TokenOrEq,
	TokenXorEq,
	TokenTilde,
	TokenNoSignal,
	TokenEof,
}

impl From<String> for TokenType {
	fn from(s: String) -> Self {
		match &s[..] {
			"return" => { TokenRet }
			"if" => { TokenIf }
			"else" => { TokenElse }
			"for" => { TokenFor }
			"int" => { TokenInt }
			"sizeof" => { TokenSizeof }
			"char" => { TokenChar }
			"do" => { TokenDo }
			"while" => { TokenWhile }
			"extern" => { TokenExtern }
			"_Alignof" => { TokenAlignof }
			"struct" => { TokenStruct }
			"typedef" => { TokenTypedef }
			"void" => { TokenVoid }
			"break" => { TokenBreak }
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
	pub fn assert_ty(&self, ty: TokenType, pos: &mut usize) {
		if !self.consume_ty(ty, pos) {
			// error(&format!("assertion failed at: {}", &self.input[..self.val as usize]));
			// for debug.
			panic!("assertion failed at: {}", &self.input[..self.val as usize]);
		}
	}
	pub fn consume_ty(&self, ty: TokenType, pos: &mut usize) -> bool {
		if self.ty == ty { 
			*pos += 1; 
			return true; 
		} else {
			match (&self.ty, ty) {
				(TokenString(_), TokenString(_))  => { return true; }
				_ => { return false; }
			}
		}
	}
	pub fn is_typename(&self, pos: &mut usize) -> bool {
		if self.ty == TokenInt {
			*pos += 1;
			return true;
		}
		return false;
	}
	pub fn getstring(&self) -> String {
		match &self.ty {
			TokenString(sb) => { return sb.clone(); }
			_ => { panic!("{:?}", self); }
		}
	}
}

pub struct Signal {
	pub name: &'static str,
	pub ty: TokenType
}

impl Signal {
	const fn new(name: &'static str, ty: TokenType) -> Self {
		Self {
			name,
			ty,
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

fn read_string<'a> (p: &mut core::str::Chars, pos: &mut usize, input: &'a str) -> Token<'a> {

	let start = *pos;
	let mut sb = String::new();

	while let Some(c) = p.next() {
		if c == '"'	{
			*pos += 1;
			break;
		}
		if c != '\\' {
			sb.push(c);
			*pos += 1;
			continue;
		}
		let c2 = p.next().unwrap();
		if c2 == '\0' {
			error("premature end of input.");
		} else if let Some(c3) = ESCAPED.lock().unwrap().get(&c2) {
			sb.push(*c3);
		} else {
			sb.push(c2.clone());
		}
		*pos += 2;
	}
	return Token::new(TokenString(sb), 0, &input[start..]);
}

fn read_char<'a> (p: &mut core::str::Chars, pos: &mut usize, input: &'a str) -> Token<'a> {
	
	let mut val = 0;
	let start = *pos;

	if let Some(c) = p.next() {
		*pos += 1;
		if c != '\\' {
			val = c as i32;
		} else {
			if let Some(c2) = p.next() {
				*pos += 1;
				if let Some(c3) = ESCAPED.lock().unwrap().get(&c2) {
					val = *c3 as i32;
				} else {
					val = c2 as i32;
				}
			} else {
				error("premature end of input.");
			}
		}
	} else {
		error("unclosed char literal.");
	}
	assert!(p.next().unwrap() == '\'');
	*pos += 1;
	return Token::new(TokenNum, val, &input[start..]);
}

fn line_comment(p: &mut core::str::Chars, pos: &mut usize) {
	let start = *pos;
	*pos += 2;
	let mut pp = p.clone();
	pp.next();
	while let Some(c) = pp.next() {
		*pos += 1;
		if c == '\n' {
			break;
		}
	}
	for _ in 0..(*pos - start)-1 {
		p.next();
	}
	return;
}

fn block_comment<'a>(p: &mut core::str::Chars, pos: &mut usize, input: &'a str) {
	let start = *pos;
	*pos += 2;
	let mut pp = p.clone();
	pp.next();
	loop {
		if let Some(c) = pp.next() {
			*pos += 1;
			if c == '*' && &input[*pos..*pos+1] == "/" {
				*pos += 1;
				break;
			}
		} else {
			error("premature end of input.");
		}
	}
	for _ in 0..(*pos - start)-1 {
		p.next();
	}
	return;
}

fn signal<'a>(p: &mut core::str::Chars, pos: &mut usize, input: &'a str) -> Option<Token<'a>> {
	for signal in &SIGNALS[..] {
		let len = signal.name.len();
		if input.len() >= *pos+len && *signal.name == input[*pos..*pos+len] {
			let token = Token::new(signal.ty.clone(), len as i32, &input[*pos..]);
			*pos += len;
			for _ in 0..len-1 { p.next(); }
			return Some(token);
		}
	}
	return None;
}

fn ident<'a>(p: &mut core::str::Chars, pos: &mut usize, input: &'a str, c: char) -> Token<'a> {
	let mut ident = String::new();
	ident.push(c);
	let mut len = 1;
	let mut pp = p.clone();
	let possub = *pos;
	loop {
		if let Some(cc) = pp.next() {
			if !cc.is_alphabetic() && !cc.is_ascii_digit() && cc != '_'{
				break;
			}
			p.next();
			ident.push(cc);
			len += 1;
			*pos += 1;
		}
	}
	let token = Token::new(TokenType::from(ident), len, &input[possub..]);
	*pos += 1;
	return token;
}

fn number<'a>(p: &mut core::str::Chars, pos: &mut usize, input: &'a str, c: char) -> Token<'a> {
	let possub = *pos;
	let num = strtol(p, pos, c);
	let token = Token::new(TokenNum, num, &input[possub..]);
	*pos += 1;
	return token;
}

pub fn tokenize(input: &str) -> Vec<Token> {
	
	let mut tokens: Vec<Token> = vec![];
	let mut pos = 0;
	let mut p = input.chars();

	while let Some(c) = p.next() {

		// space
		if c.is_whitespace() {
			pos += 1;
			continue;
		}

		// Line Comment
		if c == '/' && &input[pos+1..pos+2] == "/" {
			line_comment(&mut p, &mut pos);
			continue;
		}

		// Block Comment
		if c == '/' && &input[pos+1..pos+2] == "*" {
			block_comment(&mut p, &mut pos, &input);
			continue;
		}

		// char literal
		if c == '\'' {
			pos += 1;
			tokens.push(read_char(&mut p, &mut pos, &input));
			continue;
		}

		// string literal
		if c == '"' {
			pos += 1;
			let mut string_token = read_string(&mut p, &mut pos, &input);
			if !tokens.is_empty() {
				if let (TokenString(s1), TokenString(s2)) = (&tokens.last().unwrap().ty, &string_token.ty) {
					let s = format!("{}{}", s1, s2);
					tokens.pop();
					string_token.ty = TokenString(s);
					tokens.push(string_token);
					continue;
				}
			}
			tokens.push(string_token);
			continue;
		}
		
		// signal
		if let Some(token) = signal(&mut p, &mut pos, &input) {
			tokens.push(token);
			continue;
		}

		// ident
		if c.is_alphabetic() || c == '_' {
			tokens.push(ident(&mut p, &mut pos, &input, c));
			continue;
		}
		
		// number
		if c.is_digit(10) {
			tokens.push(number(&mut p, &mut pos, &input, c));
			continue;
		}

		error(&format!("cannot scan at {}", &input[pos..]));
	}

	// guard
	let token = Token::new(TokenEof, 0, &input[pos..]);
	tokens.push(token);
	
	tokens
}