use TokenType::*;
use std::collections::HashMap;
use std::sync::Mutex;
use super::lib::*;
use super::preprocess::*;

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
	pub static ref PROGRAMS: Mutex<Vec<String>> = Mutex::new(Vec::new());
	pub static ref ESCAPED: Mutex<HashMap<char, char>> = Mutex::new(hash![
		// ('a', "\\a"), ('b', "\\b"), ('f', "\\f"),
		('n', '\n'), ('r', '\r'), // ('v', "\\v"),
		('t', '\t') // ('e', '\033'), ('E', '\033')
	]);
	pub static ref LINE: Mutex<usize> = Mutex::new(1);
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
	Signal::new("#", TokenSharp),
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
	TokenSharp,
	TokenInclude,
	TokenDefine,
	TokenNewLine,
	TokenParam(bool),	// TokenParam(stringize)
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
			"include" => { TokenInclude }
			"define" => { TokenDefine }
			_ => { TokenIdent }
		}
	}
}

#[derive(Debug, Clone)]
pub struct Token {
	pub ty: TokenType,
	pub val: i32,
	pub program_id: usize,
	pub pos: usize,
	pub end: usize,
	pub line: usize,
}

impl Token {
	pub fn new(ty: TokenType, val: i32, program_id: usize, pos: usize, end: usize, line: usize) -> Token {
		Token {
			ty,
			val,
			program_id,
			pos,
			end,
			line
		}
	}
	pub fn getstring(&self) -> String {
		match &self.ty {
			TokenString(sb) => { return sb.clone(); }
			_ => { panic!("{:?}", self); }
		}
	}
}

pub struct TokenSet {
	pub tokens: Vec<Token>,
	pub pos: usize
}

impl TokenSet {
	pub fn new(tokens: Vec<Token>) -> Self {
		Self {
			tokens,
			pos: 0
		}
	}
	pub fn assert_ty(&mut self, ty: TokenType) {
		let pos = self.pos;
		if !self.consume_ty(ty) {
			// error(&format!("assertion failed at: {}", &self.input[..self.val as usize]));
			// for debug.
			panic!("assertion failed at: {}..", &PROGRAMS.lock().unwrap()[self.tokens[pos].program_id][pos..pos+self.tokens[pos].val as usize]);
		}
	}
	pub fn consume_ty(&mut self, ty: TokenType) -> bool {
		let token = &self.tokens[self.pos];
		match (&token.ty, &ty) {
			(TokenString(_), TokenString(_)) => {
				return true;
			}
			_ => {
				if token.ty == ty {
					self.pos += 1;
					return true;
				} else {
					return false;
				}
			}
		}
	}
	pub fn is_typename(&mut self) -> bool {
		let token = &self.tokens[self.pos];
		if token.ty == TokenInt {
			self.pos += 1;
			return true;
		}
		return false;
	}
	pub fn getstring(&self) -> String {
		let token = &self.tokens[self.pos];
		match &token.ty {
			TokenString(sb) => { return sb.clone(); }
			_ => { panic!("{:?}", token); }
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

pub fn read_file(filename: &str) -> Result<String, Box<dyn std::error::Error>>{
	let content = std::fs::read_to_string(filename)?;
	return Ok(content);
}

fn read_string (p: &mut core::str::Chars, program_id: usize, pos: &mut usize) -> Token {

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
			error(get_path(program_id), *LINE.lock().unwrap(), "premature end of input.");
		} else if let Some(c3) = ESCAPED.lock().unwrap().get(&c2) {
			sb.push(*c3);
		} else {
			sb.push(c2.clone());
		}
		*pos += 2;
	}
	return Token::new(TokenString(sb), 0, program_id, start, *pos, *LINE.lock().unwrap());
}

fn read_char (p: &mut core::str::Chars, program_id: usize, pos: &mut usize) -> Token {
	
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
				error(get_path(program_id), *LINE.lock().unwrap(), "premature end of input.");
			}
		}
	} else {
		error(get_path(program_id), *LINE.lock().unwrap(), "unclosed char literal.");
	}
	assert!(p.next().unwrap() == '\'');
	*pos += 1;
	return Token::new(TokenNum, val, program_id, start, *pos, *LINE.lock().unwrap());
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

fn block_comment(p: &mut core::str::Chars, program_id: usize, pos: &mut usize) {
	let start = *pos;
	*pos += 2;
	let mut pp = p.clone();
	pp.next();
	loop {
		if let Some(c) = pp.next() {
			*pos += 1;
			if c == '*' && &PROGRAMS.lock().unwrap()[program_id][*pos..*pos+1] == "/" {
				*pos += 1;
				break;
			}
		} else {
			error(get_path(program_id), *LINE.lock().unwrap(), "premature end of input.");
		}
	}
	for _ in 0..(*pos - start)-1 {
		p.next();
	}
	return;
}

fn signal(p: &mut core::str::Chars, program_id: usize, pos: &mut usize, input: &str) -> Option<Token> {
	for signal in &SIGNALS[..] {
		let len = signal.name.len();
		if input.len() >= *pos+len && *signal.name == input[*pos..*pos+len] {
			let token = Token::new(signal.ty.clone(), len as i32, program_id, *pos, *pos+len, *LINE.lock().unwrap());
			*pos += len;
			for _ in 0..len-1 { p.next(); }
			return Some(token);
		}
	}
	return None;
}

fn ident(p: &mut core::str::Chars, program_id: usize, pos: &mut usize, c: char) -> Token {
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
	*pos += 1;
	let token = Token::new(TokenType::from(ident), len, program_id, possub, *pos, *LINE.lock().unwrap());
	return token;
}

fn number(p: &mut core::str::Chars, program_id: usize, pos: &mut usize, input: &str, c: char) -> Token {

	if c == '0' && (&input[*pos+1..*pos+2] == "X" || &input[*pos+1..*pos+2] == "x") {
		*pos += 2;
		p.next();
		return hexadecimal(p, program_id, pos, input);
	}
	
	if c == '0' {
		*pos += 1;
		return octal(p, program_id, pos);
	}

	*pos += 1;
	return decimal(p, program_id, pos, c);
}

fn hexadecimal(p: &mut core::str::Chars, program_id: usize, pos: &mut usize, input: &str) -> Token{

	let mut pp = p.clone();
	let mut ishex = false;
	let mut num = 0;
	let possub = *pos;

	while let Some(c) = pp.next() {
		match (ishex, c) {
			(_, '0' ..= '9') => { p.next(); num = num * 16 + c as i32 - '0' as i32; }
			(_, 'a' ..= 'f') => { p.next(); num = num * 16 + c as i32 - 'a' as i32 + 10; }
			(_, 'A' ..= 'F') => { p.next(); num = num * 16 + c as i32 - 'A' as i32 + 10; }
			(true, _) => { break; }
			(false, _) => { error(get_path(program_id), *LINE.lock().unwrap(), &format!("bad hexadecimal number at {}..", &input[*pos..*pos+5])); }
		}
		ishex = true;
		*pos += 1;
	}

	return Token::new(TokenNum, num, program_id, possub-2, *pos, *LINE.lock().unwrap());
}

fn decimal(p: &mut core::str::Chars, program_id: usize, pos: &mut usize, c: char) -> Token{

	let mut pp = p.clone();
	let possub = *pos;
	let mut num = c as i32 - '0' as i32;

	while let Some(c) = pp.next() {
		if let '0' ..= '9' = c {
			num = num * 10 + c as i32 - '0' as i32;
			p.next();
			*pos += 1;
			continue;
		}
		break;
	}

	return Token::new(TokenNum, num, program_id, possub-1, *pos, *LINE.lock().unwrap());
}

fn octal(p: &mut core::str::Chars, program_id: usize, pos: &mut usize) -> Token{

	let mut pp = p.clone();
	let possub = *pos;
	let mut num = 0;

	while let Some(c) = pp.next() {
		if let '0' ..= '9' = c {
			num = num * 8 + c as i32 - '0' as i32;
			p.next();
			*pos += 1;
			continue;
		}
		break;
	}

	return Token::new(TokenNum, num, program_id, possub-1, *pos, *LINE.lock().unwrap());
}

pub fn remove_backslash_or_crlf_newline(input: &mut String) {
	let mut i = 0;
	loop {
		match (input.get(i..i+1), input.get(i+1..i+2)) {
			(Some("\\"), Some("\n")) => {
				input.remove(i);
				input.remove(i);
				continue;
			}
			(Some("\r"), Some("\\")) => {
				input.remove(i);
				continue;
			}
			(Some(_), _) => {
				i += 1;
			}
			(None, _) => {
				break;
			}
		}
	}
}

fn strip_newline_tokens(tokens: Vec<Token>) -> Vec<Token> {
	let mut v = Vec::new();
	for ref mut token in tokens {
		let token = std::mem::replace(token, NONE_TOKEN.clone());
		if let TokenNewLine = token.ty {
			continue;
		}
		v.push(token);
	}
	return v;
}

pub fn scan(program_id: usize, add_eof: bool) -> Vec<Token> {
	
	let mut tokens: Vec<Token> = vec![];
	let mut pos = 0;
	let input = PROGRAMS.lock().unwrap()[program_id].clone();
	let mut p = input.chars();
	
	while let Some(c) = p.next() {

		// \n
		if c == '\n' {
			tokens.push(Token::new(TokenNewLine, 0, program_id, pos, pos+1, *LINE.lock().unwrap()));
			pos += 1;
			*LINE.lock().unwrap() += 1;
			continue;
		}

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
			block_comment(&mut p, program_id, &mut pos);
			continue;
		}

		// char literal
		if c == '\'' {
			pos += 1;
			tokens.push(read_char(&mut p, program_id, &mut pos));
			continue;
		}

		// string literal
		if c == '"' {
			pos += 1;
			let mut string_token = read_string(&mut p, program_id, &mut pos);
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
		if let Some(token) = signal(&mut p, program_id, &mut pos, &input) {
			tokens.push(token);
			continue;
		}

		// ident
		if c.is_alphabetic() || c == '_' {
			tokens.push(ident(&mut p, program_id, &mut pos, c));
			continue;
		}
		
		// number
		if c.is_digit(10) {
			tokens.push(number(&mut p, program_id, &mut pos, &input, c));
			continue;
		}

		error(get_path(program_id), *LINE.lock().unwrap(), &format!("cannot scan at {}", &input[pos..]));
	}

	// guard
	if add_eof {
		let token = Token::new(TokenEof, 0, program_id, pos, pos, *LINE.lock().unwrap());
		tokens.push(token);
	}
	
	return tokens;
}

pub fn tokenize(program_id: usize, add_eof: bool) -> Vec<Token> {
	*LINE.lock().unwrap() = 1;
	let tokens = scan(program_id, add_eof);
	let tokens = preprocess(tokens);
	let tokens = strip_newline_tokens(tokens);
	return tokens;
}