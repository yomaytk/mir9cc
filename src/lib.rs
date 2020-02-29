#[derive(Debug, PartialEq)]
pub enum TokenType {
	TokenNum,
	TokenPlus,
	TokenMinus,
	TokenEof,
}

pub struct Token {
	pub ty: TokenType,
	pub val: i32,
	pub input: usize,
}

impl Token {
	pub fn new(ty: TokenType, val: i32, input: usize) -> Token {
		Token {
			ty: ty,
			val: val,
			input: input,
		}
	}
}

