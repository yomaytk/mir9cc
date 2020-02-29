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

#[allow(dead_code)]
pub enum NodeType {
	NodeNum,
	BinaryTree(TokenType, Option<Box<Node>>, Option<Box<Node>>),
}

impl NodeType {
	fn bit_new(tk_ty: TokenType) -> Self {
		NodeType::BinaryTree(tk_ty, None, None)
	}
}

pub struct Node {
	pub val: i32,
	pub ty: NodeType,
}

impl Node {
	pub fn new(tk_ty: TokenType) -> Self {
		Self {
			val: 0,
			ty: NodeType::bit_new(tk_ty),
		}
	}
	
	pub fn new_node_num(val: i32) -> Self {
		Self {
			val: val,
			ty: NodeType::NodeNum,
		}
	}
}