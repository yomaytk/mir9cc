static register: [&str; 9] = ["rdi", "rsi", "r10", "r11", "r12", "r13", "r14", "r15", "NULL"];

use NodeType::*;
use TokenType::*;

#[derive(Debug, PartialEq, Clone)]
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

#[allow(dead_code)]
impl NodeType {
	fn bit_new(tk_ty: TokenType) -> Self {
		NodeType::BinaryTree(tk_ty, None, None)
	}

	fn bit_init(tk_ty: TokenType, lhs: Node, rhs: Node) -> Self {
		NodeType::BinaryTree(tk_ty, Some(Box::new(lhs)), Some(Box::new(rhs)))
	}
}

pub struct Node {
	pub val: i32,
	pub ty: NodeType,
}

#[allow(dead_code)]
impl Node {
	pub fn new(tk_ty: TokenType) -> Self {
		Self {
			val: 0,
			ty: NodeType::bit_new(tk_ty),
		}
	}

	pub fn bit_init(tk_ty: TokenType, lhs: Node, rhs: Node) -> Self {
		Self {
			val: 0,
			ty: NodeType::bit_init(tk_ty, lhs, rhs),
		}
	}
	
	pub fn new_node_num(val: i32) -> Self {
		Self {
			val: val,
			ty: NodeType::NodeNum,
		}
	}

	pub fn gen(&self, regid: usize) -> &str {
		let dst;
		let src;
		
		match &self.ty {
			NodeNum => {
				let reg = register[regid];
				println!("\tmov {}, {}", reg, self.val);
				return reg;
			},
			BinaryTree(_, ref lhs, ref rhs) => {
				dst = lhs.as_ref().unwrap().gen(regid);
				src = rhs.as_ref().unwrap().gen(regid+1);
			},
		}
		
		match &self.ty {
			BinaryTree(TokenPlus, _, _) => {
				println!("\tadd {}, {}", dst, src);
				return dst;
			},
			BinaryTree(TokenMinus, _, _) => {
				println!("\tsub {}, {}", dst, src);
				return dst;
			}
			_ => {
				panic!("gen error.");
			}
		}
	}
	
}