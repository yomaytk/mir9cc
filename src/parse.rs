use std::process;
use super::token::*;
use super::token::TokenType::*;

#[allow(dead_code)]
#[derive(Debug)]
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

#[derive(Debug)]
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
}

fn number(p: &Vec<char>, tokens: &Vec<Token>, pos: usize) -> (Node, usize) {
	if tokens[pos].ty == TokenNum {
		return (Node::new_node_num(tokens[pos].val), pos+1);
	}
	eprintln!("number expected, but got {}", p[tokens[pos].input]);
	process::exit(1);
}

fn mul(p: &Vec<char>, tokens: &Vec<Token>, pos: usize) -> (Node, usize) {
	let (mut lhs, mut pos) = number(p, tokens, pos);
	
	loop {
		if tokens[pos].ty != TokenMul && tokens[pos].ty != TokenDiv {
			return (lhs, pos);
		}
		let (rhs, new_pos) = number(p, tokens, pos+1);
		lhs = Node::bit_init(tokens[pos].ty.clone(), lhs, rhs);
		pos = new_pos;
	}

}

pub fn expr(p: &Vec<char>, tokens: &Vec<Token>, pos: usize) -> (Node, usize) {
	let (mut lhs, mut pos) = mul(p, tokens, pos);

	loop {
		if tokens[pos].ty != TokenPlus && tokens[pos].ty != TokenMinus {
			return (lhs, pos);
		}
		let (rhs, new_pos) = mul(p, tokens, pos+1);
		lhs = Node::bit_init(tokens[pos].ty.clone(), lhs, rhs);
		pos = new_pos;
	}
	
}

pub fn parse(p: &Vec<char>, tokens: &Vec<Token>, pos: usize) -> Node {
	
	let (node, pos) = expr(p, tokens, pos);

	if tokens[pos].ty != TokenEof {
		eprintln!("stray token: {}", p[tokens[pos].input]);
	}
	node
}