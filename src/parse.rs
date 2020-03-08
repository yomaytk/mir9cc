use super::token::*;
use super::token::TokenType::*;

#[allow(dead_code)]
#[derive(Debug)]
pub enum NodeType {
	Num,
	BinaryTree(TokenType, Option<Box<Node>>, Option<Box<Node>>),
	Ret(Box<Node>),
	Expr(Box<Node>),
	CompStmt(Vec<Node>),
	Ident(String),
	EqTree(TokenType, Box<Node>, Box<Node>),
	IfThen(Box<Node>, Box<Node>, Option<Box<Node>>),
	Call(String, Vec<Node>)
}

#[allow(dead_code)]
impl NodeType {
	fn bit_new(tk_ty: TokenType) -> Self {
		NodeType::BinaryTree(tk_ty, None, None)
	}

	fn bit_init(tk_ty: TokenType, lhs: Node, rhs: Node) -> Self {
		NodeType::BinaryTree(tk_ty, Some(Box::new(lhs)), Some(Box::new(rhs)))
	}

	fn ret_init(lhs: Node) -> Self {
		NodeType::Ret(Box::new(lhs))
	}

	fn expr_init(lhs: Node) -> Self {
		NodeType::Expr(Box::new(lhs))
	}

	fn stmt_init(stmts: Vec<Node>) -> Self {
		NodeType::CompStmt(stmts)
	}

	fn ident_init(s: String) -> Self {
		NodeType::Ident(s)
	}

	fn eq_init(lhs: Node, rhs: Node) -> Self {
		NodeType::EqTree(TokenEq, Box::new(lhs), Box::new(rhs))
	}
	
	fn if_init(cond: Node, then: Node, elthen: Option<Node>) -> Self {
		match elthen {
			Some(node) => {
				NodeType::IfThen(Box::new(cond), Box::new(then), Some(Box::new(node)))
			}
			None => {
				NodeType::IfThen(Box::new(cond), Box::new(then), None)
			}
		}
	}

	fn call_init(ident: String, args: Vec<Node>) -> Self {
		NodeType::Call(ident, args)
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
			val: -1,
			ty: NodeType::bit_new(tk_ty),
		}
	}

	pub fn new_bit(tk_ty: TokenType, lhs: Node, rhs: Node) -> Self {
		Self {
			val: -1,
			ty: NodeType::bit_init(tk_ty, lhs, rhs),
		}
	}
	
	pub fn new_node_num(val: i32) -> Self {
		Self {
			val: val,
			ty: NodeType::Num,
		}
	}

	pub fn new_ret(lhs: Node) -> Self {
		Self {
			val: -1,
			ty: NodeType::ret_init(lhs)
		}
	}

	pub fn new_expr(lhs: Node) -> Self {
		Self {
			val: -1,
			ty: NodeType::expr_init(lhs)
		}
	}

	pub fn new_stmt(stmts: Vec<Node>) -> Self {
		Self {
			val: -1,
			ty: NodeType::stmt_init(stmts)
		}
	}

	pub fn new_ident(s: String) -> Self {
		Self {
			val: -1,
			ty: NodeType::ident_init(s)
		}
	}

	pub fn new_eq(lhs: Node, rhs: Node) -> Self {
		Self {
			val: -1,
			ty: NodeType::eq_init(lhs, rhs)
		}
	}

	pub fn new_if(cond: Node, then: Node, elthen: Option<Node>) -> Self {
		Self {
			val: -1,
			ty: NodeType::if_init(cond, then, elthen)
		}
	}

	pub fn new_call(ident: String, args: Vec<Node>) -> Self {
		Self {
			val: -1,
			ty: NodeType::call_init(ident, args)
		}
	}
}

fn term(tokens: &Vec<Token>, pos: usize) -> (Node, usize) {
	
	if tokens[pos].ty == TokenRightBrac {
		let (lhs, new_pos) = assign(tokens, pos+1);
		assert_eq!(tokens[new_pos].ty, TokenLeftBrac);
		return (lhs, new_pos+1);
	}
	if tokens[pos].ty == TokenNum {
		return (Node::new_node_num(tokens[pos].val), pos+1);
	}
	if tokens[pos].ty == TokenIdent {
		
		// variable
		let mut poss = pos+1;
		if !tokens[poss].consume("(", &mut poss){
			return (Node::new_ident(String::from(&tokens[pos].input[..tokens[pos].val as usize])), pos+1);
		}

		// function call
		let mut args = vec![];
		if tokens[poss].consume(")", &mut poss){
			return (Node::new_call(String::from(&tokens[pos].input[..tokens[pos].val as usize]), args), poss);
		}
		let (arg1, new_pos) = assign(tokens, poss);
		poss = new_pos;
		args.push(arg1);
		while tokens[poss].consume(",", &mut poss) {
			let (argv, new_pos) = assign(tokens, poss);
			poss = new_pos;
			args.push(argv);
		}
		tokens[poss].assert_ty(")", &mut poss);
		return (Node::new_call(String::from(&tokens[pos].input[..tokens[pos].val as usize]), args), poss);
	}
	panic!("parse.rs: number expected, but got {}", tokens[pos].input);
}

fn mul(tokens: &Vec<Token>, pos: usize) -> (Node, usize) {
	let (mut lhs, mut pos) = term(tokens, pos);
	
	loop {
		if tokens[pos].ty != TokenMul && tokens[pos].ty != TokenDiv {
			return (lhs, pos);
		}
		let (rhs, new_pos) = term(tokens, pos+1);
		lhs = Node::new_bit(tokens[pos].ty.clone(), lhs, rhs);
		pos = new_pos;
	}

}

fn expr(tokens: &Vec<Token>, pos: usize) -> (Node, usize) {
	let (mut lhs, mut pos) = mul(tokens, pos);

	loop {
		if tokens[pos].ty != TokenAdd && tokens[pos].ty != TokenSub {
			return (lhs, pos);
		}
		let (rhs, new_pos) = mul(tokens, pos+1);
		lhs = Node::new_bit(tokens[pos].ty.clone(), lhs, rhs);
		pos = new_pos;
	}
	
}

fn assign(tokens: &Vec<Token>, pos: usize) -> (Node, usize) {
	let (mut lhs, new_pos) = expr(tokens, pos);
	let mut pos = new_pos;
	if tokens[pos].consume("=", &mut pos) {
		let (rhs, new_pos) = expr(tokens, pos);
		lhs = Node::new_eq(lhs, rhs);
		pos = new_pos;
	}
	(lhs, pos)
} 

pub fn stmt(tokens: &Vec<Token>, pos: usize) -> (Node, usize) {
	
	let mut pos = pos;
	let node;

	match tokens[pos].ty.clone() {
		TokenRet => {
			let (lhs, new_pos) = assign(tokens, pos+1);
			node = Node::new_ret(lhs);
			pos = new_pos;
			assert_eq!(TokenSemi, tokens[pos].ty.clone());
			pos += 1;
		},
		TokenIf => {
			let mut poss = pos+1;
			tokens[poss].assert_ty("(", &mut poss);
			let (cond, mut poss) = assign(tokens, poss);
			tokens[poss].assert_ty(")", &mut poss);
			let (then, mut poss) = stmt(tokens, poss);
			if tokens[poss].consume("else", &mut poss) {
				let (elthen, new_pos) = stmt(tokens, poss);
				node = Node::new_if(cond, then, Some(elthen));
				pos = new_pos;
			} else {
				node = Node::new_if(cond, then, None);
				pos = poss;
			}
		}
		_ => {
			let (lhs, new_pos) = assign(tokens, pos);
			node = Node::new_expr(lhs);
			pos = new_pos;
			assert_eq!(TokenSemi, tokens[pos].ty.clone());
			pos += 1;
		}
	}

	

	return (node, pos);
}

pub fn compound_stmt(tokens: &Vec<Token>, pos: usize) -> Node {
	
	let mut pos = pos;
	let mut compstmts = vec![];

	loop {
		match tokens[pos].ty.clone() {
			TokenEof => { break; },
			_ => { 
				let (stmt, new_pos) = stmt(tokens, pos);
				compstmts.push(stmt);
				pos = new_pos;
			}
		}
	}

	return Node::new_stmt(compstmts);
}

pub fn parse(tokens: &Vec<Token>, pos: usize) -> Node {
	
	let program = compound_stmt(tokens, pos);

	return program;
}