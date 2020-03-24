use super::token::*;
use super::token::TokenType::*;

#[allow(dead_code)]
#[derive(Debug)]
pub enum NodeType {
	Num(i32),
	BinaryTree(TokenType, Option<Box<Node>>, Option<Box<Node>>),
	Ret(Box<Node>),
	Expr(Box<Node>),
	CompStmt(Vec<Node>),
	Ident(String),
	EqTree(Box<Node>, Box<Node>),
	IfThen(Box<Node>, Box<Node>, Option<Box<Node>>),
	Call(String, Vec<Node>),
	Func(String, Vec<Node>, Box<Node>, usize),
	LogAnd(Box<Node>, Box<Node>),
	LogOr(Box<Node>, Box<Node>),
	For(Box<Node>, Box<Node>, Box<Node>, Box<Node>),
	VarDef(Type, String, usize, Option<Box<Node>>),
	Lvar(Type, usize),
	Deref(Box<Node>),
}

#[derive(Debug, Clone)]
pub enum Ty {
	INT,
	PTR,
}

#[derive(Debug, Clone)]
pub struct Type {
	pub ty: Ty,
	pub ptr_of: Option<Box<Type>>,
}

impl Type {
	pub fn new(ty: Ty, ptr_of: Option<Box<Type>>) -> Self {
		Self {
			ty,
			ptr_of,
		}
	}
	pub fn ptr_of(self) -> Self {
		Self {
			ty: Ty::PTR,
			ptr_of: Some(Box::new(self)),
		}
	}
}

#[allow(dead_code)]
impl NodeType {
	fn num_init(val: i32) -> Self {
		NodeType::Num(val)
	}

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
		NodeType::EqTree(Box::new(lhs), Box::new(rhs))
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

	fn func_init(ident: String, args: Vec<Node>, body: Node, stacksize: usize) -> Self {
		NodeType::Func(ident, args, Box::new(body), stacksize)
	}

	fn logand_init(lhs: Node, rhs: Node) -> Self {
		NodeType::LogAnd(Box::new(lhs), Box::new(rhs))
	}

	fn logor_init(lhs: Node, rhs: Node) -> Self {
		NodeType::LogOr(Box::new(lhs), Box::new(rhs))
	}

	fn for_init(init: Node, cond: Node, inc: Node, body: Node) -> Self {
		NodeType::For(Box::new(init), Box::new(cond), Box::new(inc), Box::new(body))
	}

	fn vardef_init(ty: Type, name: String, off: usize, rhs: Option<Node>) -> Self {
		match rhs {
			Some(node) => { NodeType::VarDef(ty, name, off, Some(Box::new(node))) }
			_ => { NodeType::VarDef(ty, name, off, None)}
		}
	}

	fn lvar_init(ty: Type, stacksize: usize) -> Self {
		NodeType::Lvar(ty, stacksize)
	}

	fn deref_init(lhs: Node) -> Self {
		NodeType::Deref(Box::new(lhs))
	}
}

#[derive(Debug)]
pub struct Node {
	pub op: NodeType,
}

#[allow(dead_code)]
impl Node {
	pub fn new(tk_ty: TokenType) -> Self {
		Self {
			op: NodeType::bit_new(tk_ty),
		}
	}

	pub fn new_bit(tk_ty: TokenType, lhs: Node, rhs: Node) -> Self {
		Self {
			op: NodeType::bit_init(tk_ty, lhs, rhs),
		}
	}
	
	pub fn new_num(val: i32) -> Self {
		Self {
			op: NodeType::num_init(val),
		}
	}

	pub fn new_ret(lhs: Node) -> Self {
		Self {
			op: NodeType::ret_init(lhs)
		}
	}

	pub fn new_expr(lhs: Node) -> Self {
		Self {
			op: NodeType::expr_init(lhs)
		}
	}

	pub fn new_stmt(stmts: Vec<Node>) -> Self {
		Self {
			op: NodeType::stmt_init(stmts)
		}
	}

	pub fn new_ident(s: String) -> Self {
		Self {
			op: NodeType::ident_init(s)
		}
	}

	pub fn new_eq(lhs: Node, rhs: Node) -> Self {
		Self {
			op: NodeType::eq_init(lhs, rhs)
		}
	}

	pub fn new_if(cond: Node, then: Node, elthen: Option<Node>) -> Self {
		Self {
			op: NodeType::if_init(cond, then, elthen)
		}
	}

	pub fn new_call(ident: String, args: Vec<Node>) -> Self {
		Self {
			op: NodeType::call_init(ident, args)
		}
	}

	pub fn new_func(ident: String, args: Vec<Node>, body: Node, stacksize: usize) -> Self {
		Self {
			op: NodeType::func_init(ident, args, body, stacksize)
		}
	}

	pub fn new_and(lhs: Node, rhs: Node) -> Self {
		Self {
			op: NodeType::logand_init(lhs, rhs)
		}
	}

	pub fn new_or(lhs: Node, rhs: Node) -> Self {
		Self {
			op: NodeType::logor_init(lhs, rhs)
		}
	}

	pub fn new_for(init: Node, cond: Node, inc: Node, body: Node) -> Self {
		Self {
			op: NodeType::for_init(init, cond, inc, body)
		}
	}

	pub fn new_vardef(ty: Type, name: String, off: usize, rhs: Option<Node>) -> Self {
		Self {
			op: NodeType::vardef_init(ty, name, off, rhs)
		}
	}

	pub fn new_lvar(ty: Type, stacksize: usize) -> Self {
		Self {
			op: NodeType::lvar_init(ty, stacksize)
		}
	}

	pub fn new_deref(lhs: Node) -> Self {
		Self {
			op: NodeType::deref_init(lhs)
		}
	}
}

fn term(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	
	if tokens[*pos].consume_ty(TokenRightBrac, pos) {
		let lhs = assign(tokens, pos);
		tokens[*pos].assert_ty(TokenLeftBrac, pos);
		return lhs;
	}
	if tokens[*pos].consume_ty(TokenNum, pos) {
		return Node::new_num(tokens[*pos-1].val);
	}
	if tokens[*pos].consume_ty(TokenIdent, pos) {

		let name = String::from(&tokens[*pos-1].input[..tokens[*pos-1].val as usize]);
		
		// variable
		if !tokens[*pos].consume_ty(TokenRightBrac, pos){
			return Node::new_ident(name);
		}

		// function call
		let mut args = vec![];
		//// arity = 0;
		if tokens[*pos].consume_ty(TokenLeftBrac, pos){
			return Node::new_call(name, args);
		}
		//// arity > 0;
		let arg1 = assign(tokens, pos);
		args.push(arg1);
		while tokens[*pos].consume_ty(TokenComma, pos) {
			let argv = assign(tokens, pos);
			args.push(argv);
		}
		tokens[*pos].assert_ty(TokenLeftBrac, pos);
		return Node::new_call(name, args);
	}
	panic!("parse.rs: term parse fail. and got {}", tokens[*pos].input);
}

fn unary(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	if tokens[*pos].consume_ty(TokenStar, pos) {
		return Node::new_deref(mul(tokens, pos));
	}
	return term(tokens, pos);
}

fn mul(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	let mut lhs = unary(tokens, pos);
	
	loop {
		if !tokens[*pos].consume_ty(TokenStar, pos) && !tokens[*pos].consume_ty(TokenDiv, pos) {
			return lhs;
		}
		
		let ty = tokens[*pos-1].ty.clone();
		let rhs = unary(tokens, pos);
		lhs = Node::new_bit(ty, lhs, rhs);
	}

}

fn add(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	let mut lhs = mul(tokens, pos);
	
	loop {
		if !tokens[*pos].consume_ty(TokenAdd, pos) && !tokens[*pos].consume_ty(TokenSub, pos) {
			return lhs;
		}
		let ty = tokens[*pos-1].ty.clone();
		let rhs = mul(tokens, pos);
		lhs = Node::new_bit(ty, lhs, rhs);
	}
	
}

fn rel(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	let mut lhs = add(tokens, pos);

	loop {
		if tokens[*pos].consume_ty(TokenLt, pos) {
			lhs = Node::new_bit(TokenLt, lhs, add(tokens, pos));
			continue;
		}
		if tokens[*pos].consume_ty(TokenRt, pos) {
			lhs = Node::new_bit(TokenLt, add(tokens, pos), lhs);
			continue;
		}
		return lhs;
	}
}

fn logand(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	let mut lhs = rel(tokens, pos);

	loop {
		if !tokens[*pos].consume_ty(TokenLogAnd, pos) {
			return lhs;
		}
		let rhs = add(tokens, pos);
		lhs = Node::new_and(lhs, rhs);
	}
}

fn logor(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	let mut lhs = logand(tokens, pos);

	loop {
		if !tokens[*pos].consume_ty(TokenLogOr, pos) {
			return lhs;
		}
		let rhs = logand(tokens, pos);
		lhs = Node::new_or(lhs, rhs);
	}
}

fn assign(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	let mut lhs = logor(tokens, pos);

	if tokens[*pos].consume_ty(TokenEq, pos) {
		let rhs = logor(tokens, pos);
		lhs = Node::new_eq(lhs, rhs);
	}
	return lhs;
}

fn typeis(tokens: &Vec<Token>, pos: &mut usize) -> Type {
	tokens[*pos].assert_ty(TokenInt, pos);
	let mut ty = Type::new(Ty::INT, None);
	while tokens[*pos].consume_ty(TokenStar, pos) {
		ty = ty.ptr_of();
	}
	return ty;
}

fn decl(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	let ty = typeis(tokens, pos);
	let name = String::from(&tokens[*pos].input[..tokens[*pos].val as usize]);
	tokens[*pos].assert_ty(TokenIdent, pos);
	if tokens[*pos].consume_ty(TokenEq, pos) {
		let rhs = assign(tokens, pos);
		tokens[*pos].assert_ty(TokenSemi, pos);
		return Node::new_vardef(ty, name, 0, Some(rhs));
	} else {
		tokens[*pos].assert_ty(TokenSemi, pos);
		return Node::new_vardef(ty, name, 0, None);
	}
}

fn expr(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	let lhs = assign(tokens, pos);
	tokens[*pos].consume_ty(TokenSemi, pos);
	return Node::new_expr(lhs);
}

pub fn stmt(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	
	match tokens[*pos].ty {
		TokenRet => {
			*pos += 1;
			let lhs = assign(tokens, pos);
			tokens[*pos].assert_ty(TokenSemi, pos);
			return Node::new_ret(lhs);
		},
		TokenIf => {
			*pos += 1;
			tokens[*pos].assert_ty(TokenRightBrac, pos);
			let cond = assign(tokens, pos);
			tokens[*pos].assert_ty(TokenLeftBrac, pos);
			let then = stmt(tokens, pos);
			if tokens[*pos].consume_ty(TokenElse, pos) {
				let elthen = stmt(tokens, pos);
				return Node::new_if(cond, then, Some(elthen));
			} else {
				return Node::new_if(cond, then, None);
			}
		}
		TokenFor => {
			*pos += 1;
			tokens[*pos].assert_ty(TokenRightBrac, pos);
			let init;
			if tokens[*pos].is_typename(pos) {
				*pos -= 1;
				init = decl(tokens, pos);
			} else {
				init = expr(tokens, pos);
			}
			let cond = assign(tokens, pos);
			tokens[*pos].assert_ty(TokenSemi, pos);
			let inc = assign(tokens, pos);
			tokens[*pos].assert_ty(TokenLeftBrac, pos);
			let body = stmt(tokens, pos);
			return Node::new_for(init, cond, inc, body);
		}
		TokenRightCurlyBrace => {
			*pos += 1;
			let mut stmts = vec![];
			while !tokens[*pos].consume_ty(TokenLeftCurlyBrace, pos) {
				stmts.push(stmt(tokens, pos));
			}
			return Node::new_stmt(stmts);
		}
		TokenInt => {
			return decl(tokens, pos);
		}
		_ => {
			return expr(tokens, pos);
		}
	}
}

pub fn compound_stmt(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	
	let mut compstmts = vec![];
	tokens[*pos].assert_ty(TokenRightCurlyBrace, pos);

	loop {
		match tokens[*pos].consume_ty(TokenLeftCurlyBrace, pos) {
			true => { break; },
			false => { 
				let stmt = stmt(tokens, pos);
				compstmts.push(stmt);
			}
		}
	}

	return Node::new_stmt(compstmts);
}

pub fn param(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	
	// type
	let ty = typeis(tokens, pos);

	// identifier
	let name = String::from(&tokens[*pos].input[..tokens[*pos].val as usize]);
	tokens[*pos].assert_ty(TokenIdent, pos);
	return Node::new_vardef(ty, name, 0, None);
}

pub fn function(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	
	let mut args = vec![];
	
	if !tokens[*pos].consume_ty(TokenInt, pos) {
		panic!("function should have type: at {}", tokens[*pos].input)
	}

	let name = String::from(&tokens[*pos].input[..tokens[*pos].val as usize]);
	if !tokens[*pos].consume_ty(TokenIdent, pos) {
		panic!("function should start from ident : at {}", tokens[*pos].input)
	}

	// argument
	tokens[*pos].assert_ty(TokenRightBrac, pos);
	if !tokens[*pos].consume_ty(TokenLeftBrac, pos) {
		loop {
			args.push(param(tokens, pos));
			if tokens[*pos].consume_ty(TokenLeftBrac, pos){ break; }
			tokens[*pos].assert_ty(TokenComma, pos);
		}
	}
	
	// body
	let body = compound_stmt(tokens, pos);

	return Node::new_func(name, args, body, 0);
}

pub fn parse(tokens: &Vec<Token>, pos: &mut usize) -> Vec<Node> {
	
	let mut program = vec![];

	loop {
		match tokens[*pos].consume_ty(TokenEof, pos) {
			true => { break; }
			false => { program.push(function(tokens, pos)); }
		}
	}

	return program;
}