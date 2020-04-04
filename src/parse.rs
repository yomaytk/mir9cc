use super::token::*;
use super::token::TokenType::*;

lazy_static! {
	pub static ref INT_TY: Type = Type {
		ty: Ty::INT,
		ptr_of: None,
		ary_of: None,
		len: 0,
	};
	pub static ref CHAR_TY: Type = Type {
		ty: Ty::CHAR,
		ptr_of: None,
		ary_of: None,
		len: 0,
	};
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum NodeType {
	Num(i32),
	BinaryTree(Type, TokenType, Option<Box<Node>>, Option<Box<Node>>),
	Ret(Box<Node>),
	Expr(Box<Node>),
	CompStmt(Vec<Node>),
	Ident(String),
	EqTree(Type, Box<Node>, Box<Node>),
	IfThen(Box<Node>, Box<Node>, Option<Box<Node>>),
	Call(String, Vec<Node>),
	Func(String, Vec<Node>, Box<Node>, usize),
	LogAnd(Box<Node>, Box<Node>),
	LogOr(Box<Node>, Box<Node>),
	For(Box<Node>, Box<Node>, Box<Node>, Box<Node>),
	VarDef(Type, String, usize, Option<Box<Node>>),
	Lvar(Type, usize),
	Deref(Type, Box<Node>),
	Addr(Type, Box<Node>),
	Sizeof(Type, usize, Box<Node>),
	Str(Type, String, usize),
	Gvar(Type, usize),
	EqEq(Box<Node>, Box<Node>),
	Ne(Box<Node>, Box<Node>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
	INT,
	PTR,
	ARY,
	CHAR
}

#[derive(Debug, Clone)]
pub struct Type {
	pub ty: Ty,
	pub ptr_of: Option<Box<Type>>,
	pub ary_of: Option<Box<Type>>,
	pub len: usize,
}

impl Type {
	pub fn new(ty: Ty, ptr_of: Option<Box<Type>>, ary_of: Option<Box<Type>>, len: usize) -> Self {
		Self {
			ty,
			ptr_of,
			ary_of,
			len,
		}
	}
	pub fn size_of(&self) -> usize {
		match &self.ty {
			Ty::INT => { return 4; }
			Ty::PTR => { return 8; }
			Ty::ARY => { return self.len * self.ary_of.as_ref().unwrap().size_of(); }
			Ty::CHAR => { return 1; }
		}
	}
	pub fn ptr_of(self) -> Self {
		Self {
			ty: Ty::PTR,
			ptr_of: Some(Box::new(self)),
			ary_of: None,
			len: 0,
		}
	}
	pub fn ary_of(self, len: usize) -> Self {
		Self {
			ty: Ty::ARY,
			ptr_of: None,
			ary_of: Some(Box::new(self)),
			len,
		}
	}
}

#[allow(dead_code)]
impl NodeType {
	fn num_init(val: i32) -> Self {
		NodeType::Num(val)
	}

	fn bit_init(ctype: Type, tk_ty: TokenType, lhs: Node, rhs: Node) -> Self {
		NodeType::BinaryTree(ctype, tk_ty, Some(Box::new(lhs)), Some(Box::new(rhs)))
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

	fn eq_init(ctype: Type, lhs: Node, rhs: Node) -> Self {
		NodeType::EqTree(ctype, Box::new(lhs), Box::new(rhs))
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

	fn deref_init(ctype: Type, lhs: Node) -> Self {
		NodeType::Deref(ctype, Box::new(lhs))
	}

	fn addr_init(ctype: Type, lhs: Node) -> Self {
		NodeType::Addr(ctype, Box::new(lhs))
	}

	fn sizeof_init(ctype: Type, val: usize, lhs: Node) -> Self {
		NodeType::Sizeof(ctype, val, Box::new(lhs))
	}

	fn string_init(ctype: Type, strname: String, label: usize) -> Self {
		NodeType::Str(ctype, strname, label)
	}

	fn gvar_init(ctype: Type, label: usize) -> Self {
		NodeType::Gvar(ctype, label)
	}

	fn eqeq_init(lhs: Node, rhs: Node) -> Self {
		NodeType::EqEq(Box::new(lhs), Box::new(rhs))
	}

	fn neq_init(lhs: Node, rhs: Node) -> Self {
		NodeType::Ne(Box::new(lhs), Box::new(rhs))
	}
}

#[derive(Debug, Clone)]
pub struct Node {
	pub op: NodeType,
}

#[allow(dead_code)]
impl Node {
	
	pub fn hasctype(&self) -> bool {
		match &self.op {
			NodeType::Lvar(_, _) | NodeType::BinaryTree(_, _, _, _) 
			| NodeType::Deref(_, _) | NodeType::Addr(_, _)
			| NodeType::Sizeof(_, _, _) | NodeType::Str(_, _, _)
			| NodeType::Gvar(_, _) => { return true; }
			_ => { return false; }
		}
	}

	pub fn nodesctype(&self) -> Type {
		match &self.op {
			NodeType::Lvar(ctype, _) | NodeType::BinaryTree(ctype, _, _, _) 
			| NodeType::Deref(ctype, _) | NodeType::Addr(ctype, _) 
			| NodeType::Sizeof(ctype, _, _) | NodeType::Str(ctype, _, _)
			| NodeType::Gvar(ctype, _) => { return ctype.clone(); }
			_ => { panic!("nodesctype fun error."); }
		} 
	}

	pub fn checklval(&self) {
		match &self.op {
			NodeType::Lvar(_, _) | NodeType::Gvar(_, _) | NodeType::Deref(_, _) => {}
			_ => { panic!("not an lvalue"); }
		}
	}

	pub fn new_bit(ctype: Type, tk_ty: TokenType, lhs: Node, rhs: Node) -> Self {
		Self {
			op: NodeType::bit_init(ctype, tk_ty, lhs, rhs),
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

	pub fn new_eq(ctype: Type, lhs: Node, rhs: Node) -> Self {
		Self {
			op: NodeType::eq_init(ctype, lhs, rhs)
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

	pub fn new_deref(ctype: Type, lhs: Node) -> Self {
		Self {
			op: NodeType::deref_init(ctype, lhs)
		}
	}

	pub fn new_addr(ctype: Type, lhs: Node) -> Self {
		Self {
			op: NodeType::addr_init(ctype, lhs)
		}
	}

	pub fn new_sizeof(ctype: Type, val: usize, lhs: Node) -> Self {
		Self {
			op: NodeType::sizeof_init(ctype, val, lhs)
		}
	}

	pub fn new_string(ctype: Type, strname: String, label: usize) -> Self {
		Self {
			op: NodeType::string_init(ctype, strname, label)
		}
	}

	pub fn new_gvar(ctype: Type, label: usize) -> Self {
		Self {
			op: NodeType::gvar_init(ctype, label)
		}
	}

	pub fn new_eqeq(lhs: Node, rhs: Node) -> Self {
		Self {
			op: NodeType::eqeq_init(lhs, rhs)
		}
	}

	pub fn new_neq(lhs: Node, rhs: Node) -> Self {
		Self {
			op: NodeType::neq_init(lhs, rhs)
		}
	}
}

fn primary(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	
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
	if tokens[*pos].consume_ty(TokenString(String::new()), pos) {
		let strname = tokens[*pos].getstring();
		let cty = CHAR_TY.clone().ary_of(tokens[*pos].val as usize);
		*pos += 1;
		return Node::new_string(cty, strname, 0);
	}
	panic!("parse.rs: primary parse fail. and got {}", tokens[*pos].input);
}

fn postfix(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	let mut lhs = primary(tokens, pos);
	while tokens[*pos].consume_ty(TokenRightmiddleBrace, pos)  {
		let id = assign(tokens, pos);
		let lhs2 = Node::new_bit(INT_TY.clone(), TokenAdd, lhs, id);
		lhs = Node::new_deref(INT_TY.clone(), lhs2);
		tokens[*pos].assert_ty(TokenLeftmiddleBrace, pos);
	}
	return lhs;
}

fn unary(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	
	if tokens[*pos].consume_ty(TokenStar, pos) {
		return Node::new_deref(INT_TY.clone(), mul(tokens, pos));
	}
	if tokens[*pos].consume_ty(TokenAmpersand, pos) {
		return Node::new_addr(INT_TY.clone(), mul(tokens, pos));
	}
	if tokens[*pos].consume_ty(TokenSizeof, pos) {
		return Node::new_sizeof(INT_TY.clone(), 0, unary(tokens, pos));
	}
	return postfix(tokens, pos);
}

fn mul(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	let mut lhs = unary(tokens, pos);
	
	loop {
		if !tokens[*pos].consume_ty(TokenStar, pos) && !tokens[*pos].consume_ty(TokenDiv, pos) {
			return lhs;
		}
		
		let ty = tokens[*pos-1].ty.clone();
		let rhs = unary(tokens, pos);
		lhs = Node::new_bit(INT_TY.clone(), ty, lhs, rhs);
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
		let ctype = Type::new(Ty::INT, None, None, 0);
		lhs = Node::new_bit(ctype, ty, lhs, rhs);
	}
	
}

fn rel(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	let mut lhs = add(tokens, pos);
	
	loop {
		if tokens[*pos].consume_ty(TokenLt, pos) {
			lhs = Node::new_bit(INT_TY.clone(), TokenLt, lhs, add(tokens, pos));
			continue;
		}
		if tokens[*pos].consume_ty(TokenRt, pos) {
			lhs = Node::new_bit(INT_TY.clone(), TokenLt, add(tokens, pos), lhs);
			continue;
		}
		return lhs;
	}
}

fn equarity(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	let mut lhs = rel(tokens, pos);
	
	loop {
		if tokens[*pos].consume_ty(TokenEqEq, pos) {
			lhs = Node::new_eqeq(lhs, rel(tokens, pos));
			continue;
		}
		if tokens[*pos].consume_ty(TokenNe, pos) {
			lhs = Node::new_neq(lhs, rel(tokens, pos));
			continue;
		}
		return lhs;
	}
}

fn logand(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	let mut lhs = equarity(tokens, pos);

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
		lhs = Node::new_eq(INT_TY.clone(), lhs, rhs);
	}
	return lhs;
}

fn ctype(tokens: &Vec<Token>, pos: &mut usize) -> Type {
	
	let mut ty = tokens[*pos].decl_type(pos);

	while tokens[*pos].consume_ty(TokenStar, pos) {
		ty = ty.ptr_of();
	}
	return ty;
}

fn read_array(tokens: &Vec<Token>, pos: &mut usize, ty: Type) -> Type {
	let mut ary_size = vec![];
	let mut ty = ty.clone();

	while tokens[*pos].consume_ty(TokenRightmiddleBrace, pos) {
		let len = primary(tokens, pos);
		if let NodeType::Num(val) = &len.op {
			ary_size.push(*val as usize);
			tokens[*pos].assert_ty(TokenLeftmiddleBrace, pos);
			continue;
		}
		panic!("array declaration is invalid at {}.", tokens[*pos].input);
	}

	if ary_size.len() > 0 {
		for i in (0..ary_size.len()).rev() {
			ty = ty.ary_of(ary_size[i]);
		}
	}

	return ty;
}

fn decl(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	// declaratoin type
	let mut ty = ctype(tokens, pos);

	// identifier
	let name = String::from(&tokens[*pos].input[..tokens[*pos].val as usize]);
	tokens[*pos].assert_ty(TokenIdent, pos);

	// array decralation
	ty = read_array(tokens, pos, ty);

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
		TokenInt | TokenChar => {
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
	let ty = ctype(tokens, pos);

	// identifier
	let name = String::from(&tokens[*pos].input[..tokens[*pos].val as usize]);
	tokens[*pos].assert_ty(TokenIdent, pos);
	return Node::new_vardef(ty, name, 0, None);
}

pub fn toplevel(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	
	let mut args = vec![];
	
	// Ctype
	let mut ctype = ctype(tokens, pos);

	// identifier
	let name = String::from(&tokens[*pos].input[..tokens[*pos].val as usize]);
	if !tokens[*pos].consume_ty(TokenIdent, pos) {
		panic!("function should start from ident : at {}", tokens[*pos].input)
	}
	
	// function
	if tokens[*pos].consume_ty(TokenRightBrac, pos){
		// argument
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
	
	// global variable
	ctype = read_array(tokens, pos, ctype);
	tokens[*pos].assert_ty(TokenSemi, pos);
	return Node::new_vardef(ctype, name, 0, None);

}

pub fn parse(tokens: &Vec<Token>, pos: &mut usize) -> Vec<Node> {
	
	let mut program = vec![];

	loop {
		match tokens[*pos].consume_ty(TokenEof, pos) {
			true => { break; }
			false => { program.push(toplevel(tokens, pos)); }
		}
	}

	return program;
}