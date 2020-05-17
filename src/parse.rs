use super::token::*;
use super::token::TokenType::*;
// use super::lib::*;
use std::collections::HashMap;
use std::sync::Mutex;

// This is a recursive-descendent parser which constructs abstract
// syntax tree from input tokens.
//
// This parser knows only about BNF of the C grammer and doesn't care
// about its semantics. Therefore, some invalid expressions, such as
// `1+2=3`, are accepted by this parser, but that's intentional.
// Semantic errors are detected in a later pass.

macro_rules! env_find {
	($s:expr, $m:ident, $ty:expr) => {
		{
			if let Some(t) = ENV.lock().unwrap().$m.get(&$s) {
				return t.clone()
			}
			let mut env = &ENV.lock().unwrap().next;
			let mut ctype = NULL_TY.clone();
			while let Some(e) = env {
				if let Some(t) = e.$m.get(&$s) {
					ctype = t.clone();
					break;
				}
				env = &e.next;
			}
			return ctype
		}
	};
}

lazy_static! {
	pub static ref INT_TY: Type = Type {
		ty: Ty::INT,
		ptr_to: None,
		ary_to: None,
		size: 4,
		align: 4,
		offset: 0,
		len: 0,
	};
	pub static ref CHAR_TY: Type = Type {
		ty: Ty::CHAR,
		ptr_to: None,
		ary_to: None,
		size: 1,
		align: 1,
		offset: 0,
		len: 0,
	};
	pub static ref VOID_TY: Type = Type {
		ty: Ty::VOID,
		ptr_to: None,
		ary_to: None,
		size: 0,
		align: 0,
		offset: 0,
		len: 0,
	};
	pub static ref NULL_TY: Type = Type {
		ty: Ty::NULL,
		ptr_to: None,
		ary_to: None,
		size: 0,
		align: 0,
		offset: 0,
		len: 0,
	};
	pub static ref STRUCT_TY: Type = Type {
		ty: Ty::STRUCT(String::new(), Vec::new()),
		ptr_to: None,
		ary_to: None,
		size: 0,
		align: 0,
		offset: 0,
		len: 0,
	};
	pub static ref ENV: Mutex<Env> = Mutex::new(Env::new_env(None));
}

#[derive(Debug, Clone)]
pub struct Type {
	pub ty: Ty,
	pub ptr_to: Option<Box<Type>>,
	pub ary_to: Option<Box<Type>>,
	pub size: usize,
	pub align: usize,
	pub offset: usize,
	pub len: usize,
}

impl Type {
	pub fn new(ty: Ty, ptr_to: Option<Box<Type>>, ary_to: Option<Box<Type>>, size: usize, align: usize, offset: usize, len: usize) -> Self {
		Self {
			ty,
			ptr_to,
			ary_to,
			size, 
			align,
			offset,
			len,
		}
	}
	pub fn ptr_to(self) -> Self {
		Self {
			ty: Ty::PTR,
			ptr_to: Some(Box::new(self)),
			ary_to: None,
			size: 8,
			align: 8,
			offset: 0,
			len: 0,
		}
	}
	pub fn ary_of(self, len: usize) -> Self {
		let size = self.size;
		let align = self.align;
		Self {
			ty: Ty::ARY,
			ptr_to: None,
			ary_to: Some(Box::new(self)),
			size: size * len,
			align: align,
			offset: 0,
			len,
		}
	}
}

#[derive(Debug, Clone)]
pub enum Ty {
	INT,
	PTR,
	ARY,
	CHAR,
	STRUCT(String, Vec<Node>),
	VOID,
	NULL,
}

impl PartialEq for Ty {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(Ty::INT, Ty::INT) | (Ty::PTR, Ty::PTR) | (Ty::ARY, Ty::ARY)
			| (Ty::CHAR, Ty::CHAR) | (Ty::VOID, Ty::VOID) | (Ty::NULL, Ty::NULL) => {
				return true;
			}
			(Ty::STRUCT(tag1, _), Ty::STRUCT(tag2, _)) => {
				if tag1 == tag2 {
					return true;
				} else {
					return false;
				}
			}
			_ => {
				return false;
			}
		}
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum NodeType {
	Num(i32),																	// Num(val)
	BinaryTree(Type, TokenType, Box<Node>, Box<Node>),							// BinaryTree(ctype, tk_ty, lhs, rhs)
	Ret(Box<Node>),																// Ret(lhs)
	Expr(Box<Node>),															// Expr(lhs)
	CompStmt(Vec<Node>),														// CompStmt(stmts)
	StmtExpr(Type, Box<Node>),													// StmtExpr(ctype, body)
	Ident(String),																// Ident(s)
	EqTree(Type, Box<Node>, Box<Node>),											// EqTree(ctype, lhs, rhs)
	IfThen(Box<Node>, Box<Node>, Option<Box<Node>>),							// IfThen(cond, then, elthen)
	Call(Type, String, Vec<Node>),												// Call(ctype, ident, args)
	Func(Type, String, bool, Vec<Node>, Box<Node>, usize),						// Func(ctype, ident, is_extern, args, body, stacksize)
	LogAnd(Box<Node>, Box<Node>),												// LogAnd(lhs, rhs)
	LogOr(Box<Node>, Box<Node>),												// LogOr(lhs, rhs)
	For(Box<Node>, Box<Node>, Box<Node>, Box<Node>),							// For(init, cond, inc, body)
	VarDef(Type, bool, String, usize, Option<Box<Node>>),						// VarDef(ty, is_extern, name, off, rhs)
	Lvar(Type, usize),															// Lvar(ty, stacksize)
	Deref(Type, Box<Node>),														// Deref(ctype, lhs)
	Addr(Type, Box<Node>),														// Addr(ctype, lhs)
	Sizeof(Type, usize, Box<Node>),												// Sizeof(ctype, val, lhs)
	Str(Type, String, usize),													// Str(ctype, strname, label)
	Gvar(Type, String),															// Gvar(ctype, label)
	EqEq(Box<Node>, Box<Node>),													// EqEq(lhs, rhs)
	Ne(Box<Node>, Box<Node>),													// Ne(lhs, rhs)
	DoWhile(Box<Node>, Box<Node>),												// Dowhile(boyd, cond)
	Alignof(Box<Node>),															// Alignof(expr)
	Dot(Type, Box<Node>, String, usize),										// Dot(ctype, expr, name, offset)
	Not(Box<Node>),																// Not(expr)
	Ternary(Type, Box<Node>, Box<Node>, Box<Node>),								// Ternary(ctype, cond, then, els)
	TupleExpr(Type, Box<Node>, Box<Node>),										// TupleExpr(ctype, lhs, rhs)
	Neg(Box<Node>),																// Neg(expr)
	IncDec(Type, i32, Box<Node>),												// IncDec(ctype, selector, expr)
	Decl(Type, String, Vec<Node>, bool),										// Decl(ctype, ident)
	Break,																		// Break
	NULL,																		// NULL
}

#[derive(Debug, Clone)]
pub struct Node {
	pub op: NodeType,
}

#[allow(dead_code)]
impl Node {
	
	pub fn nodesctype(&self, basetype: Option<Type>) -> Type {
		match &self.op {
			NodeType::Lvar(ctype, ..) | NodeType::BinaryTree(ctype, ..) 
			| NodeType::Deref(ctype,..) | NodeType::Addr(ctype, ..) 
			| NodeType::Sizeof(ctype, ..) | NodeType::Str(ctype, ..)
			| NodeType::Gvar(ctype,..) | NodeType::Dot(ctype, ..) 
			| NodeType::Ternary(ctype, ..) | NodeType::IncDec(ctype, ..) 
			| NodeType::EqTree(ctype, ..) | NodeType::VarDef(ctype, ..) => { 
				return ctype.clone(); 
			}
			_ => { 
				if let Some(ty) = basetype {
					return ty;
				} else {
					return NULL_TY.clone();
				}
			}
		} 
	}

	fn ctype_replace(&mut self, new_ty: Type) {
		match &mut self.op {
			NodeType::Lvar(ref mut ctype, ..) | NodeType::BinaryTree(ref mut ctype, ..) 
			| NodeType::Deref(ref mut ctype,..) | NodeType::Addr(ref mut ctype, ..) 
			| NodeType::Sizeof(ref mut ctype, ..) | NodeType::Str(ref mut ctype, ..)
			| NodeType::Gvar(ref mut ctype,..) | NodeType::Dot(ref mut ctype, ..) 
			| NodeType::Ternary(ref mut ctype, ..) | NodeType::IncDec(ref mut ctype, ..) 
			| NodeType::EqTree(ref mut ctype, ..) | NodeType::VarDef(ref mut ctype, ..) => { 
				std::mem::replace(ctype, new_ty);
			}
			_ => {}
		}
	}

	pub fn checklval(&self) {
		match &self.op {
			NodeType::Lvar(..) | NodeType::Gvar(..) | NodeType::Deref(..) | NodeType::Dot(..) => {}
			_ => { 
				// error("not an lvalue");
				// for debug.
				panic!("not an lvalue");
			}
		}
	}

	pub fn new_num(val: i32) -> Self {
		Self {
			op: NodeType::Num(val),
		}
	}
	pub fn new_bit(ctype: Type, tk_ty: TokenType, lhs: Node, rhs: Node) -> Self {
		Self {
			op: NodeType::BinaryTree(ctype, tk_ty, Box::new(lhs), Box::new(rhs))
		}
	}
	pub fn new_ret(lhs: Node) -> Self {
		Self {
			op: NodeType::Ret(Box::new(lhs))
		}
	}
	pub fn new_expr(lhs: Node) -> Self {
		Self {
			op: NodeType::Expr(Box::new(lhs))
		}
	}
	pub fn new_stmt(stmts: Vec<Node>) -> Self {
		Self {
			op: NodeType::CompStmt(stmts)
		}
	}
	pub fn new_ident(ident: String) -> Self {
		Self {
			op: NodeType::Ident(ident)
		}
	}
	pub fn new_eq(ctype: Type, lhs: Node, rhs: Node) -> Self {
		Self {
			op: NodeType::EqTree(ctype, Box::new(lhs), Box::new(rhs))
		}
	}
	pub fn new_if(cond: Node, then: Node, elthen: Option<Node>) -> Self {
		Self {
			op: match elthen {
				Some(node) => {
					NodeType::IfThen(Box::new(cond), Box::new(then), Some(Box::new(node)))
				}
				None => {
					NodeType::IfThen(Box::new(cond), Box::new(then), None)
				}
			}
		}
	}
	pub fn new_call(ctype: Type, ident: String, args: Vec<Node>) -> Self {
		Self {
			op: NodeType::Call(ctype, ident, args)
		}
	}
	pub fn new_func(ctype: Type, ident: String, is_extern: bool, args: Vec<Node>, body: Node, stacksize: usize) -> Self {
		Self {
			op: NodeType::Func(ctype, ident, is_extern, args, Box::new(body), stacksize)
		}
	}
	pub fn new_and(lhs: Node, rhs: Node) -> Self {
		Self {
			op: NodeType::LogAnd(Box::new(lhs), Box::new(rhs))
		}
	}
	pub fn new_or(lhs: Node, rhs: Node) -> Self {
		Self {
			op: NodeType::LogOr(Box::new(lhs), Box::new(rhs))
		}
	}
	pub fn new_for(init: Node, cond: Node, inc: Node, body: Node) -> Self {
		Self {
			op: NodeType::For(Box::new(init), Box::new(cond), Box::new(inc), Box::new(body))
		}
	}
	pub fn new_vardef(ty: Type, is_extern: bool, name: String, off: usize, rhs: Option<Node>) -> Self {
		Self {
			op: match rhs {
				Some(node) => { NodeType::VarDef(ty, is_extern, name, off, Some(Box::new(node))) }
				_ => { NodeType::VarDef(ty, is_extern, name, off, None)}
			}
		}
	}
	pub fn new_lvar(ty: Type, stacksize: usize) -> Self {
		Self {
			op: NodeType::Lvar(ty, stacksize)
		}
	}
	pub fn new_deref(ctype: Type, lhs: Node) -> Self {
		Self {
			op: NodeType::Deref(ctype, Box::new(lhs))
		}
	}
	pub fn new_addr(ctype: Type, lhs: Node) -> Self {
		Self {
			op: NodeType::Addr(ctype, Box::new(lhs))
		}
	}
	pub fn new_sizeof(ctype: Type, val: usize, lhs: Node) -> Self {
		Self {
			op: NodeType::Sizeof(ctype, val, Box::new(lhs))
		}
	}
	pub fn new_string(ctype: Type, strname: String, label: usize) -> Self {
		Self {
			op: NodeType::Str(ctype, strname, label)
		}
	}
	pub fn new_gvar(ctype: Type, label: String) -> Self {
		Self {
			op: NodeType::Gvar(ctype, label)
		}
	}
	pub fn new_eqeq(lhs: Node, rhs: Node) -> Self {
		Self {
			op: NodeType::EqEq(Box::new(lhs), Box::new(rhs))
		}
	}
	pub fn new_neq(lhs: Node, rhs: Node) -> Self {
		Self {
			op: NodeType::Ne(Box::new(lhs), Box::new(rhs))
		}
	}
	pub fn new_dowhile(body: Node, cond: Node) -> Self {
		Self {
			op: NodeType::DoWhile(Box::new(body), Box::new(cond))
		}
	}
	pub fn new_stmtexpr(ctype: Type, body: Node) -> Self {
		Self {
			op: NodeType::StmtExpr(ctype, Box::new(body))
		}
	}
	pub fn new_alignof(expr: Node) -> Self {
		Self {
			op: NodeType::Alignof(Box::new(expr))
		}
	}
	pub fn new_dot(ctype: Type, expr: Node, member: String, offset: usize) -> Self {
		Self {
			op: NodeType::Dot(ctype, Box::new(expr), member, offset)
		}
	}
	pub fn new_not(expr: Node) -> Self {
		Self {
			op: NodeType::Not(Box::new(expr))
		}
	}
	pub fn new_ternary(ctype: Type, cond: Node, then: Node, els: Node) -> Self {
		Self {
			op: NodeType::Ternary(ctype, Box::new(cond), Box::new(then), Box::new(els))
		}
	}
	pub fn new_tuple(ctype: Type, lhs: Node, rhs: Node) -> Self {
		Self {
			op: NodeType::TupleExpr(ctype, Box::new(lhs), Box::new(rhs))
		}
	}
	pub fn new_neg(expr: Node) -> Self {
		Self {
			op: NodeType::Neg(Box::new(expr))
		}
	}
	pub fn new_incdec(ctype: Type, selector: i32, expr: Node) -> Self {
		Self {
			op: NodeType::IncDec(ctype, selector, Box::new(expr))
		}
	}
	pub fn new_decl(ctype: Type, ident: String, args: Vec<Node>, is_extern: bool) -> Self {
		Self {
			op: NodeType::Decl(ctype, ident, args, is_extern)
		}
	}
	pub fn new_break() -> Self {
		Self {
			op: NodeType::Break
		}
	}
	pub fn new_null() -> Self {
		Self {
			op: NodeType::NULL
		}
	}
}

#[derive(Debug, Clone)]
pub struct Env {
	tags: HashMap<String, Type>,
	typedefs: HashMap<String, Type>,
	next: Option<Box<Env>>,
}

impl Env {
	pub fn new_env(env: Option<Env>) -> Self {
		match env {
			Some(_env) => {
				Self {
					tags: HashMap::new(),
					typedefs: HashMap::new(),
					next: Some(Box::new(_env)),
				}
			}
			None => {
				Self {
					tags: HashMap::new(),
					typedefs: HashMap::new(),
					next: None,
				}
			}
		}
	}
}

pub fn roundup(x: usize, align: usize) -> usize {
	return (x + align - 1) & !(align - 1);
}

pub fn decl_specifiers(tokens: &Vec<Token>,  pos: &mut usize) -> Type {
	if tokens[*pos].consume_ty(TokenIdent, pos) {
		*pos -= 1;
		let name = ident(tokens, pos);
		env_find!(name, typedefs, NULL_TY.clone());
	}
	if tokens[*pos].consume_ty(TokenInt, pos){
		return INT_TY.clone();
	}
	if tokens[*pos].consume_ty(TokenChar, pos){
		return CHAR_TY.clone();
	}
	if tokens[*pos].consume_ty(TokenStruct, pos){
		
		let mut members = vec![];
		let mut tag = String::new();
		// tag
		if tokens[*pos].consume_ty(TokenIdent, pos) {
			*pos -= 1;
			tag = ident(tokens, pos);
		}
		
		// struct member
		if tokens[*pos].consume_ty(TokenRightCurlyBrace, pos) {
			while !tokens[*pos].consume_ty(TokenLeftCurlyBrace, pos) {
				members.push(declaration(tokens, pos));
			}
		}
		match (members.is_empty(), tag.is_empty()) {
			(true, true) => {
				// error("bat struct definition.");
				// for debug.
				panic!("bat struct definition.");
			}
			(true, false) => {
				env_find!(tag.clone(), tags, STRUCT_TY.clone());
			}
			(false, c) => {
				let struct_type = new_struct(tag.clone(), members);
				if !c {
					ENV.lock().unwrap().tags.insert(tag, struct_type.clone());
				}
				return struct_type;
			}
		}
	}
	if tokens[*pos].consume_ty(TokenVoid, pos) {
		return VOID_TY.clone();
	}
	return NULL_TY.clone();
}

pub fn new_struct(tag: String, mut members: Vec<Node>) -> Type {
	let mut ty_align = 0;

	let mut off = 0;
	for i in 0..members.len() {
		if let NodeType::VarDef(ctype, ..) = &mut members[i].op {
			off = roundup(off, ctype.align);
			ctype.offset = off;
			off += ctype.size;
			ty_align = std::cmp::max(ty_align, ctype.align);
		}
	}
	let ty_size = roundup(off, ty_align);

	return Type::new(Ty::STRUCT(tag, members), None, None, ty_size ,ty_align , 0, 0);
}

fn assignment_op(tokens: &Vec<Token>, pos: &mut usize) -> Option<TokenType> {
	if tokens[*pos].consume_ty(TokenAddEq, pos) { return Some(TokenAdd); }
	else if tokens[*pos].consume_ty(TokenSubEq, pos) { return Some(TokenSub); }
	else if tokens[*pos].consume_ty(TokenMulEq, pos) { return Some(TokenStar); }
	else if tokens[*pos].consume_ty(TokenDivEq, pos) { return Some(TokenDiv); }
	else if tokens[*pos].consume_ty(TokenModEq, pos) { return Some(TokenMod); }
	else if tokens[*pos].consume_ty(TokenShlEq, pos) { return Some(TokenShl); }
	else if tokens[*pos].consume_ty(TokenShrEq, pos) { return Some(TokenShr); }
	else if tokens[*pos].consume_ty(TokenAndEq, pos) { return Some(TokenAmpersand); }
	else if tokens[*pos].consume_ty(TokenOrEq, pos) { return Some(TokenOr); }
	else if tokens[*pos].consume_ty(TokenXorEq, pos) { return Some(TokenXor); }
	else if tokens[*pos].consume_ty(TokenEq, pos) { return Some(TokenEq); }
	else { return None; }
}

fn primary(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	
	if tokens[*pos].consume_ty(TokenRightBrac, pos) {
		if tokens[*pos].consume_ty(TokenRightCurlyBrace, pos) {
			*pos -= 1;
			let body = Node::new_stmtexpr(INT_TY.clone(), compound_stmt(tokens, pos));
			tokens[*pos].assert_ty(TokenLeftBrac, pos);
			return body;
		}
		let lhs = expr(tokens, pos);
		tokens[*pos].assert_ty(TokenLeftBrac, pos);
		return lhs;
	}
	if tokens[*pos].consume_ty(TokenNum, pos) {
		return Node::new_num(tokens[*pos-1].val);
	}
	if tokens[*pos].consume_ty(TokenIdent, pos) {

		let name = String::from(&PROGRAMS.lock().unwrap()[tokens[*pos-1].program_id][tokens[*pos-1].pos..tokens[*pos-1].end]);
		// variable
		if !tokens[*pos].consume_ty(TokenRightBrac, pos){
			return Node::new_ident(name);
		}

		// function call
		let mut args = vec![];
		//// arity = 0;
		if tokens[*pos].consume_ty(TokenLeftBrac, pos){
			return Node::new_call(NULL_TY.clone(), name, args);
		}
		//// arity > 0;
		let arg1 = assign(tokens, pos);
		args.push(arg1);
		while tokens[*pos].consume_ty(TokenComma, pos) {
			let argv = assign(tokens, pos);
			args.push(argv);
		}
		tokens[*pos].assert_ty(TokenLeftBrac, pos);
		return Node::new_call(NULL_TY.clone(), name, args);
	}
	if tokens[*pos].consume_ty(TokenString(String::new()), pos) {
		let strname = tokens[*pos].getstring();
		let cty = CHAR_TY.clone().ary_of(strname.len() + 1);
		*pos += 1;
		return Node::new_string(cty, strname, 0);
	}
	// error(&format!("parse.rs: primary parse fail. and got {}", tokens[*pos].input));
	// for debug.
	panic!("parse.rs: primary parse fail. and got rerere{:?}rererer {}", tokens[*pos], &PROGRAMS.lock().unwrap()[tokens[*pos].program_id][tokens[*pos].pos..]);
}

fn postfix(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	
	let mut lhs = primary(tokens, pos);

	loop {
		if tokens[*pos].consume_ty(TokenInc, pos) {
			lhs = Node::new_incdec(NULL_TY.clone(), 1, lhs);
		}
		if tokens[*pos].consume_ty(TokenDec, pos) {
			lhs = Node::new_incdec(NULL_TY.clone(), 2, lhs);
		}
		// struct member
		if tokens[*pos].consume_ty(TokenDot, pos) {
			let name = ident(tokens, pos);
			lhs = Node::new_dot(NULL_TY.clone(), lhs, name, 0);
		// struct member arrow
		} else if tokens[*pos].consume_ty(TokenArrow, pos) {
			let name = ident(tokens, pos);
			let expr = Node::new_deref(INT_TY.clone(), lhs);
			lhs = Node::new_dot(NULL_TY.clone(), expr, name, 0);
		// array
		} else if tokens[*pos].consume_ty(TokenRightmiddleBrace, pos)  {
			let id = assign(tokens, pos);
			let lhs2 = Node::new_bit(INT_TY.clone(), TokenAdd, lhs, id);
			lhs = Node::new_deref(INT_TY.clone(), lhs2);
			tokens[*pos].assert_ty(TokenLeftmiddleBrace, pos);
		} else {
			return lhs;
		}
	}
}

fn unary(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	
	if tokens[*pos].consume_ty(TokenInc, pos) {
		let lhs = unary(tokens, pos);
		let rhs = Node::new_bit(NULL_TY.clone(), TokenAdd, lhs.clone(), Node::new_num(1));
		return Node::new_eq(NULL_TY.clone(), lhs, rhs);
	}
	if tokens[*pos].consume_ty(TokenDec, pos) {
		let lhs = unary(tokens, pos);
		let rhs = Node::new_bit(NULL_TY.clone(), TokenSub, lhs.clone(), Node::new_num(1));
		return Node::new_eq(NULL_TY.clone(), lhs, rhs);
	}
	if tokens[*pos].consume_ty(TokenSub, pos) {
		return Node::new_neg(unary(tokens, pos));
	}
	if tokens[*pos].consume_ty(TokenStar, pos) {
		return Node::new_deref(INT_TY.clone(), unary(tokens, pos));
	}
	if tokens[*pos].consume_ty(TokenAmpersand, pos) {
		return Node::new_addr(INT_TY.clone(), unary(tokens, pos));
	}
	if tokens[*pos].consume_ty(TokenSizeof, pos) {
		return Node::new_sizeof(INT_TY.clone(), 0, unary(tokens, pos));
	}
	if tokens[*pos].consume_ty(TokenAlignof, pos) {
		return Node::new_alignof(unary(tokens, pos));
	}
	if tokens[*pos].consume_ty(TokenNot, pos) {
		return Node::new_not(unary(tokens, pos));
	}
	if tokens[*pos].consume_ty(TokenTilde, pos) {
		return Node::new_bit(NULL_TY.clone(), TokenTilde, unary(tokens, pos), Node::new_num(1));
	}
	return postfix(tokens, pos);
}

fn mul(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	let mut lhs = unary(tokens, pos);
	
	loop {
		if tokens[*pos].consume_ty(TokenStar, pos) {
			lhs = Node::new_bit(NULL_TY.clone(), TokenStar, lhs, unary(tokens, pos));
		} else if tokens[*pos].consume_ty(TokenDiv, pos) {
			lhs = Node::new_bit(NULL_TY.clone(), TokenDiv, lhs, unary(tokens, pos));
		} else if tokens[*pos].consume_ty(TokenMod, pos) {
			lhs = Node::new_bit(NULL_TY.clone(), TokenMod, lhs, unary(tokens, pos));
		} else {
			return lhs;
		}
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
		lhs = Node::new_bit(NULL_TY.clone(), ty, lhs, rhs);
	}
	
}

fn shift(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	let mut lhs = add(tokens, pos);

	loop {
		if tokens[*pos].consume_ty(TokenShl, pos) {
			lhs = Node::new_bit(NULL_TY.clone(), TokenShl, lhs, add(tokens, pos));
		} else if tokens[*pos].consume_ty(TokenShr, pos) {
			lhs = Node::new_bit(NULL_TY.clone(), TokenShr, lhs, add(tokens, pos));
		} else {
			return lhs;
		}
	}

}

fn relational(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	let mut lhs = shift(tokens, pos);
	
	loop {
		if tokens[*pos].consume_ty(TokenLt, pos) {
			lhs = Node::new_bit(INT_TY.clone(), TokenLt, lhs, shift(tokens, pos));
		} else if tokens[*pos].consume_ty(TokenRt, pos) {
			lhs = Node::new_bit(INT_TY.clone(), TokenLt, shift(tokens, pos), lhs);
		} else if tokens[*pos].consume_ty(TokenLe, pos) {
			lhs = Node::new_bit(INT_TY.clone(), TokenLe, lhs, shift(tokens, pos));
		} else if tokens[*pos].consume_ty(TokenGe, pos) {
			lhs = Node::new_bit(INT_TY.clone(), TokenLe, shift(tokens, pos), lhs);
		} else {
			return lhs;
		}
	}
}

fn equarity(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	let mut lhs = relational(tokens, pos);
	
	loop {
		if tokens[*pos].consume_ty(TokenEqEq, pos) {
			lhs = Node::new_eqeq(lhs, relational(tokens, pos));
		} else if tokens[*pos].consume_ty(TokenNe, pos) {
			lhs = Node::new_neq(lhs, relational(tokens, pos));
		} else {
			return lhs;
		}
	}
}

fn bitand(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	let mut lhs = equarity(tokens, pos);

	while tokens[*pos].consume_ty(TokenAmpersand, pos) {
		lhs = Node::new_bit(INT_TY.clone(), TokenAmpersand, lhs, equarity(tokens, pos));
	}
	return lhs;
}

fn bitxor(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	let mut lhs = bitand(tokens, pos);

	while tokens[*pos].consume_ty(TokenXor, pos) {
		lhs = Node::new_bit(INT_TY.clone(), TokenXor, lhs, bitand(tokens, pos));
	}
	return lhs;
}

fn bitor(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	let mut lhs = bitxor(tokens, pos);

	while tokens[*pos].consume_ty(TokenOr, pos) {
		lhs = Node::new_bit(INT_TY.clone(), TokenOr, lhs, bitxor(tokens, pos));
	}
	return lhs;
}

fn logand(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	let mut lhs = bitor(tokens, pos);

	while tokens[*pos].consume_ty(TokenLogAnd, pos) {
		lhs = Node::new_and(lhs, bitor(tokens, pos));
	}
	return lhs;
}

fn logor(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	let mut lhs = logand(tokens, pos);

	while tokens[*pos].consume_ty(TokenLogOr, pos) {
		lhs = Node::new_or(lhs, logand(tokens, pos));
	}
	return lhs;
}

fn conditional(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	let cond = logor(tokens, pos);
	if tokens[*pos].consume_ty(TokenQuestion, pos) {
		let then = expr(tokens, pos);
		tokens[*pos].assert_ty(TokenColon, pos);
		let els = conditional(tokens, pos);
		return Node::new_ternary(NULL_TY.clone(), cond, then, els);
	}
	return cond;
}

fn assign(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	let mut lhs = conditional(tokens, pos);

	if let Some(op) = assignment_op(tokens, pos) {
		let rhs = assign(tokens, pos);
		match op {
			TokenEq => {
				lhs = Node::new_eq(NULL_TY.clone(), lhs, rhs);
			}
			_ => {
				let llhs = Node::new_bit(NULL_TY.clone(), op, lhs.clone(), rhs);
				lhs = Node::new_eq(NULL_TY.clone(), lhs, llhs);
			}
		}
	}
	return lhs;
}

fn expr(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	let lhs = assign(tokens, pos);
	if tokens[*pos].consume_ty(TokenComma, pos) {
		return Node::new_tuple(NULL_TY.clone(), lhs, expr(tokens, pos));
	}
	return lhs;
}

fn declarator(tokens: &Vec<Token>, pos: &mut usize, mut ty: Type) -> Node {
	
	while tokens[*pos].consume_ty(TokenStar, pos) {
		ty = ty.ptr_to();
	}
	
	return direct_decl(tokens, pos, ty);

}

fn read_array(tokens: &Vec<Token>, pos: &mut usize, ty: Type) -> Type {
	let mut ary_size = vec![];
	let mut ty = ty.clone();

	while tokens[*pos].consume_ty(TokenRightmiddleBrace, pos) {
		if tokens[*pos].consume_ty(TokenLeftmiddleBrace, pos) {
			ary_size.push(0);
			continue;
		}
		let len = expr(tokens, pos);
		if let NodeType::Num(val) = &len.op {
			ary_size.push(*val as usize);
			tokens[*pos].assert_ty(TokenLeftmiddleBrace, pos);
			continue;
		}
		// error(&format!("array declaration is invalid at {}.", tokens[*pos].input));
		// for debug.
		panic!("array declaration is invalid at {}.", &PROGRAMS.lock().unwrap()[tokens[*pos].program_id][tokens[*pos].pos..]);
	}

	if ary_size.len() > 0 {
		for i in (0..ary_size.len()).rev() {
			ty = ty.ary_of(ary_size[i]);
		}
	}

	return ty;
}

fn ident(tokens: &Vec<Token>, pos: &mut usize) -> String {
	// panic!("{}, {}, {}", tokens[*pos].program_id, tokens[*pos].pos, tokens[*pos].val);
	let name = String::from(&PROGRAMS.lock().unwrap()[tokens[*pos].program_id][tokens[*pos].pos..tokens[*pos].end]);
	if !tokens[*pos].consume_ty(TokenIdent, pos) {
		// error(&format!("should be identifier at {}", &tokens[*pos].input[*pos..]));
		// for debug.
		panic!("should be identifier at {}", &PROGRAMS.lock().unwrap()[tokens[*pos].program_id][tokens[*pos].pos..]);
	}
	return name;
}

fn decl_init(tokens: &Vec<Token>, pos: &mut usize, node: &mut Node) {
	if let NodeType::VarDef(_, _, _, _, ref mut init) = node.op {
		if tokens[*pos].consume_ty(TokenEq, pos) {
			let rhs = assign(tokens, pos);
			std::mem::replace(init, Some(Box::new(rhs)));
		}
	}
	return;
}

fn new_ptr_to_replace_type(ctype: &Type, true_ty: Type) -> Type {
	match ctype.ty {
		Ty::NULL => {
			return true_ty;
		}
		_ => {
			return Type::new(
				ctype.ty.clone(),
				Some(Box::new(new_ptr_to_replace_type(ctype.ptr_to.as_ref().unwrap().as_ref(), true_ty))),
				ctype.ary_to.clone(),
				ctype.size,
				ctype.align,
				ctype.offset,
				ctype.len
			)
		}
	}
}

fn direct_decl(tokens: &Vec<Token>, pos: &mut usize, mut ty: Type) -> Node {

	let mut ident_node;
	
	if tokens[*pos].consume_ty(TokenIdent, pos) {
		*pos -= 1;
		let name = ident(tokens, pos);
		ty = read_array(tokens, pos, ty);
		ident_node = Node::new_vardef(ty, false, name, 0, None);
	} else if tokens[*pos].consume_ty(TokenRightBrac, pos) {
		ident_node = declarator(tokens, pos, NULL_TY.clone());
		tokens[*pos].assert_ty(TokenLeftBrac, pos);
		
		let true_ty = read_array(tokens, pos, ty);
		let ident_node_true_ty = new_ptr_to_replace_type(&ident_node.nodesctype(None), true_ty);

		if let NodeType::VarDef(_, false, name, offset, init) = ident_node.op {
			let op = NodeType::VarDef(ident_node_true_ty, false, name, offset, init);
			return Node { op };
		} else {
			panic!("direct_decl fun error.");
		}
	} else {
		// for debug
		panic!("bad direct declarator at {}", &PROGRAMS.lock().unwrap()[tokens[*pos].program_id][tokens[*pos].pos..]);
		// error(&format!("bad direct declarator at {}", &tokens[*pos].input[..]));
	}
	
	decl_init(tokens, pos, &mut ident_node);
	return ident_node;
}

fn declaration(tokens: &Vec<Token>, pos: &mut usize) -> Node {

	// declaration type
	let ty = decl_specifiers(tokens, pos);

	let ident_node = declarator(tokens, pos, ty);
	tokens[*pos].assert_ty(TokenSemi, pos);
	// panic!("{:#?}", ident_node);
	return ident_node;
}

fn expr_stmt(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	let lhs = expr(tokens, pos);
	tokens[*pos].consume_ty(TokenSemi, pos);
	return Node::new_expr(lhs);
}

pub fn stmt(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	
	match tokens[*pos].ty {
		TokenRet => {
			*pos += 1;
			let lhs = expr(tokens, pos);
			tokens[*pos].assert_ty(TokenSemi, pos);
			return Node::new_ret(lhs);
		},
		TokenIf => {
			*pos += 1;
			tokens[*pos].assert_ty(TokenRightBrac, pos);
			let cond = expr(tokens, pos);
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
				init = declaration(tokens, pos);
			} else if tokens[*pos].consume_ty(TokenSemi, pos) {
				init = Node::new_null();
			} else {
				init = expr_stmt(tokens, pos);
			}
			let mut cond = Node::new_null();
			if !tokens[*pos].consume_ty(TokenSemi, pos) {
				cond = expr(tokens, pos);
				tokens[*pos].assert_ty(TokenSemi, pos);
			}
			let mut inc = Node::new_null();
			if !tokens[*pos].consume_ty(TokenLeftBrac, pos) {
				inc = stmt(tokens, pos);
				tokens[*pos].assert_ty(TokenLeftBrac, pos);
			} 
			let body = stmt(tokens, pos);
			return Node::new_for(init, cond, inc, body);
		}
		TokenWhile => {
			*pos += 1;
			let init = Node::new_null();
			let inc = Node::new_null();
			tokens[*pos].assert_ty(TokenRightBrac, pos);
			let cond = expr(tokens, pos);
			tokens[*pos].assert_ty(TokenLeftBrac, pos);
			let body = stmt(tokens, pos);
			return Node::new_for(init, cond, inc, body);
		}
		TokenDo => {
			*pos += 1;
			let body = stmt(tokens, pos);
			tokens[*pos].assert_ty(TokenWhile, pos);
			tokens[*pos].assert_ty(TokenRightBrac, pos);
			let cond = expr(tokens, pos);
			tokens[*pos].assert_ty(TokenLeftBrac, pos);
			tokens[*pos].assert_ty(TokenSemi, pos);
			return Node::new_dowhile(body, cond);
		}
		TokenRightCurlyBrace => {
			*pos += 1;
			let mut compstmts = vec![];
			while !tokens[*pos].consume_ty(TokenLeftCurlyBrace, pos) {
				compstmts.push(stmt(tokens, pos));
			}
			return Node::new_stmt(compstmts);
		}
		TokenInt | TokenChar | TokenStruct => {
			return declaration(tokens, pos);
		}
		TokenSemi => {
			*pos += 1;
			return Node::new_null();
		}
		TokenTypedef => {
			*pos += 1;
			let lhs = declaration(tokens, pos);
			if let NodeType::VarDef(ctype, _, name, _, None) = lhs.op {
				ENV.lock().unwrap().typedefs.insert(name, ctype);
				return Node::new_null();
			}
			panic!("typedef error.");
		}
		TokenBreak => {
			*pos += 1;
			return Node::new_break();
		}
		_ => {
			if tokens[*pos].consume_ty(TokenIdent, pos) {
				if tokens[*pos].consume_ty(TokenIdent, pos) {
					*pos -= 2;
					return declaration(tokens, pos);
				}
				*pos -= 1;
			}
			return expr_stmt(tokens, pos);
		}
	}
}

pub fn compound_stmt(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	
	let mut compstmts = vec![];
	tokens[*pos].assert_ty(TokenRightCurlyBrace, pos);
	let env = (*ENV.lock().unwrap()).clone();
	*ENV.lock().unwrap() = Env::new_env(Some(env));
	loop {
		match tokens[*pos].consume_ty(TokenLeftCurlyBrace, pos) {
			true => { break; },
			false => { 
				let stmt = stmt(tokens, pos);
				compstmts.push(stmt);
			}
		}
	}
	let env = (*ENV.lock().unwrap()).clone();
	*ENV.lock().unwrap() = *env.next.unwrap();
	return Node::new_stmt(compstmts);
}

pub fn param_declaration(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	
	// type
	let ty = decl_specifiers(tokens, pos);
	let mut node = declarator(tokens, pos, ty);
	let ctype = node.nodesctype(None);
	if let Ty::ARY = &ctype.ty {
		let new_ty = ctype.ary_to.unwrap().clone().ptr_to();
		node.ctype_replace(new_ty);
	}

	return node;
}

pub fn toplevel(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	
	let mut args = vec![];

	let is_extern = tokens[*pos].consume_ty(TokenExtern, pos);
	// if is_extern {
	// 	panic!("rerererererer {}", *pos);
	// }
	let is_typedef = tokens[*pos].consume_ty(TokenTypedef, pos);

	// Ctype
	let mut ctype = decl_specifiers(tokens, pos);
	
	while tokens[*pos].consume_ty(TokenStar, pos) {
		ctype = ctype.ptr_to();
	}
	
	// identifier
	let ident = ident(tokens, pos);
	
	// function
	if tokens[*pos].consume_ty(TokenRightBrac, pos){
		if is_typedef {
			// error(&format!("typedef {} has function definition.", name));
			// for debug.
			panic!("typedef {} has function definition.", ident);
		}
		// argument
		if !tokens[*pos].consume_ty(TokenLeftBrac, pos) {
			loop {
				args.push(param_declaration(tokens, pos));
				if tokens[*pos].consume_ty(TokenLeftBrac, pos){ break; }
				tokens[*pos].assert_ty(TokenComma, pos);
			}
		}
		// function decl
		if tokens[*pos].consume_ty(TokenSemi, pos) {
			return Node::new_decl(ctype, ident, args, is_extern);
		}
		// body
		let body = compound_stmt(tokens, pos);
		return Node::new_func(ctype, ident, is_extern, args, body, 0);
	}

	ctype = read_array(tokens, pos, ctype);
	tokens[*pos].assert_ty(TokenSemi, pos);
	// typedef
	if is_typedef {
		ENV.lock().unwrap().typedefs.insert(ident, ctype);
		return Node::new_null();
	}
	// global variable
	return Node::new_vardef(ctype, is_extern, ident, 0, None);

}

pub fn parse(tokens: &Vec<Token>, pos: &mut usize) -> Vec<Node> {
	
	let mut program = vec![];
	let env = (*ENV.lock().unwrap()).clone();
	*ENV.lock().unwrap() = Env::new_env(Some(env));

	loop {
		match tokens[*pos].consume_ty(TokenEof, pos) {
			true => { break; }
			false => { program.push(toplevel(tokens, pos)); }
		}
	}

	return program;
}