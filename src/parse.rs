use super::token::*;
use super::token::TokenType::*;
// use super::lib::*;
use super::mir::*;

use std::collections::HashMap;
use std::sync::Mutex;

// This is a recursive-descendent parser which constructs abstract
// syntax tree from input tokenset.
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
	Dot(Type, Box<Node>, String),												// Dot(ctype, expr, name)
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
	pub fn new_dot(ctype: Type, expr: Node, member: String) -> Self {
		Self {
			op: NodeType::Dot(ctype, Box::new(expr), member)
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

pub fn decl_specifiers(tokenset: &mut TokenSet) -> Type {
	if tokenset.consume_ty(TokenIdent) {
		tokenset.pos -= 1;
		let name = ident(tokenset);
		env_find!(name, typedefs, NULL_TY.clone());
	}
	if tokenset.consume_ty(TokenInt){
		return INT_TY.clone();
	}
	if tokenset.consume_ty(TokenChar){
		return CHAR_TY.clone();
	}
	if tokenset.consume_ty(TokenStruct){
		
		let mut members = vec![];
		let mut tag = String::new();
		// tag
		if tokenset.consume_ty(TokenIdent) {
			tokenset.pos -= 1;
			tag = ident(tokenset);
		}
		
		// struct member
		if tokenset.consume_ty(TokenRightCurlyBrace) {
			while !tokenset.consume_ty(TokenLeftCurlyBrace) {
				members.push(declaration(tokenset));
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
	if tokenset.consume_ty(TokenVoid) {
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

fn assignment_op(tokenset: &mut TokenSet) -> Option<TokenType> {
	if tokenset.consume_ty(TokenAddEq) { return Some(TokenAdd); }
	else if tokenset.consume_ty(TokenSubEq) { return Some(TokenSub); }
	else if tokenset.consume_ty(TokenMulEq) { return Some(TokenStar); }
	else if tokenset.consume_ty(TokenDivEq) { return Some(TokenDiv); }
	else if tokenset.consume_ty(TokenModEq) { return Some(TokenMod); }
	else if tokenset.consume_ty(TokenShlEq) { return Some(TokenShl); }
	else if tokenset.consume_ty(TokenShrEq) { return Some(TokenShr); }
	else if tokenset.consume_ty(TokenAndEq) { return Some(TokenAmpersand); }
	else if tokenset.consume_ty(TokenOrEq) { return Some(TokenOr); }
	else if tokenset.consume_ty(TokenXorEq) { return Some(TokenXor); }
	else if tokenset.consume_ty(TokenEq) { return Some(TokenEq); }
	else { return None; }
}

fn primary(tokenset: &mut TokenSet) -> Node {
	
	if tokenset.consume_ty(TokenRightBrac) {
		if tokenset.consume_ty(TokenRightCurlyBrace) {
			tokenset.pos -= 1;
			let body = Node::new_stmtexpr(INT_TY.clone(), compound_stmt(tokenset));
			tokenset.assert_ty(TokenLeftBrac);
			return body;
		}
		let lhs = expr(tokenset);
		tokenset.assert_ty(TokenLeftBrac);
		return lhs;
	}
	if tokenset.consume_ty(TokenNum) {
		return Node::new_num(tokenset.tokens[tokenset.pos-1].val);
	}
	if tokenset.consume_ty(TokenIdent) {

		let token = &tokenset.tokens[tokenset.pos-1];
		let name = String::from(&PROGRAMS.lock().unwrap()[token.program_id][token.pos..token.end]);
		// variable
		if !tokenset.consume_ty(TokenRightBrac){
			return Node::new_ident(name);
		}

		// function call
		let mut args = vec![];
		//// arity = 0;
		if tokenset.consume_ty(TokenLeftBrac){
			return Node::new_call(NULL_TY.clone(), name, args);
		}
		//// arity > 0;
		let arg1 = assign(tokenset);
		args.push(arg1);
		while tokenset.consume_ty(TokenComma) {
			let argv = assign(tokenset);
			args.push(argv);
		}
		tokenset.assert_ty(TokenLeftBrac);
		return Node::new_call(NULL_TY.clone(), name, args);
	}
	if tokenset.consume_ty(TokenString(String::new())) {
		let strname = tokenset.getstring();
		let cty = CHAR_TY.clone().ary_of(strname.len() + 1);
		tokenset.pos += 1;
		return Node::new_string(cty, strname, 0);
	}
	// error(&format!("parse.rs: primary parse fail. and got {}", tokenset[*pos].input));
	// for debug.
	let token = &tokenset.tokens[tokenset.pos];
	panic!("parse.rs: primary parse fail. and got rerere{:?}rererer {}", token, &PROGRAMS.lock().unwrap()[token.program_id][token.pos..]);
}

fn postfix(tokenset: &mut TokenSet) -> Node {
	
	let mut lhs = primary(tokenset);

	loop {
		if tokenset.consume_ty(TokenInc) {
			lhs = Node::new_incdec(NULL_TY.clone(), 1, lhs);
		}
		if tokenset.consume_ty(TokenDec) {
			lhs = Node::new_incdec(NULL_TY.clone(), 2, lhs);
		}
		// struct member
		if tokenset.consume_ty(TokenDot) {
			let name = ident(tokenset);
			lhs = Node::new_dot(NULL_TY.clone(), lhs, name);
		// struct member arrow
		} else if tokenset.consume_ty(TokenArrow) {
			let name = ident(tokenset);
			let expr = Node::new_deref(INT_TY.clone(), lhs);
			lhs = Node::new_dot(NULL_TY.clone(), expr, name);
		// array
		} else if tokenset.consume_ty(TokenRightmiddleBrace)  {
			let id = assign(tokenset);
			let lhs2 = Node::new_bit(INT_TY.clone(), TokenAdd, lhs, id);
			lhs = Node::new_deref(INT_TY.clone(), lhs2);
			tokenset.assert_ty(TokenLeftmiddleBrace);
		} else {
			return lhs;
		}
	}
}

fn unary(tokenset: &mut TokenSet) -> Node {
	
	if tokenset.consume_ty(TokenInc) {
		let lhs = unary(tokenset);
		let rhs = Node::new_bit(NULL_TY.clone(), TokenAdd, lhs.clone(), Node::new_num(1));
		return Node::new_eq(NULL_TY.clone(), lhs, rhs);
	}
	if tokenset.consume_ty(TokenDec) {
		let lhs = unary(tokenset);
		let rhs = Node::new_bit(NULL_TY.clone(), TokenSub, lhs.clone(), Node::new_num(1));
		return Node::new_eq(NULL_TY.clone(), lhs, rhs);
	}
	if tokenset.consume_ty(TokenSub) {
		return Node::new_neg(unary(tokenset));
	}
	if tokenset.consume_ty(TokenStar) {
		return Node::new_deref(INT_TY.clone(), unary(tokenset));
	}
	if tokenset.consume_ty(TokenAmpersand) {
		return Node::new_addr(INT_TY.clone(), unary(tokenset));
	}
	if tokenset.consume_ty(TokenSizeof) {
		return Node::new_sizeof(INT_TY.clone(), 0, unary(tokenset));
	}
	if tokenset.consume_ty(TokenAlignof) {
		return Node::new_alignof(unary(tokenset));
	}
	if tokenset.consume_ty(TokenNot) {
		return Node::new_not(unary(tokenset));
	}
	if tokenset.consume_ty(TokenTilde) {
		return Node::new_bit(NULL_TY.clone(), TokenTilde, unary(tokenset), Node::new_num(1));
	}
	return postfix(tokenset);
}

fn mul(tokenset: &mut TokenSet) -> Node {
	let mut lhs = unary(tokenset);
	
	loop {
		if tokenset.consume_ty(TokenStar) {
			lhs = Node::new_bit(NULL_TY.clone(), TokenStar, lhs, unary(tokenset));
		} else if tokenset.consume_ty(TokenDiv) {
			lhs = Node::new_bit(NULL_TY.clone(), TokenDiv, lhs, unary(tokenset));
		} else if tokenset.consume_ty(TokenMod) {
			lhs = Node::new_bit(NULL_TY.clone(), TokenMod, lhs, unary(tokenset));
		} else {
			return lhs;
		}
	}

}

fn add(tokenset: &mut TokenSet) -> Node {
	let mut lhs = mul(tokenset);
	
	loop {
		if !tokenset.consume_ty(TokenAdd) && !tokenset.consume_ty(TokenSub) {
			return lhs;
		}
		let ty = tokenset.tokens[tokenset.pos-1].ty.clone();
		let rhs = mul(tokenset);
		lhs = Node::new_bit(NULL_TY.clone(), ty, lhs, rhs);
	}
	
}

fn shift(tokenset: &mut TokenSet) -> Node {
	let mut lhs = add(tokenset);

	loop {
		if tokenset.consume_ty(TokenShl) {
			lhs = Node::new_bit(NULL_TY.clone(), TokenShl, lhs, add(tokenset));
		} else if tokenset.consume_ty(TokenShr) {
			lhs = Node::new_bit(NULL_TY.clone(), TokenShr, lhs, add(tokenset));
		} else {
			return lhs;
		}
	}

}

fn relational(tokenset: &mut TokenSet) -> Node {
	let mut lhs = shift(tokenset);
	
	loop {
		if tokenset.consume_ty(TokenLt) {
			lhs = Node::new_bit(INT_TY.clone(), TokenLt, lhs, shift(tokenset));
		} else if tokenset.consume_ty(TokenRt) {
			lhs = Node::new_bit(INT_TY.clone(), TokenLt, shift(tokenset), lhs);
		} else if tokenset.consume_ty(TokenLe) {
			lhs = Node::new_bit(INT_TY.clone(), TokenLe, lhs, shift(tokenset));
		} else if tokenset.consume_ty(TokenGe) {
			lhs = Node::new_bit(INT_TY.clone(), TokenLe, shift(tokenset), lhs);
		} else {
			return lhs;
		}
	}
}

fn equarity(tokenset: &mut TokenSet) -> Node {
	let mut lhs = relational(tokenset);
	
	loop {
		if tokenset.consume_ty(TokenEqEq) {
			lhs = Node::new_eqeq(lhs, relational(tokenset));
		} else if tokenset.consume_ty(TokenNe) {
			lhs = Node::new_neq(lhs, relational(tokenset));
		} else {
			return lhs;
		}
	}
}

fn bitand(tokenset: &mut TokenSet) -> Node {
	let mut lhs = equarity(tokenset);

	while tokenset.consume_ty(TokenAmpersand) {
		lhs = Node::new_bit(INT_TY.clone(), TokenAmpersand, lhs, equarity(tokenset));
	}
	return lhs;
}

fn bitxor(tokenset: &mut TokenSet) -> Node {
	let mut lhs = bitand(tokenset);

	while tokenset.consume_ty(TokenXor) {
		lhs = Node::new_bit(INT_TY.clone(), TokenXor, lhs, bitand(tokenset));
	}
	return lhs;
}

fn bitor(tokenset: &mut TokenSet) -> Node {
	let mut lhs = bitxor(tokenset);

	while tokenset.consume_ty(TokenOr) {
		lhs = Node::new_bit(INT_TY.clone(), TokenOr, lhs, bitxor(tokenset));
	}
	return lhs;
}

fn logand(tokenset: &mut TokenSet) -> Node {
	let mut lhs = bitor(tokenset);

	while tokenset.consume_ty(TokenLogAnd) {
		lhs = Node::new_and(lhs, bitor(tokenset));
	}
	return lhs;
}

fn logor(tokenset: &mut TokenSet) -> Node {
	let mut lhs = logand(tokenset);

	while tokenset.consume_ty(TokenLogOr) {
		lhs = Node::new_or(lhs, logand(tokenset));
	}
	return lhs;
}

fn conditional(tokenset: &mut TokenSet) -> Node {
	let cond = logor(tokenset);
	if tokenset.consume_ty(TokenQuestion) {
		let then = expr(tokenset);
		tokenset.assert_ty(TokenColon);
		let els = conditional(tokenset);
		return Node::new_ternary(NULL_TY.clone(), cond, then, els);
	}
	return cond;
}

fn assign(tokenset: &mut TokenSet) -> Node {
	let mut lhs = conditional(tokenset);

	if let Some(op) = assignment_op(tokenset) {
		let rhs = assign(tokenset);
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

fn expr(tokenset: &mut TokenSet) -> Node {
	let lhs = assign(tokenset);
	if tokenset.consume_ty(TokenComma) {
		return Node::new_tuple(NULL_TY.clone(), lhs, expr(tokenset));
	}
	return lhs;
}

fn declarator(tokenset: &mut TokenSet, mut ty: Type) -> Node {
	
	while tokenset.consume_ty(TokenStar) {
		ty = ty.ptr_to();
	}
	
	return direct_decl(tokenset, ty);

}

fn read_array(tokenset: &mut TokenSet, ty: Type) -> Type {
	let mut ary_size = vec![];
	let mut ty = ty.clone();

	while tokenset.consume_ty(TokenRightmiddleBrace) {
		if tokenset.consume_ty(TokenLeftmiddleBrace) {
			ary_size.push(0);
			continue;
		}
		let len = expr(tokenset);
		if let NodeType::Num(val) = &len.op {
			ary_size.push(*val as usize);
			tokenset.assert_ty(TokenLeftmiddleBrace);
			continue;
		}
		// error(&format!("array declaration is invalid at {}.", tokenset[*pos].input));
		// for debug.
		let token = &tokenset.tokens[tokenset.pos];
		panic!("array declaration is invalid at {}.", &PROGRAMS.lock().unwrap()[token.program_id][token.pos..]);
	}

	if ary_size.len() > 0 {
		for i in (0..ary_size.len()).rev() {
			ty = ty.ary_of(ary_size[i]);
		}
	}

	return ty;
}

fn ident(tokenset: &mut TokenSet) -> String {
	// panic!("{}, {}, {}", tokenset[*pos].program_id, tokenset[*pos].pos, tokenset[*pos].val);
	let token = tokenset.tokens[tokenset.pos].clone();
	let name = String::from(&PROGRAMS.lock().unwrap()[token.program_id][token.pos..token.end]);
	if !tokenset.consume_ty(TokenIdent) {
		// error(&format!("should be identifier at {}", &tokenset[*pos].input[*pos..]));
		// for debug.
		panic!("should be identifier at {}", &PROGRAMS.lock().unwrap()[token.program_id][token.pos..]);
	}
	return name;
}

fn decl_init(tokenset: &mut TokenSet, node: &mut Node) {
	if let NodeType::VarDef(_, _, _, _, ref mut init) = node.op {
		if tokenset.consume_ty(TokenEq) {
			let rhs = assign(tokenset);
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

fn direct_decl(tokenset: &mut TokenSet, mut ty: Type) -> Node {

	let mut ident_node;
	
	if tokenset.consume_ty(TokenIdent) {
		tokenset.pos -= 1;
		let name = ident(tokenset);
		ty = read_array(tokenset, ty);
		ident_node = Node::new_vardef(ty, false, name, 0, None);
	} else if tokenset.consume_ty(TokenRightBrac) {
		ident_node = declarator(tokenset, NULL_TY.clone());
		tokenset.assert_ty(TokenLeftBrac);
		
		let true_ty = read_array(tokenset, ty);
		let ident_node_true_ty = new_ptr_to_replace_type(&ident_node.nodesctype(None), true_ty);

		if let NodeType::VarDef(_, false, name, offset, init) = ident_node.op {
			let op = NodeType::VarDef(ident_node_true_ty, false, name, offset, init);
			return Node { op };
		} else {
			panic!("direct_decl fun error.");
		}
	} else {
		// for debug
		let token = &tokenset.tokens[tokenset.pos];
		panic!("bad direct declarator at {}", &PROGRAMS.lock().unwrap()[token.program_id][token.pos..]);
		// error(&format!("bad direct declarator at {}", &tokenset[*pos].input[..]));
	}
	
	decl_init(tokenset, &mut ident_node);
	return ident_node;
}

fn declaration(tokenset: &mut TokenSet) -> Node {

	// declaration type
	let ty = decl_specifiers(tokenset);

	let ident_node = declarator(tokenset, ty);
	tokenset.assert_ty(TokenSemi);
	// panic!("{:#?}", ident_node);
	return ident_node;
}

fn expr_stmt(tokenset: &mut TokenSet) -> Node {
	let lhs = expr(tokenset);
	tokenset.consume_ty(TokenSemi);
	return Node::new_expr(lhs);
}

pub fn stmt(tokenset: &mut TokenSet) -> Node {
	
	match tokenset.tokens[tokenset.pos].ty {
		TokenRet => {
			tokenset.pos += 1;
			let lhs = expr(tokenset);
			tokenset.assert_ty(TokenSemi);
			return Node::new_ret(lhs);
		},
		TokenIf => {
			tokenset.pos += 1;
			tokenset.assert_ty(TokenRightBrac);
			let cond = expr(tokenset);
			tokenset.assert_ty(TokenLeftBrac);
			let then = stmt(tokenset);
			if tokenset.consume_ty(TokenElse) {
				let elthen = stmt(tokenset);
				return Node::new_if(cond, then, Some(elthen));
			} else {
				return Node::new_if(cond, then, None);
			}
		}
		TokenFor => {
			tokenset.pos += 1;
			tokenset.assert_ty(TokenRightBrac);
			let init;
			if tokenset.is_typename() {
				tokenset.pos -= 1;
				init = declaration(tokenset);
			} else if tokenset.consume_ty(TokenSemi) {
				init = Node::new_null();
			} else {
				init = expr_stmt(tokenset);
			}
			let mut cond = Node::new_null();
			if !tokenset.consume_ty(TokenSemi) {
				cond = expr(tokenset);
				tokenset.assert_ty(TokenSemi);
			}
			let mut inc = Node::new_null();
			if !tokenset.consume_ty(TokenLeftBrac) {
				inc = stmt(tokenset);
				tokenset.assert_ty(TokenLeftBrac);
			} 
			let body = stmt(tokenset);
			return Node::new_for(init, cond, inc, body);
		}
		TokenWhile => {
			tokenset.pos += 1;
			let init = Node::new_null();
			let inc = Node::new_null();
			tokenset.assert_ty(TokenRightBrac);
			let cond = expr(tokenset);
			tokenset.assert_ty(TokenLeftBrac);
			let body = stmt(tokenset);
			return Node::new_for(init, cond, inc, body);
		}
		TokenDo => {
			tokenset.pos += 1;
			let body = stmt(tokenset);
			tokenset.assert_ty(TokenWhile);
			tokenset.assert_ty(TokenRightBrac);
			let cond = expr(tokenset);
			tokenset.assert_ty(TokenLeftBrac);
			tokenset.assert_ty(TokenSemi);
			return Node::new_dowhile(body, cond);
		}
		TokenRightCurlyBrace => {
			tokenset.pos += 1;
			let mut compstmts = vec![];
			while !tokenset.consume_ty(TokenLeftCurlyBrace) {
				compstmts.push(stmt(tokenset));
			}
			return Node::new_stmt(compstmts);
		}
		TokenInt | TokenChar | TokenStruct => {
			return declaration(tokenset);
		}
		TokenSemi => {
			tokenset.pos += 1;
			return Node::new_null();
		}
		TokenTypedef => {
			tokenset.pos += 1;
			let lhs = declaration(tokenset);
			if let NodeType::VarDef(ctype, _, name, _, None) = lhs.op {
				ENV.lock().unwrap().typedefs.insert(name, ctype);
				return Node::new_null();
			}
			panic!("typedef error.");
		}
		TokenBreak => {
			tokenset.pos += 1;
			return Node::new_break();
		}
		_ => {
			if tokenset.consume_ty(TokenIdent) {
				if tokenset.consume_ty(TokenIdent) {
					tokenset.pos -= 2;
					return declaration(tokenset);
				}
				tokenset.pos -= 1;
			}
			return expr_stmt(tokenset);
		}
	}
}

pub fn compound_stmt(tokenset: &mut TokenSet) -> Node {
	
	let mut compstmts = vec![];
	tokenset.assert_ty(TokenRightCurlyBrace);
	let env = (*ENV.lock().unwrap()).clone();
	*ENV.lock().unwrap() = Env::new_env(Some(env));
	loop {
		match tokenset.consume_ty(TokenLeftCurlyBrace) {
			true => { break; },
			false => { 
				let stmt = stmt(tokenset);
				compstmts.push(stmt);
			}
		}
	}
	let env = (*ENV.lock().unwrap()).clone();
	*ENV.lock().unwrap() = *env.next.unwrap();
	return Node::new_stmt(compstmts);
}

pub fn param_declaration(tokenset: &mut TokenSet) -> Node {
	
	// type
	let ty = decl_specifiers(tokenset);
	let mut node = declarator(tokenset, ty);
	let ctype = node.nodesctype(None);
	if let Ty::ARY = &ctype.ty {
		let new_ty = ctype.ary_to.unwrap().clone().ptr_to();
		node.ctype_replace(new_ty);
	}

	return node;
}

pub fn toplevel(tokenset: &mut TokenSet) -> Node {
	
	let mut args = vec![];

	let is_extern = tokenset.consume_ty(TokenExtern);
	// if is_extern {
	// 	panic!("rerererererer {}", *pos);
	// }
	let is_typedef = tokenset.consume_ty(TokenTypedef);

	// Ctype
	let mut ctype = decl_specifiers(tokenset);
	
	while tokenset.consume_ty(TokenStar) {
		ctype = ctype.ptr_to();
	}
	
	// identifier
	let ident = ident(tokenset);
	
	// function
	if tokenset.consume_ty(TokenRightBrac){
		if is_typedef {
			// error(&format!("typedef {} has function definition.", name));
			// for debug.
			panic!("typedef {} has function definition.", ident);
		}
		// argument
		if !tokenset.consume_ty(TokenLeftBrac) {
			loop {
				args.push(param_declaration(tokenset));
				if tokenset.consume_ty(TokenLeftBrac){ break; }
				tokenset.assert_ty(TokenComma);
			}
		}
		// function decl
		if tokenset.consume_ty(TokenSemi) {
			return Node::new_decl(ctype, ident, args, is_extern);
		}
		// body
		let body = compound_stmt(tokenset);
		return Node::new_func(ctype, ident, is_extern, args, body, 0);
	}

	ctype = read_array(tokenset, ctype);
	tokenset.assert_ty(TokenSemi);
	// typedef
	if is_typedef {
		ENV.lock().unwrap().typedefs.insert(ident, ctype);
		return Node::new_null();
	}
	// global variable
	return Node::new_vardef(ctype, is_extern, ident, 0, None);

}

pub fn parse(tokenset: &mut TokenSet, program: &mut Program) {
	
	let env = (*ENV.lock().unwrap()).clone();
	*ENV.lock().unwrap() = Env::new_env(Some(env));

	loop {
		match tokenset.consume_ty(TokenEof) {
			true => { break; }
			false => { program.nodes.push(toplevel(tokenset)); }
		}
	}
}