use super::token::*;
use super::token::TokenType::*;
use super::gen_ir::IrOp;
use std::collections::HashMap;
use std::sync::Mutex;

// This is a recursive-descendent parser which constructs abstract
// syntax tree from input tokens.
//
// This parser knows only about BNF of the C grammer and doesn't care
// about its semantics. Therefore, some invalid expressions, such as
// `1+2=3`, are accepted by this parser, but that's intentional.
// Semantic errors are detected in a later pass.

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
	pub fn choose_insn(&self, op8: IrOp, op32: IrOp, op64: IrOp) -> IrOp {
		match self.size {
			1 => { return op8; }
			4 => { return op32; }
			c => { 
				if c != 8 {
					panic!("{:?}", self)
				}; 
				return op64; 
			}
		}
	}
	pub fn load_insn(&self) -> IrOp {
		return self.choose_insn(IrOp::IrLoad8, IrOp::IrLoad32, IrOp::IrLoad64);
	}
	pub fn store_insn(&self) -> IrOp {
		return self.choose_insn( IrOp::IrStore8, IrOp::IrStore32, IrOp::IrStore64);
	}
	pub fn store_arg_insn(&self) -> IrOp {
		return self.choose_insn(IrOp::IrStoreArgs8, IrOp::IrStoreArgs32, IrOp::IrStoreArgs64);
	}
}

#[derive(Debug, Clone)]
pub enum Ty {
	INT,
	PTR,
	ARY,
	CHAR,
	STRUCT(Vec<Node>),
	VOID,
	NULL,
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
	Call(String, Vec<Node>),													// Call(ident, args)
	Func(String, bool, Vec<Node>, Box<Node>, usize),							// Func(ident, is_extern, args, body, stacksize)
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
	NULL,																		// NULL
}

#[allow(dead_code)]
impl NodeType {
	fn num_init(val: i32) -> Self {
		NodeType::Num(val)
	}

	fn bit_init(ctype: Type, tk_ty: TokenType, lhs: Node, rhs: Node) -> Self {
		NodeType::BinaryTree(ctype, tk_ty, Box::new(lhs), Box::new(rhs))
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

	fn ident_init(ident: String) -> Self {
		NodeType::Ident(ident)
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

	fn func_init(ident: String, is_extern: bool, args: Vec<Node>, body: Node, stacksize: usize) -> Self {
		NodeType::Func(ident, is_extern, args, Box::new(body), stacksize)
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

	fn vardef_init(ty: Type, is_extern: bool, name: String, off: usize, rhs: Option<Node>) -> Self {
		match rhs {
			Some(node) => { NodeType::VarDef(ty, is_extern, name, off, Some(Box::new(node))) }
			_ => { NodeType::VarDef(ty, is_extern, name, off, None)}
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

	fn gvar_init(ctype: Type, label: String) -> Self {
		NodeType::Gvar(ctype, label)
	}

	fn eqeq_init(lhs: Node, rhs: Node) -> Self {
		NodeType::EqEq(Box::new(lhs), Box::new(rhs))
	}

	fn neq_init(lhs: Node, rhs: Node) -> Self {
		NodeType::Ne(Box::new(lhs), Box::new(rhs))
	}

	fn dowhile_init(body: Node, cond: Node) -> Self {
		NodeType::DoWhile(Box::new(body), Box::new(cond))
	}

	fn stmtexpr_init(ctype: Type, body: Node) -> Self {
		NodeType::StmtExpr(ctype, Box::new(body))
	}

	fn null_init() -> Self {
		NodeType::NULL
	}

	fn alignof_init(expr: Node) -> Self {
		NodeType::Alignof(Box::new(expr))
	}

	fn dot_init(ctype: Type, expr: Node, member: String, offset: usize) -> Self {
		NodeType::Dot(ctype, Box::new(expr), member, offset)
	}

	fn not_init(expr: Node) -> Self {
		NodeType::Not(Box::new(expr))
	}
}

#[derive(Debug, Clone)]
pub struct Node {
	pub op: NodeType,
}

#[allow(dead_code)]
impl Node {
	
	pub fn nodesctype(&self, basetype: Option<Type>) -> Type {
		match &self.op {
			NodeType::Lvar(ctype, _) | NodeType::BinaryTree(ctype, _, _, _) 
			| NodeType::Deref(ctype, _) | NodeType::Addr(ctype, _) 
			| NodeType::Sizeof(ctype, _, _) | NodeType::Str(ctype, _, _)
			| NodeType::Gvar(ctype, _) | NodeType::Dot(ctype, _, _, _) => { 
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

	pub fn checklval(&self) {
		match &self.op {
			NodeType::Lvar(_, _) | NodeType::Gvar(_, _) | NodeType::Deref(_, _) | NodeType::Dot(_, _, _, _) => {}
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

	pub fn new_ident(ident: String) -> Self {
		Self {
			op: NodeType::ident_init(ident)
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

	pub fn new_func(ident: String, is_extern: bool, args: Vec<Node>, body: Node, stacksize: usize) -> Self {
		Self {
			op: NodeType::func_init(ident, is_extern, args, body, stacksize)
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

	pub fn new_vardef(ty: Type, is_extern: bool, name: String, off: usize, rhs: Option<Node>) -> Self {
		Self {
			op: NodeType::vardef_init(ty, is_extern, name, off, rhs)
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

	pub fn new_gvar(ctype: Type, label: String) -> Self {
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

	pub fn new_dowhile(body: Node, cond: Node) -> Self {
		Self {
			op: NodeType::dowhile_init(body, cond)
		}
	}

	pub fn new_stmtexpr(ctype: Type, body: Node) -> Self {
		Self {
			op: NodeType::stmtexpr_init(ctype, body)
		}
	}

	pub fn new_null() -> Self {
		Self {
			op: NodeType::null_init()
		}
	}

	pub fn new_alignof(expr: Node) -> Self {
		Self {
			op: NodeType::alignof_init(expr)
		}
	}

	pub fn new_dot(ctype: Type, expr: Node, name: String, offset: usize) -> Self {
		Self {
			op: NodeType::dot_init(ctype, expr, name, offset)
		}
	}

	pub fn new_not(expr: Node) -> Self {
		Self {
			op: NodeType::not_init(expr)
		}
	}
}

#[derive(Debug, Clone)]
pub struct Env {
	tags: HashMap<String, Vec<Node>>,
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

pub fn read_type(tokens: &Vec<Token>,  pos: &mut usize) -> Type {
	if tokens[*pos].consume_ty(TokenIdent, pos) {
		*pos -= 1;
		let name = ident(tokens, pos);
		if let Some(ty) = ENV.lock().unwrap().typedefs.get(&name) {
			return ty.clone();
		}
		panic!("{} is no defined type.", name);
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
				members.push(decl(tokens, pos));
			}
		}

		// new struct type
		if !tag.is_empty() && !members.is_empty() {
			ENV.lock().unwrap().tags.insert(tag, members.clone());
		// use existed struct type
		} else if !tag.is_empty() && members.is_empty() {
			members = ENV.lock().unwrap().tags.get(&tag).unwrap().clone();
		} else if tag.is_empty() && members.is_empty() {
			panic!("bat struct definition.");
		}
		return struct_of(members);
	}
	if tokens[*pos].consume_ty(TokenVoid, pos) {
		return VOID_TY.clone();
	}
	return NULL_TY.clone();
}

pub fn struct_of(mut members: Vec<Node>) -> Type {
	let mut ty_align = 0;

	let mut off = 0;
	for i in 0..members.len() {
		if let NodeType::VarDef(ctype, _, _, _, _) = &mut members[i].op {
			off = roundup(off, ctype.align);
			ctype.offset = off;
			off += ctype.size;
			ty_align = std::cmp::max(ty_align, ctype.align);
		}
	}
	let ty_size = roundup(off, ty_align);

	return Type::new(Ty::STRUCT(members), None, None, ty_size ,ty_align , 0, 0);
}

pub fn type_exist(name: &str) -> bool {
	if let Some(_) = ENV.lock().unwrap().typedefs.get(name) {
		return true;
	} else {
		return false;
	}
}

fn primary(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	
	if tokens[*pos].consume_ty(TokenRightBrac, pos) {
		if tokens[*pos].consume_ty(TokenRightCurlyBrace, pos) {
			*pos -= 1;
			let body = Node::new_stmtexpr(INT_TY.clone(), compound_stmt(tokens, pos));
			tokens[*pos].assert_ty(TokenLeftBrac, pos);
			return body;
		}
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

	loop {
		// struct member
		if tokens[*pos].consume_ty(TokenDot, pos) {
			let name = ident(tokens, pos);
			lhs = Node::new_dot(NULL_TY.clone(), lhs, name, 0);
			continue;
		}
		// struct member arrow
		if tokens[*pos].consume_ty(TokenArrow, pos) {
			let name = ident(tokens, pos);
			let expr = Node::new_deref(INT_TY.clone(), lhs);
			lhs = Node::new_dot(NULL_TY.clone(), expr, name, 0);
			continue;
		}
		// array
		if tokens[*pos].consume_ty(TokenRightmiddleBrace, pos)  {
			let id = assign(tokens, pos);
			let lhs2 = Node::new_bit(INT_TY.clone(), TokenAdd, lhs, id);
			lhs = Node::new_deref(INT_TY.clone(), lhs2);
			tokens[*pos].assert_ty(TokenLeftmiddleBrace, pos);
			continue;
		}
		return lhs;
	}
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
	if tokens[*pos].consume_ty(TokenAlignof, pos) {
		return Node::new_alignof(unary(tokens, pos));
	}
	if tokens[*pos].consume_ty(TokenNot, pos) {
		return Node::new_not(unary(tokens, pos));
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
		let ctype = INT_TY.clone();
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
	
	let mut ty = read_type(tokens, pos);

	while tokens[*pos].consume_ty(TokenStar, pos) {
		ty = ty.ptr_to();
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

fn ident(tokens: &Vec<Token>, pos: &mut usize) -> String {
	let name = String::from(&tokens[*pos].input[..tokens[*pos].val as usize]);
	if !tokens[*pos].consume_ty(TokenIdent, pos) {
		panic!("should be identifier at {}", &tokens[*pos].input[*pos..]);
	}
	return name;
}

fn decl(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	// declaration type
	let mut ty = ctype(tokens, pos);

	// identifier
	let name = ident(tokens, pos);

	// array decralation
	ty = read_array(tokens, pos, ty);
	
	if let Ty::VOID = ty.ty {
		panic!("void variable. {}", name);
	}

	if tokens[*pos].consume_ty(TokenEq, pos) {
		let rhs = assign(tokens, pos);
		tokens[*pos].assert_ty(TokenSemi, pos);
		return Node::new_vardef(ty, false, name, 0, Some(rhs));
	} else {
		tokens[*pos].assert_ty(TokenSemi, pos);
		return Node::new_vardef(ty, false, name, 0, None);
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
			let inc = stmt(tokens, pos);
			tokens[*pos].assert_ty(TokenLeftBrac, pos);
			let body = stmt(tokens, pos);
			return Node::new_for(init, cond, inc, body);
		}
		TokenWhile => {
			*pos += 1;
			let init = Node::new_null();
			let inc = Node::new_null();
			tokens[*pos].assert_ty(TokenRightBrac, pos);
			let cond = assign(tokens, pos);
			tokens[*pos].assert_ty(TokenLeftBrac, pos);
			let body = stmt(tokens, pos);
			return Node::new_for(init, cond, inc, body);
		}
		TokenDo => {
			*pos += 1;
			let body = stmt(tokens, pos);
			tokens[*pos].assert_ty(TokenWhile, pos);
			tokens[*pos].assert_ty(TokenRightBrac, pos);
			let cond = assign(tokens, pos);
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
			return decl(tokens, pos);
		}
		TokenSemi => {
			*pos += 1;
			return Node::new_null();
		}
		TokenTypedef => {
			*pos += 1;
			let lhs = decl(tokens, pos);
			if let NodeType::VarDef(ctype, _, name, _, None) = lhs.op {
				ENV.lock().unwrap().typedefs.insert(name, ctype);
				return Node::new_null();
			}
			panic!("typedef error.");
		}
		_ => {
			if tokens[*pos].consume_ty(TokenIdent, pos) {
				*pos -= 1;
				let name = ident(tokens, pos);
				*pos -= 1;
				if type_exist(&name) {
					return decl(tokens, pos);
				}
			}
			return expr(tokens, pos);
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

pub fn param(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	
	// type
	let ty = ctype(tokens, pos);

	// identifier
	let name = ident(tokens, pos);
	return Node::new_vardef(ty, false, name, 0, None);
}

pub fn toplevel(tokens: &Vec<Token>, pos: &mut usize) -> Node {
	
	let mut args = vec![];

	let is_extern = tokens[*pos].consume_ty(TokenExtern, pos);
	
	// Ctype
	let mut ctype = ctype(tokens, pos);

	// identifier
	let name = ident(tokens, pos);
	
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
		return Node::new_func(name, is_extern, args, body, 0);
	}
	
	// global variable
	ctype = read_array(tokens, pos, ctype);
	tokens[*pos].assert_ty(TokenSemi, pos);
	return Node::new_vardef(ctype, is_extern, name, 0, None);

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