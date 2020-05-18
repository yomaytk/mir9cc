use super::parse::{*, NodeType::*, INT_TY};
use super::token::TokenType::*;
// use super::lib::*;
use super::mir::*;

use std::collections::HashMap;
use std::sync::Mutex;

// Semantics analyzer. This pass plays a few important roles as shown
// below:
//
// - Add types to nodes. For example, a tree that represents "1+2" is
//   typed as INT because the result type of an addition of two
//   integers is integer.
//
// - Resolve variable names based on the C scope rules.
//   Local variables are resolved to offsets from the base pointer.
//   Global variables are resolved to their names.
//
// - Insert nodes to make array-to-pointer conversion explicit.
//   Recall that, in C, "array of T" is automatically converted to
//   "pointer to T" in most contexts.
//
// - Reject bad assignments, such as `1=2+3`.

lazy_static! {
	pub static ref STACKSIZE: Mutex<usize> = Mutex::new(0);
	pub static ref GVARS: Mutex<Vec<Var>> = Mutex::new(vec![]);
	pub static ref STRLABEL: Mutex<usize> = Mutex::new(0);
}

#[derive(Debug, Clone)]
pub struct Var {
	pub ctype: Type,
	pub offset: usize,
	pub is_local: bool,
	pub ident: String,
	pub strname: String,
	pub is_extern: bool,
}

impl Var {
	pub fn new(ctype: Type, offset: usize, is_local: bool, ident: String, strname: String) -> Self {
		let is_extern = ctype.is_extern;
		Self {
			ctype,
			offset,
			is_local,
			ident,
			strname,
			is_extern,
		}
	}
}

#[derive(Debug, Clone)]
pub struct Env {
	pub vars: HashMap<String, Var>,
	pub next: Option<Box<Env>>,
}

impl Env {
	pub fn new(next: Option<Env>) -> Self {
		match next {
			Some(nextenv) => {
				Self {
					vars: HashMap::new(),
					next: Some(Box::new(nextenv)),
				}
			}
			_ => {
				Self {
					vars: HashMap::new(),
					next: None,
				}
			}
		}
	}
	pub fn find(&self, name: String) -> Option<&Var> {

		if let Some(var) = self.vars.get(&name) {
			return Some(var);
		}

		let mut env = &self.next;

		while let Some(e) = env {
			if let Some(var) = e.vars.get(&name) {
				return Some(var);
			}
			env = &e.next;
		}
		return None;
	}
}

pub fn maybe_decay(node: Node, decay: bool) -> Node {
	let ctype = node.nodesctype(None);
	match node.nodesctype(None).ty {
		Ty::ARY if decay => {
			return Node::new_addr(ctype.ary_to.as_ref().unwrap().as_ref().clone().ptr_to(), node);
		}
		Ty::NULL => {
			panic!("maybe_decay type error {:#?}", node);
		}
		_ => { return node; }
	}
}

pub fn new_global(ctype: &Type, ident: String, strname: Option<String>) -> Var {
	let mut strdata = String::new();
	if let Some(data) = strname {
		strdata = data.clone()
	}
	let var = Var::new(
		ctype.clone(), 
		0, 
		false, 
		ident,
		strdata,
	);
	return var;
}

pub fn binwalk(f: fn(Type, lhs: Node, rhs: Node) -> Node, lhs: &Node, rhs: &Node, env: &mut Env, lhs_or_rhs: i32) -> Node {
	let lhs2 = walk(lhs, env);
	let rhs2 = walk(rhs, env);
	if lhs_or_rhs == -1 {
		return f(lhs2.nodesctype(Some(INT_TY.clone())), lhs2, rhs2);
	} else {
		return f(rhs2.nodesctype(Some(INT_TY.clone())), lhs2, rhs2);
	}
}

pub fn walk(node: &Node, env: &mut Env) -> Node {
	return do_walk(node, env, true)
}

pub fn walk_noconv(node: &Node, env: &mut Env) -> Node {
	return do_walk(node, env, false)
}

fn bin_ptr_swap(ctype: &mut Type, mut lhs: Node, mut rhs: Node) -> (Node, Node) {
	match (lhs.nodesctype(None).ty, rhs.nodesctype(None).ty) {
		(Ty::PTR, _) => {}
		(_, Ty::PTR) => {
			*ctype = rhs.nodesctype(None);
			lhs = std::mem::replace(&mut rhs, lhs);
		}
		_ => { 
			return (lhs, rhs); 
		}
	}
	let scale_ptr = ctype.ptr_to.as_ref().unwrap().size as i32;
	rhs = Node::new_bit(INT_TY.clone(), TokenStar, rhs, Node::new_num(scale_ptr));
	return (lhs, rhs);
}

fn same_type(ty1: Type, ty2: Type) -> bool {
	if ty1.ty != ty2.ty {
		return false;
	}
	match ty1.ty {
		Ty::PTR => {
			return same_type(*ty1.ptr_to.unwrap(), *ty2.ptr_to.unwrap());
		}
		Ty::ARY => {
			return ty1.size == ty2.size && same_type(*ty1.ary_to.unwrap(), *ty2.ary_to.unwrap());
		}
		_ => {
			return true;
		}
	}
}

pub fn do_walk(node: &Node, env: &mut Env, decay: bool) -> Node {
	match &node.op {
		Num(val) => { return Node::new_num(*val); }
		BinaryTree(_, op, lhs, rhs)  => {
			let mut lhs2 = walk(lhs, env);
			let mut rhs2 = walk(rhs, env);
			let mut ctype = INT_TY.clone();
			ctype = lhs2.nodesctype(Some(ctype));
			if let TokenAdd = op {
				let (lhs2_, rhs2_) = bin_ptr_swap(&mut ctype, lhs2, rhs2);
				lhs2 = lhs2_;
				rhs2 = rhs2_;
				if let Ty::PTR = rhs2.nodesctype(None).ty {
					panic!("pointer + pointer is not defined.");
				}
			} else if let TokenSub = op {
				match (lhs2.nodesctype(None).ty, rhs2.nodesctype(None).ty) {
					(Ty::PTR, Ty::PTR) => {
						if !same_type(lhs2.nodesctype(None).clone(), rhs2.nodesctype(None).clone()) {
							panic!("both type of operand of ptr - ptr should be same")
						}
						let node = Node::new_bit(ctype.clone(), TokenSub, lhs2, rhs2);
						let scale_ptr = ctype.ptr_to.as_ref().unwrap().size as i32;
						return Node::new_bit(ctype, TokenDiv, node, Node::new_num(scale_ptr));
					}
					_ => {
						let (lhs2_, rhs2_) = bin_ptr_swap(&mut ctype, lhs2, rhs2);
						lhs2 = lhs2_;
						rhs2 = rhs2_;
					}
				}
			}
			return Node::new_bit(ctype, op.clone(), lhs2, rhs2);
		}
		Ret(lhs) => { return Node::new_ret(walk(lhs, env)); }
		Expr(lhs) => { return Node::new_expr(walk(lhs, env)); }
		CompStmt(lhsv) => {
			let mut v = vec![];
			let mut newenv = Env::new(Some(env.clone()));
			for lhs in lhsv {
				v.push(walk(lhs, &mut newenv));
			}
			return Node::new_stmt(v);
		}
		StmtExpr(ctype, body) => {
			return Node::new_stmtexpr(ctype.clone(), walk(body, env));
		}
		Ident(name) => {
			if let Some(var) = env.find(name.clone()) {
				if let Ty::NULL = var.ctype.ty {
					panic!("type of {} is invalid.", var.ident);
				}
				if var.is_local {
					let lvar = Node::new_lvar(var.ctype.clone(), var.offset);
					return maybe_decay(lvar, decay);
				} else {
					let gvar = Node::new_gvar(var.ctype.clone(), var.ident.clone());
					return maybe_decay(gvar, decay);
				}
			}
			// error(&format!("\"{}\" is not defined.", name));
			// for debug.
			panic!("\"{}\" is not defined.", name);
		}
		EqTree(_, lhs, rhs) => {
			let lhs2 = walk_noconv(lhs, env);
			lhs2.checklval();
			let rhs2 = walk(rhs, env);
			return Node::new_eq(lhs2.nodesctype(None).clone(), lhs2, rhs2);
		}
		IfThen(cond, then, elthen) => {
			match elthen {
				Some(elth) => { 
					return Node::new_if(walk(cond, env), walk(then, env), Some(walk(elth, env)));
				}
				_ => { return Node::new_if(walk(cond, env), walk(then, env), None); }
			}
		}
		Call(_, name, args) => {
			let ctype;
			if let Some(var) = env.find(name.clone()) {
				ctype = var.ctype.clone();
			} else {
				eprintln!("bad function: {}", name.clone());
				ctype = INT_TY.clone();
			}
			let mut v = vec![];
			for arg in args {
				v.push(walk(arg, env));
			}
			return Node::new_call(ctype, name.clone(), v);
		}
		LogAnd(lhs, rhs) => { return Node::new_and(walk(lhs, env), walk(rhs, env)); }
		LogOr(lhs, rhs) => { return Node::new_or(walk(lhs, env), walk(rhs, env)); }
		For(init, cond, inc, body) => {
			let mut newenv = Env::new(Some(env.clone()));
			return Node::new_for(walk(init, &mut newenv), walk(cond, &mut newenv), walk(inc, &mut newenv), walk(body, &mut newenv));
		}
		VarDef(ctype, ident, _, init) => {
			let mut rexpr = None;
			if let Some(rhs) = init {
				rexpr = Some(walk(rhs, env));
			}
			let stacksize = *STACKSIZE.lock().unwrap();
			*STACKSIZE.lock().unwrap() = roundup(stacksize, ctype.align);
			*STACKSIZE.lock().unwrap() += ctype.size;
			let offset = *STACKSIZE.lock().unwrap();
			env.vars.insert(
				ident.clone(),
				Var::new(
					ctype.clone(), 
					offset, 
					true, 
					ident.clone(),
					String::from("dummy"),
				),
			);
			return Node::new_vardef(ctype.clone(), ident.clone(), offset, rexpr)
		}
		Deref(_, lhs) => {
			let lhs2 = walk(lhs, env);
			let ctype = lhs2.nodesctype(None);
			match ctype.ty {
				Ty::PTR => { 
					if let Ty::VOID = ctype.ptr_to.as_ref().unwrap().as_ref().ty {
						// error("cannot dereference void pointer.");
						// for debug.
						panic!("cannot dereference void pointer.");
					}
					return maybe_decay(Node::new_deref(ctype.ptr_to.as_ref().unwrap().as_ref().clone(), lhs2), decay); 
				}
				_ => {
					// error(&format!("operand must be a pointer."));
					// for debug.
					panic!("operand must be a pointer.");
				}
			}
		}
		Addr(_, lhs) => {
			let lhs2 = walk(lhs, env);
			lhs2.checklval();
			return Node::new_addr(lhs2.nodesctype(None).ptr_to(), lhs2);
		}
		Sizeof(.., lhs) => {
			let lhs2 = walk_noconv(lhs, env);
			let ctype = lhs2.nodesctype(None);
			match ctype.ty {
				Ty::NULL => { panic!("The size of an untyped value cannot be calculated."); }
				_ => {
					let val = ctype.size;
					return Node::new_num(val as i32);
				}
			}
			
		}
		Str(ctype, strname, _) => {
		// A string literal is converted to a reference to an anonymous
		// global variable of type char array.
			*STRLABEL.lock().unwrap() += 1;
			let labelname = format!(".L.str{}", *STRLABEL.lock().unwrap());
			GVARS.lock().unwrap().push(new_global(&ctype, labelname.clone(), Some(strname.clone())));
			let lhs = Node::new_gvar(ctype.clone(), labelname);
			return maybe_decay(lhs, decay);
		}
		EqEq(lhs, rhs) => {
			return Node::new_eqeq(walk(lhs, env), walk(rhs, env));
		}
		Ne(lhs, rhs) => {
			return Node::new_neq(walk(lhs, env), walk(rhs, env));
		}
		DoWhile(body, cond) => {
			return Node::new_dowhile(walk(body, env), walk(cond, env));
		}
		Alignof(expr) => {
			let expr2 = walk_noconv(expr, env);
			let ctype = expr2.nodesctype(None);
			match ctype.ty {
				Ty::NULL => { panic!("_Alignof should be used for Node has Ctype."); }
				_ => { return Node::new_num(ctype.align as i32); }
			}
		}
		Dot(_, expr, name) => {
			let expr2 = walk(expr, env);
			match expr2.nodesctype(None).ty {
				Ty::STRUCT(_, members) => {
					if members.is_empty() {
						// error("incomplete type.");
						// for debug.
						panic!("incomplete type.")
					}
					for membernode in members {
						if let NodeType::VarDef(ctype, name2, ..) = membernode.op {
							if &name[..] != &name2[..] {
								continue;
							}
							let lhs = Node::new_dot(ctype.clone(), expr2, name2);
							return maybe_decay(lhs, decay);
						}
					}
					// error(&format!("member missing."));
					// for debug.
					panic!("member missing.");
				}
				_ => {
					// error(&format!("struct expected before ."));
					// for debug.
					panic!("struct expected before ."); }
			}
		}
		Not(expr) => {
			let expr2 = walk(expr, env);
			return Node::new_not(expr2);
		}
		Ternary(_, cond, then, els) => {
			let cond2 = walk(cond, env);
			let then2 = walk(then, env);
			let els2 = walk(els, env);
			return Node::new_ternary(then2.nodesctype(Some(INT_TY.clone())), cond2, then2, els2);
		}
		TupleExpr(_, lhs, rhs) => {
			return binwalk(Node::new_tuple, lhs, rhs, env, 1);
		}
		Neg(expr) => {
			return Node::new_neg(walk(expr, env));
		}
		IncDec(_, selector, expr) => {
			let lhs = walk(expr, env);
			lhs.checklval();
			return Node::new_incdec(lhs.nodesctype(None), *selector, lhs);
		}
		Break => {
			return Node::new_break();
		}
		NULL => {
			return Node::new_null();
		}
		_ => { panic!("sema error at: {:?}", node); }
	}
}

pub fn sema(program: &mut Program) {
	
	let mut topenv = Env::new(None);
	let mut nodes = vec![];
	
	for topnode in &mut program.nodes {

		if let VarDef(ctype, ident, ..) = &topnode.op {
			let var = new_global(&ctype, ident.clone(), None);
			GVARS.lock().unwrap().push(var.clone());
			topenv.vars.insert(ident.clone(), var);
			continue;
		}

		match &topnode.op {
			Func(ctype, ident, args, body, _) => {
				*STACKSIZE.lock().unwrap() = 0;
				// eval args
				let mut argv = vec![];
				for arg in args {
					argv.push(walk(arg, &mut topenv));
				}
				// eval body
				let body = walk(body, &mut topenv);
				let node = Node::new_func(ctype.clone(), ident.clone(), argv, body, *STACKSIZE.lock().unwrap());
				// add to var env
				let var = Var::new(ctype.clone(), 0, false, ident.clone(), String::new());
				topenv.vars.insert(ident.clone(), var);
				
				nodes.push(node);
			}
			Decl(ctype, ident, _) => {
				// add to global
				let var = Var::new(ctype.clone(), 0, false, ident.clone(), String::new());
				topenv.vars.insert(ident.clone(), var);
			}
			NULL => { continue; }
			_ => { panic!("funode should be NodeType::Func. "); }
		}
	}

	program.gvars = std::mem::replace(&mut GVARS.lock().unwrap(), vec![]);
	program.nodes = nodes;
}