use super::parse::{*, NodeType::*};
use super::token::TokenType::*;

use std::collections::HashMap;
use std::sync::Mutex;

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
	pub label: usize,
	pub ident: String,
	pub strname: String,
	pub is_extern: bool,
	pub is_string_decl: bool,
}

impl Var {
	pub fn new(ctype: Type, offset: usize, is_local: bool, label: usize, ident: String, strname: String, is_extern: bool, is_string_decl: bool) -> Self {
		Self {
			ctype,
			offset,
			is_local,
			label,
			ident,
			strname,
			is_extern,
			is_string_decl,
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
	match &node.op {
		Lvar(ctype, _) | Gvar(ctype, _) => {
			if decay && ctype.ty == Ty::ARY {
				return Node::new_addr(ctype.ary_of.as_ref().unwrap().as_ref().clone().ptr_of(), node);
			}
			return node;
		}
		_ => { panic!("maybe_decay type error"); }
	}
}

pub fn new_global(ctype: &Type, ident: String, strname: Option<String>, is_extern: bool, is_string_decl: bool) -> Var {
	*STRLABEL.lock().unwrap() += 1;
	let label = *STRLABEL.lock().unwrap();
	let mut strdata = String::new();
	if let Some(data) = strname {
		strdata = data.clone()
	}
	let var = Var::new(
		ctype.clone(), 
		0, 
		false, 
		label, 
		ident,
		strdata,
		is_extern,
		is_string_decl,
	);
	return var;
}

pub fn walk(env: &mut Env, node: &Node, decay: bool) -> Node {
	match &node.op {
		Num(val) => { return Node::new_num(*val); }
		BinaryTree(_ctype, op, lhs, rhs)  => {
			let lhs2 = walk(env, lhs.as_ref().unwrap(), true);
			let rhs2 = walk(env, rhs.as_ref().unwrap(), true);
			let mut ctype = Type::new(Ty::INT, None, None, 0);
			if lhs2.hasctype(){
				ctype = lhs2.nodesctype();
			}
			match op {
				TokenAdd | TokenSub => {
					if rhs2.hasctype() && rhs2.nodesctype().ty == Ty::PTR {
						if lhs2.hasctype() && lhs2.nodesctype().ty == Ty::PTR {
							panic!("pointer +- pointer is not defind.");
						}
						ctype = rhs2.nodesctype();
					}
				}
				_ => {}
			}
			return Node::new_bit(ctype, op.clone(), lhs2, rhs2);
		}
		Ret(lhs) => { return Node::new_ret(walk(env, lhs, true)); }
		Expr(lhs) => { return Node::new_expr(walk(env, lhs, true)); }
		CompStmt(lhsv) => {
			let mut v = vec![];
			let mut newenv = Env::new(Some(env.clone()));
			for lhs in lhsv {
				v.push(walk(&mut newenv, lhs, true));
			}
			return Node::new_stmt(v);
		}
		StmtExpr(ctype, body) => {
			return Node::new_stmtexpr(ctype.clone(), walk(env, body, true));
		}
		Ident(name) => {
			if let Some(var) = env.find(name.clone()) {
				if var.is_local {
					let lvar = Node::new_lvar(var.ctype.clone(), var.offset);
					return maybe_decay(lvar, decay);
				} else if var.is_string_decl {
					let gvar = Node::new_gvar(var.ctype.clone(), format!(".L.str{}", var.label));
					return maybe_decay(gvar, decay);
				} else {
					let gvar = Node::new_gvar(var.ctype.clone(), var.ident.clone());
					return maybe_decay(gvar, decay);
				}
			}
			panic!("\"{}\" is not defined.", name);
		}
		EqTree(_, lhs, rhs) => {
			let lhs2 = walk(env, lhs, false);
			lhs2.checklval();
			let rhs2 = walk(env, rhs, true);
			return Node::new_eq(lhs2.nodesctype().clone(), lhs2, rhs2);
		}
		IfThen(cond, then, elthen) => {
			match elthen {
				Some(elth) => { 
					return Node::new_if(walk(env, cond, true), walk(env, then, true), Some(walk(env, elth, true)));
				}
				_ => { return Node::new_if(walk(env, cond, true), walk(env, then, true), None); }
			}
		}
		Call(name, args) => {
			let mut v = vec![];
			for arg in args {
				v.push(walk(env, arg, true));
			}
			return Node::new_call(name.clone(), v);
		}
		Func(name, is_extern, args, body, _) => {
			let mut argv = vec![];
			for arg in args {
				argv.push(walk(env, arg, true));
			}
			let body = walk(env, body, true);
			return Node::new_func(name.clone(), *is_extern, argv, body, 0);
		}
		LogAnd(lhs, rhs) => { return Node::new_and(walk(env, lhs, true), walk(env, rhs, true)); }
		LogOr(lhs, rhs) => { return Node::new_or(walk(env, lhs, true), walk(env, rhs, true)); }
		For(init, cond, inc, body) => {
			return Node::new_for(walk(env, init, true), walk(env, cond, true), walk(env, inc, true), walk(env, body, true));
		}
		VarDef(ctype, is_extern, ident, _, init) => {
			let mut rexpr = None;
			if let Some(rhs) = init {
				rexpr = Some(walk(env, rhs, true));
			}
			*STACKSIZE.lock().unwrap() += ctype.size_of();
			let offset = *STACKSIZE.lock().unwrap();
			env.vars.insert(
				ident.clone(),
				Var::new(
					ctype.clone(), 
					offset, 
					true, 
					0, 
					ident.clone(),
					String::from("dummy"),
					*is_extern,
					false,
				),
			);
			return Node::new_vardef(ctype.clone(), *is_extern, ident.clone(), offset, rexpr)
		}
		Deref(_, lhs) => {
			let lhs2 = walk(env, lhs, true);
			if lhs2.hasctype() && lhs2.nodesctype().ty == Ty::PTR {
				return Node::new_deref(lhs2.nodesctype().ptr_of.as_ref().unwrap().as_ref().clone(), lhs2);
			}
			{ panic!("operand must be a pointer."); }
		}
		Addr(_, lhs) => {
			let lhs2 = walk(env, lhs, true);
			lhs2.checklval();
			return Node::new_addr(lhs2.nodesctype().ptr_of(), lhs2);
		}
		Sizeof(_, _, lhs) => {
			let lhs2 = walk(env, lhs, false);
			if lhs2.hasctype() {
				let val = lhs2.nodesctype().size_of();
				return Node::new_num(val as i32);
			}
			panic!("The size of an untyped value cannot be calculated.");
		}
		Str(ctype, strname, _) => {
			GVARS.lock().unwrap().push(new_global(&ctype, String::new(), Some(strname.clone()), false, true));
			let lhs = Node::new_gvar(ctype.clone(), format!(".L.str{}", *STRLABEL.lock().unwrap()));
			return maybe_decay(lhs, decay);
		}
		EqEq(lhs, rhs) => {
			return Node::new_eqeq(walk(env, lhs, true), walk(env, rhs, true));
		}
		Ne(lhs, rhs) => {
			return Node::new_neq(walk(env, lhs, true), walk(env, rhs, true));
		}
		DoWhile(body, cond) => {
			return Node::new_dowhile(walk(env, body, true), walk(env, cond, true));
		}
		_ => { panic!("sema error at: {:?}", node); }
	}
}

pub fn sema(nodes: &Vec<Node>) -> (Vec<Node>, Vec<Var>) {
	
	let mut funcv = vec![];
	let mut topenv = Env::new(None);
	
	for topnode in nodes {
		let node;

		if let VarDef(ctype, is_extern, ident, _, _) = &topnode.op {
			let var = new_global(&ctype, ident.clone(), None, *is_extern, false);
			GVARS.lock().unwrap().push(var.clone());
			topenv.vars.insert(ident.clone(), var);
			continue;
		}

		match walk(&mut topenv, topnode, true).op {
			Func(name, is_extern, args, body, _) => { 
				node = Node::new_func(name.clone(), is_extern, args, *body, *STACKSIZE.lock().unwrap());
			}
			_ => { panic!("funode should be NodeType::Func. "); }
		}
		funcv.push(node);
	}

	return (funcv, GVARS.lock().unwrap().clone());
}