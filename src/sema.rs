use super::parse::{*, NodeType::*, INT_TY};
use super::token::TokenType::*;
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
	pub fn new(ctype: Type, offset: usize, is_local: bool, ident: String, strname: String, is_extern: bool) -> Self {
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
	match &node.op {
		Lvar(ctype, ..) | Gvar(ctype, ..) | Dot(ctype, ..) => {
			match ctype.ty {
				Ty::ARY if decay => {
					return Node::new_addr(ctype.ary_to.as_ref().unwrap().as_ref().clone().ptr_to(), node);
				}
				_ => { return node; }
			}
		}
		_ => { panic!("maybe_decay type error"); }
	}
}

pub fn new_global(ctype: &Type, ident: String, strname: Option<String>, is_extern: bool) -> Var {
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
		is_extern,
	);
	return var;
}

pub fn binwalk(f: fn(Type, lhs: Node, rhs: Node) -> Node, lhs: &Node, rhs: &Node, env: &mut Env, lhs_or_rhs: i32) -> Node {
	let lhs2 = walk(lhs, env, true);
	let rhs2 = walk(rhs, env, true);
	if lhs_or_rhs == -1 {
		return f(lhs2.nodesctype(Some(INT_TY.clone())), lhs2, rhs2);
	} else {
		return f(rhs2.nodesctype(Some(INT_TY.clone())), lhs2, rhs2);
	}
}

pub fn walk(node: &Node, env: &mut Env, decay: bool) -> Node {
	match &node.op {
		Num(val) => { return Node::new_num(*val); }
		BinaryTree(_ctype, op, lhs, rhs)  => {
			let lhs2 = walk(lhs, env, true);
			let rhs2 = walk(rhs, env, true);
			let mut ctype = INT_TY.clone();
			ctype = lhs2.nodesctype(Some(ctype));
			match op {
				TokenAdd | TokenSub => {
					match (lhs2.nodesctype(None).ty, rhs2.nodesctype(None).ty) {
						(Ty::NULL, Ty::PTR) => { ctype = rhs2.nodesctype(None); }
						(Ty::PTR, Ty::PTR) => { panic!("pointer +- pointer is not defined."); }
						_ => {}
					}
				}
				_ => {}
			}
			return Node::new_bit(ctype, op.clone(), lhs2, rhs2);
		}
		Ret(lhs) => { return Node::new_ret(walk(lhs, env, true)); }
		Expr(lhs) => { return Node::new_expr(walk(lhs, env, true)); }
		CompStmt(lhsv) => {
			let mut v = vec![];
			let mut newenv = Env::new(Some(env.clone()));
			for lhs in lhsv {
				v.push(walk(lhs, &mut newenv, true));
			}
			return Node::new_stmt(v);
		}
		StmtExpr(ctype, body) => {
			return Node::new_stmtexpr(ctype.clone(), walk(body, env, true));
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
			panic!("\"{}\" is not defined.", name);
		}
		EqTree(_, lhs, rhs) => {
			let lhs2 = walk(lhs, env, false);
			lhs2.checklval();
			let rhs2 = walk(rhs, env, true);
			return Node::new_eq(lhs2.nodesctype(None).clone(), lhs2, rhs2);
		}
		IfThen(cond, then, elthen) => {
			match elthen {
				Some(elth) => { 
					return Node::new_if(walk(cond, env, true), walk(then, env, true), Some(walk(elth, env, true)));
				}
				_ => { return Node::new_if(walk(cond, env, true), walk(then, env, true), None); }
			}
		}
		Call(name, args) => {
			let mut v = vec![];
			for arg in args {
				v.push(walk(arg, env, true));
			}
			return Node::new_call(name.clone(), v);
		}
		Func(name, is_extern, args, body, _) => {
			let mut argv = vec![];
			for arg in args {
				argv.push(walk(arg, env, true));
			}
			let body = walk(body, env, true);
			return Node::new_func(name.clone(), *is_extern, argv, body, 0);
		}
		LogAnd(lhs, rhs) => { return Node::new_and(walk(lhs, env, true), walk(rhs, env, true)); }
		LogOr(lhs, rhs) => { return Node::new_or(walk(lhs, env, true), walk(rhs, env, true)); }
		For(init, cond, inc, body) => {
			return Node::new_for(walk(init, env,  true), walk(cond, env, true), walk(inc, env, true), walk(body, env, true));
		}
		VarDef(ctype, is_extern, ident, _, init) => {
			let mut rexpr = None;
			if let Some(rhs) = init {
				rexpr = Some(walk(rhs, env, true));
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
					*is_extern,
				),
			);
			return Node::new_vardef(ctype.clone(), *is_extern, ident.clone(), offset, rexpr)
		}
		Deref(_, lhs) => {
			let lhs2 = walk(lhs, env, true);
			let ctype = lhs2.nodesctype(None);
			match ctype.ty {
				Ty::PTR => { 
					if let Ty::VOID = ctype.ptr_to.as_ref().unwrap().as_ref().ty {
						panic!("cannot dereference void pointer.");
					}
					return Node::new_deref(ctype.ptr_to.as_ref().unwrap().as_ref().clone(), lhs2); 
				}
				_ => { panic!("operand must be a pointer."); }
			}
		}
		Addr(_, lhs) => {
			let lhs2 = walk(lhs, env, true);
			lhs2.checklval();
			return Node::new_addr(lhs2.nodesctype(None).ptr_to(), lhs2);
		}
		Sizeof(.., lhs) => {
			let lhs2 = walk(lhs, env, false);
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
			*STRLABEL.lock().unwrap() += 1;
			let labelname = format!(".L.str{}", *STRLABEL.lock().unwrap());
			GVARS.lock().unwrap().push(new_global(&ctype, labelname.clone(), Some(strname.clone()), false));
			let lhs = Node::new_gvar(ctype.clone(), labelname);
			return maybe_decay(lhs, decay);
		}
		EqEq(lhs, rhs) => {
			return Node::new_eqeq(walk(lhs, env, true), walk(rhs, env, true));
		}
		Ne(lhs, rhs) => {
			return Node::new_neq(walk(lhs, env, true), walk(rhs, env, true));
		}
		DoWhile(body, cond) => {
			return Node::new_dowhile(walk(body, env, true), walk(cond, env, true));
		}
		Alignof(expr) => {
			let expr2 = walk(expr, env, false);
			let ctype = expr2.nodesctype(None);
			match ctype.ty {
				Ty::NULL => { panic!("_Alignof should be used for Node has Ctype."); }
				_ => { return Node::new_num(ctype.align as i32); }
			}
		}
		Dot(_, expr, name, _) => {
			let expr2 = walk(expr, env, true);
			match expr2.nodesctype(None).ty {
				Ty::STRUCT(members) => {
					if members.is_empty() {
						panic!("incomplete type.");
					}
					for membernode in members {
						if let NodeType::VarDef(ctype, _, name2, ..) = membernode.op {
							if &name[..] != &name2[..] {
								continue;
							}
							let lhs = Node::new_dot(ctype.clone(), expr2, name2, ctype.offset);
							return maybe_decay(lhs, decay);
						}
					}
					panic!("member missing.");
				}
				_ => { panic!("struct expected before ."); }
			}
		}
		Not(expr) => {
			let expr2 = walk(expr, env, true);
			return Node::new_not(expr2);
		}
		Ternary(_, cond, then, els) => {
			let cond2 = walk(cond, env, true);
			let then2 = walk(then, env, true);
			let els2 = walk(els, env, true);
			return Node::new_ternary(then2.nodesctype(Some(INT_TY.clone())), cond2, then2, els2);
		}
		TupleExpr(_, lhs, rhs) => {
			return binwalk(Node::new_tuple, lhs, rhs, env, 1);
		}
		BitOr(_, lhs, rhs) => {
			return binwalk(Node::new_bitor, lhs, rhs, env, -1);
		}
		BitXor(_, lhs, rhs) => {
			return binwalk(Node::new_bitxor, lhs, rhs, env, -1);
		}
		BitAnd(_, lhs, rhs) => {
			return binwalk(Node::new_bitand, lhs, rhs, env, -1);
		}
		Neg(expr) => {
			return Node::new_neg(walk(expr, env, true));
		}
		IncDec(_, selector, expr) => {
			let lhs = walk(expr, env, true);
			lhs.checklval();
			return Node::new_incdec(lhs.nodesctype(None), *selector, lhs);
		}
		NULL => {
			return Node::new_null();
		}
		_ => { panic!("sema error at: {:?}", node); }
	}
}

pub fn sema(nodes: &Vec<Node>) -> (Vec<Node>, Vec<Var>) {
	
	let mut funcv = vec![];
	let mut topenv = Env::new(None);
	
	for topnode in nodes {
		let node;

		if let VarDef(ctype, is_extern, ident, ..) = &topnode.op {
			let var = new_global(&ctype, ident.clone(), None, *is_extern);
			GVARS.lock().unwrap().push(var.clone());
			topenv.vars.insert(ident.clone(), var);
			continue;
		}

		match walk(topnode, &mut topenv, true).op {
			Func(name, is_extern, args, body, _) => { 
				node = Node::new_func(name.clone(), is_extern, args, *body, *STACKSIZE.lock().unwrap());
			}
			NULL => { continue; }
			_ => { panic!("funode should be NodeType::Func. "); }
		}
		funcv.push(node);
	}

	return (funcv, GVARS.lock().unwrap().clone());
}