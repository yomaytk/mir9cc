use super::parse::{*, NodeType::*};
use super::token::TokenType::*;

use std::collections::HashMap;
use std::sync::Mutex;

lazy_static! {
	pub static ref VARS: Mutex<HashMap<String, Var>> = Mutex::new(HashMap::new());
	pub static ref STACKSIZE: Mutex<usize> = Mutex::new(0);
}

pub struct Var {
	ctype: Type,
	offset: usize,
}

impl Var {
	pub fn new(ctype: Type, offset: usize) -> Self {
		Self {
			ctype,
			offset,
		}
	}
}

pub fn walk(node: &Node, decay: bool) -> Node {
	match &node.op {
		Num(val) => { return Node::new_num(*val); }
		BinaryTree(_ctype, op, lhs, rhs)  => {
			let lhs2 = walk(lhs.as_ref().unwrap(), true);
			let rhs2 = walk(rhs.as_ref().unwrap(), true);
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
		Ret(lhs) => { return Node::new_ret(walk(lhs, true)); }
		Expr(lhs) => { return Node::new_expr(walk(lhs, true)); }
		CompStmt(lhsv) => {
			let mut v = vec![];
			for lhs in lhsv {
				v.push(walk(lhs, true));
			}
			return Node::new_stmt(v);
		}
		Ident(name) => {
			if let Some(var) = VARS.lock().unwrap().get(name) {
				if decay && var.ctype.ty == Ty::ARY {
					return Node::addr_of(Node::new_lvar(var.ctype.clone(), var.offset), var.ctype.ary_of.as_ref().unwrap().as_ref().clone());
				} else {
					return Node::new_lvar(var.ctype.clone(), var.offset);
				}
			}
			panic!("\"{}\" is not defined.", name);
		}
		EqTree(_, lhs, rhs) => {
			let lhs2 = walk(lhs, false);
			let rhs2 = walk(rhs, true);
			match &lhs2.op {
				NodeType::VarDef(cty, _, _, _) | NodeType::Lvar(cty, _) | NodeType::Deref(cty, _) => {
					return Node::new_eq(cty.clone(), lhs2, rhs2);
				}
				_ => { panic!("The left side of = must be an lvalue."); }
			}
		}
		IfThen(cond, then, elthen) => {
			match elthen {
				Some(elth) => { 
					return Node::new_if(walk(cond, true), walk(then, true), Some(walk(elth, true)));
				}
				_ => { return Node::new_if(walk(cond, true), walk(then, true), None); }
			}
		}
		Call(name, args) => {
			let mut v = vec![];
			for arg in args {
				v.push(walk(arg, true));
			}
			return Node::new_call(name.clone(), v);
		}
		Func(name, args, body, _) => {
			let mut argv = vec![];
			for arg in args {
				argv.push(walk(arg, true));
			}
			return Node::new_func(name.clone(), argv, walk(body, true), 0);
		}
		LogAnd(lhs, rhs) => { return Node::new_and(walk(lhs, true), walk(rhs, true)); }
		LogOr(lhs, rhs) => { return Node::new_or(walk(lhs, true), walk(rhs, true)); }
		For(init, cond, inc, body) => {
			return Node::new_for(walk(init, true), walk(cond, true), walk(inc, true), walk(body, true));
		}
		VarDef(ctype, name, _, init) => {
			let mut rexpr = None;
			if let Some(rhs) = init {
				rexpr = Some(walk(rhs, true));
			}
			*STACKSIZE.lock().unwrap() += ctype.size_of();
			let offset = *STACKSIZE.lock().unwrap();
			VARS.lock().unwrap().insert(
				name.clone(),
				Var::new(ctype.clone(), offset),
			);
			return Node::new_vardef(ctype.clone(), name.clone(), offset, rexpr)
		}
		Deref(_, lhs) => { 
			let lhs2 = walk(lhs, true);
			let lhs2cty = lhs2.nodesctype();
			if lhs2.hasctype() && lhs2cty.ty == Ty::PTR {
				return Node::new_deref(lhs2cty.ptr_of.as_ref().unwrap().as_ref().clone(), lhs2);
			}
			{ panic!("operand must be a pointer."); }
		}
		_ => { panic!("sema error at: {:?}", node); }
	}
}

pub fn sema(nodes: &Vec<Node>) -> Vec<Node> {
	
	let mut funcv = vec![];
	for funode in nodes {
		let node;
		match walk(funode, true).op {
			Func(name, args, body, _) => { node = Node::new_func(name.clone(), args, *body, *STACKSIZE.lock().unwrap()); }
			_ => { panic!("funode should be NodeType::Func. "); }
		}
		funcv.push(node);
	}

	return funcv;
}