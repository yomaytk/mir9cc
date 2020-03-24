use super::parse::{*, NodeType::*};

use std::collections::HashMap;
use std::sync::Mutex;

lazy_static! {
	pub static ref VARS: Mutex<HashMap<String, Var>> = Mutex::new(HashMap::new());
	pub static ref STACKSIZE: Mutex<usize> = Mutex::new(0);
}

pub struct Var {
	typeis: Type,
	offset: usize,
}

impl Var {
	pub fn new(typeis: Type, offset: usize) -> Self {
		Self {
			typeis,
			offset,
		}
	}
}

pub fn walk(node: &Node) -> Node {
	match &node.op {
		Num(val) => { return Node::new_num(*val); }
		BinaryTree(ty, lhs, rhs)  => {
			return Node::new_bit(ty.clone(),
				walk(lhs.as_ref().unwrap()), 
				walk(rhs.as_ref().unwrap())
			);
		}
		Ret(lhs) => { return Node::new_ret(walk(lhs)); }
		Expr(lhs) => { return Node::new_expr(walk(lhs)); }
		CompStmt(lhsv) => {
			let mut v = vec![];
			for lhs in lhsv {
				v.push(walk(lhs));
			}
			return Node::new_stmt(v);
		}
		Ident(name) => {
			if let Some(var) = VARS.lock().unwrap().get(name) {
				return Node::new_lvar(var.typeis.clone(), var.offset);
			}
			panic!("\"{}\" is not defined.", name);
		}
		EqTree(lhs, rhs) => { return Node::new_eq(walk(lhs), walk(rhs)); }
		IfThen(cond, then, elthen) => {
			match elthen {
				Some(elth) => { 
					return Node::new_if(walk(cond), walk(then), Some(walk(elth)));
				}
				_ => { return Node::new_if(walk(cond), walk(then), None); }
			}
		}
		Call(name, args) => {
			let mut v = vec![];
			for arg in args {
				v.push(walk(arg));
			}
			return Node::new_call(name.clone(), v);
		}
		Func(name, args, body, _) => {
			let mut argv = vec![];
			for arg in args {
				argv.push(walk(arg));
			}
			return Node::new_func(name.clone(), argv, walk(body), 0);
		}
		LogAnd(lhs, rhs) => { return Node::new_and(walk(lhs), walk(rhs)); }
		LogOr(lhs, rhs) => { return Node::new_or(walk(lhs), walk(rhs)); }
		For(init, cond, inc, body) => {
			return Node::new_for(walk(init), walk(cond), walk(inc), walk(body));
		}
		VarDef(typeis, name, _, init) => {
			let mut rexpr = None;
			if let Some(rhs) = init {
				rexpr = Some(walk(rhs));
			}
			*STACKSIZE.lock().unwrap() += 8;
			let offset = *STACKSIZE.lock().unwrap();
			VARS.lock().unwrap().insert(
				name.clone(),
				Var::new(typeis.clone(), offset),
			);
			return Node::new_vardef(typeis.clone(), name.clone(), offset, rexpr)
		}
		Deref(lhs) => { return Node::new_deref(walk(lhs)); }
		_ => { panic!("sema error at: {:?}", node); }
	}
}

pub fn sema(nodes: &Vec<Node>) -> Vec<Node> {
	
	let mut funcv = vec![];
	for funode in nodes {
		let node;
		match walk(funode).op {
			Func(name, args, body, _) => { node = Node::new_func(name.clone(), args, *body, *STACKSIZE.lock().unwrap()); }
			_ => { panic!("funode should be NodeType::Func. "); }
		}
		funcv.push(node);
	}

	return funcv;
}