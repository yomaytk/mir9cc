use super::parse::{*, NodeType::*};
use super::token::TokenType::*;

use std::collections::HashMap;
use std::sync::Mutex;

lazy_static! {
	pub static ref LVARS: Mutex<HashMap<String, Var>> = Mutex::new(HashMap::new());
	pub static ref STACKSIZE: Mutex<usize> = Mutex::new(0);
	pub static ref STRING: Mutex<Vec<Node>> = Mutex::new(vec![]);
	pub static ref STRLABEL: Mutex<usize> = Mutex::new(0);
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
			if let Some(var) = LVARS.lock().unwrap().get(name) {
				if decay && var.ctype.ty == Ty::ARY {
					return Node::new_addr(var.ctype.ary_of.as_ref().unwrap().as_ref().clone().ptr_of(), Node::new_lvar(var.ctype.clone(), var.offset));
				} else {
					return Node::new_lvar(var.ctype.clone(), var.offset);
				}
			}
			panic!("\"{}\" is not defined.", name);
		}
		EqTree(_, lhs, rhs) => {
			let lhs2 = walk(lhs, false);
			match &lhs2.op {
				NodeType::Lvar(_, _) | NodeType::Deref(_, _) => {}
				_ => { panic!("not an lvalue"); }
			}
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
		Func(name, _, args, body, _) => {
			let mut argv = vec![];
			for arg in args {
				argv.push(walk(arg, true));
			}
			let body = walk(body, true);
			return Node::new_func(name.clone(), STRING.lock().unwrap().clone(), argv, body, 0);
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
			LVARS.lock().unwrap().insert(
				name.clone(),
				Var::new(ctype.clone(), offset),
			);
			return Node::new_vardef(ctype.clone(), name.clone(), offset, rexpr)
		}
		Deref(_, lhs) => { 
			let lhs2 = walk(lhs, true);
			if lhs2.hasctype() && lhs2.nodesctype().ty == Ty::PTR {
				return Node::new_deref(lhs2.nodesctype().ptr_of.as_ref().unwrap().as_ref().clone(), lhs2);
			}
			{ panic!("operand must be a pointer."); }
		}
		Addr(_, lhs) => {
			let lhs2 = walk(lhs, true);
			if lhs2.hasctype() {
				return Node::new_addr(lhs2.nodesctype().ptr_of(), lhs2);
			}
			panic!("Address values ​​other than variables cannot be calculated.");
		}
		Sizeof(_, _, lhs) => {
			let lhs2 = walk(lhs, false);
			if lhs2.hasctype() {
				let val = lhs2.nodesctype().size_of();
				return Node::new_num(val as i32);
			}
			panic!("The size of an untyped value cannot be calculated.");
		}
		Str(ctype, strname, _) => {
			*STRLABEL.lock().unwrap() += 1;
			let label = *STRLABEL.lock().unwrap();
			STRING.lock().unwrap().push(Node::new_string(ctype.clone(), strname.clone(), label));
			let lhs = Node::new_gvar(ctype.clone(), label);
			return walk(&lhs, decay);
		}
		Gvar(ctype, labelname) => {
			if decay && ctype.ty == Ty::ARY {
				return Node::new_addr(ctype.ary_of.as_ref().unwrap().as_ref().clone().ptr_of(), Node::new_gvar(ctype.clone(), *labelname));
			}
			return Node::new_gvar(ctype.clone(), *labelname);
		}
		_ => { panic!("sema error at: {:?}", node); }
	}
}

pub fn sema(nodes: &Vec<Node>) -> Vec<Node> {
	
	let mut funcv = vec![];
	
	for funode in nodes {
		let node;
		(*STRING.lock().unwrap()).clear();
		match walk(funode, true).op {
			Func(name, strgvar, args, body, _) => { node = Node::new_func(name.clone(), strgvar, args, *body, *STACKSIZE.lock().unwrap()); }
			_ => { panic!("funode should be NodeType::Func. "); }
		}
		funcv.push(node);
	}

	return funcv;
}