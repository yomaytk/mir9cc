use super::parse::{*, NodeType::*, INT_TY};
use super::token::TokenType::*;
// use super::lib::*;
use super::mir::*;

// Semantics analyzer. This pass plays a few important roles as shown
// below:
//
// - Add types to nodes. For example, a tree that represents "1+2" is
//   typed as INT because the result type of an addition of two
//   integers is integer.
//
// - Insert nodes to make array-to-pointer conversion explicit.
//   Recall that, in C, "array of T" is automatically converted to
//   "pointer to T" in most contexts.
//
// - Reject bad assignments, such as `1=2+3`.

pub fn maybe_decay(node: Node, decay: bool) -> Node {
	let ctype = node.nodesctype(None);
	match ctype.ty {
		Ty::ARY if decay => {
			return Node::new_addr(ctype.ary_to.as_ref().unwrap().as_ref().clone().ptr_to(), node);
		}
		_ => { return node; }
	}
}

pub fn binwalk(f: fn(Type, lhs: Node, rhs: Node) -> Node, lhs: &Node, rhs: &Node, lhs_or_rhs: i32) -> Node {
	let lhs2 = walk(lhs);
	let rhs2 = walk(rhs);
	if lhs_or_rhs == -1 {
		return f(lhs2.nodesctype(Some(INT_TY.clone())), lhs2, rhs2);
	} else {
		return f(rhs2.nodesctype(Some(INT_TY.clone())), lhs2, rhs2);
	}
}

pub fn walk(node: &Node) -> Node {
	return do_walk(node, true)
}

pub fn walk_nodecay(node: &Node) -> Node {
	return do_walk(node, false)
}

fn bin_ptr_swap(ctype: &mut Type, lhs: &mut Node, rhs: &mut Node) {
	match (lhs.nodesctype(None).ty, rhs.nodesctype(None).ty) {
		(Ty::PTR, _) => {}
		(_, Ty::PTR) => {
			*ctype = rhs.nodesctype(None);
			*lhs = std::mem::replace(rhs, Node::new_null());
		}
		_ => { 
			return; 
		}
	}
	let scale_ptr = ctype.ptr_to.as_ref().unwrap().size as i32;
	*rhs = Node::new_bit(INT_TY.clone(), TokenStar, rhs.clone(), Node::new_num(scale_ptr));
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

pub fn get_type(node: &Node) -> Type {
	return walk_nodecay(node).nodesctype(None);
}

fn check_int(node: &Node) {
	let ctype = node.nodesctype(None);
	if ctype.ty != Ty::INT && ctype.ty != Ty::CHAR && ctype.ty != Ty::BOOL {
		panic!("{:?} is not an Integer.", node);
	}
}

pub fn do_walk(node: &Node, decay: bool) -> Node {
	match &node.op {
		Num(val) => { return Node::new_num(*val); }
		BinaryTree(_, op, lhs, rhs)  => {
			let mut lhs2 = walk(lhs);
			let mut rhs2 = walk(rhs);
			let mut ctype = INT_TY.clone();
			ctype = lhs2.nodesctype(Some(ctype));
			match op {
				TokenAdd => {
					bin_ptr_swap(&mut ctype, &mut lhs2, &mut rhs2);
					if let Ty::PTR = rhs2.nodesctype(None).ty {
						panic!("pointer + pointer is not defined.");
					}
					return Node::new_bit(ctype, op.clone(), lhs2, rhs2);
				}
				TokenSub => {
					let lty = lhs2.nodesctype(None);
					let rty = rhs2.nodesctype(None);
					match (&lty.ty, &rty.ty) {
						(Ty::PTR, Ty::PTR) => {
							if !same_type(lty, rty) {
								panic!("both type of operand of ptr - ptr should be same")
							}
							let node = Node::new_bit(ctype.clone(), TokenSub, lhs2, rhs2);
							let scale_ptr = ctype.ptr_to.as_ref().unwrap().size as i32;
							return Node::new_bit(ctype, TokenDiv, node, Node::new_num(scale_ptr));
						}
						_ => {
							bin_ptr_swap(&mut ctype, &mut lhs2, &mut rhs2);
						}
					}
					return Node::new_bit(ctype, op.clone(), lhs2, rhs2);
				}
				_ => {
					check_int(&lhs2);
					check_int(&rhs2);
					return Node::new_bit(ctype, op.clone(), lhs2, rhs2);
				}
			}
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
		StmtExpr(_, body) => {
			let mut ctype = VOID_TY.clone();
			let body = walk(body);
			if let NodeType::CompStmt(stmts) = &body.op {
				if let Some(node) = stmts.last() {
					ctype = node.nodesctype(None);
				}
			} else {
				panic!("body.op of StmtExpr must be CompStmt.");
			}
			return Node::new_stmtexpr(ctype, body);
		}
		VarRef(_) => {
			return maybe_decay(node.clone(), decay);
		}
		Assign(_, lhs, rhs) => {
			if let NodeType::ArrIni(_) = rhs.op {
				return walk(rhs);
			}
			let lhs_ = walk_nodecay(lhs);
			lhs_.checklval();
			let mut rhs_ = walk(rhs);
			let lty_ = lhs_.nodesctype(None);
			if lty_.ty == Ty::BOOL {
				rhs_ = Node::new_cast(BOOL_TY.clone(), rhs_);
			}
			return Node::new_assign(lty_, lhs_, rhs_);
		}
		IfThen(cond, then, elthen) => {
			match elthen {
				Some(elth) => { 
					return Node::new_if(walk(cond), walk(then), Some(walk(elth)));
				}
				_ => { return Node::new_if(walk(cond), walk(then), None); }
			}
		}
		Call(ctype, name, args) => {
			let mut v = vec![];
			for arg in args {
				v.push(walk(arg));
			}
			return Node::new_call(ctype.clone(), name.clone(), v);
		}
		For(init, cond, inc, body) => {
			return Node::new_for(walk(init), walk(cond), walk(inc), walk(body));
		}
		Deref(_, lhs) => {
			let lhs2 = walk(lhs);
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
			let lhs2 = walk(lhs);
			lhs2.checklval();
			return Node::new_addr(lhs2.nodesctype(None).ptr_to(), lhs2);
		}
		Equal(lhs, rhs) => {
			return Node::new_equal(walk(lhs), walk(rhs));
		}
		Ne(lhs, rhs) => {
			return Node::new_neq(walk(lhs), walk(rhs));
		}
		DoWhile(body, cond) => {
			return Node::new_dowhile(walk(body), walk(cond));
		}
		Switch(cond, body, case_conds) => {
			return Node::new_switch(walk(cond), walk(body), case_conds.clone());
		}
		Case(val, body) => {
			return Node::new_case(*val.clone(), walk(body));
		}
		Dot(_, expr, name) => {
			let expr2 = walk(expr);
			match expr2.nodesctype(None).ty {
				Ty::STRUCT(_, mb_map) => {
					if let Some(ctype) = mb_map.get(name) {
						let lhs = Node::new_dot(ctype.clone(), expr2, name.clone());
						return maybe_decay(lhs, decay);
					}
					// error(&format!("member missing."));
					// for debug.
					panic!("member missing.");
				}
				_ => {
					// error(&format!("struct expected before ."));
					// for debug.
					panic!("struct expected before . {:?}", expr2.clone()); 
				}
			}
		}
		Not(expr) => {
			let expr2 = walk(expr);
			return Node::new_not(expr2);
		}
		Ternary(_, cond, then, els) => {
			let cond2 = walk(cond);
			let then2 = walk(then);
			let els2 = walk(els);
			return Node::new_ternary(then2.nodesctype(Some(INT_TY.clone())), cond2, then2, els2);
		}
		TupleExpr(_, lhs, rhs) => {
			return binwalk(Node::new_tuple, lhs, rhs, 1);
		}
		IncDec(_, selector, expr) => {
			let lhs = walk(expr);
			lhs.checklval();
			return Node::new_incdec(lhs.nodesctype(None), *selector, lhs);
		}
		ArrIni(arrini) => {
			let mut new_arrini = vec![];
			for (lhs, rhs) in arrini {
				let lhs2 = walk_nodecay(lhs);
				lhs2.checklval();
				let mut rhs2 = walk(rhs);
				let lty = lhs2.nodesctype(None);
				if lty.ty == Ty::BOOL {
					rhs2 = Node::new_cast(BOOL_TY.clone(), rhs2);
				}
				new_arrini.push((lhs2, rhs2));
			}
			return Node::new_arrini(new_arrini);
		}
		Break | Continue => {
			return node.clone();
		}
		NULL => {
			return Node::new_null();
		}
		_ => { panic!("sema error at: {:?}", node); }
	}
}

pub fn sema(program: &mut Program) {
	
	let mut nodes = vec![];
	let program_nodes = std::mem::replace(&mut program.nodes, vec![]);

	for topnode in program_nodes {

		match topnode.op {
			Func(ctype, ident, args, body, stacksize) => {
				// eval body
				let body = walk(&body);
				let node = Node::new_func(ctype.clone(), ident.clone(), args, body, stacksize);
				nodes.push(node);
			}
			NULL => { continue; }
			_ => { panic!("funode should be NodeType::Func or NULL but got {:?}", topnode.op); }
		}
	}
	program.nodes = nodes;
}