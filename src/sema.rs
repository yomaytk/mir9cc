use super::parse::{*, NodeType::*};

use std::collections::HashMap;
use std::sync::Mutex;

lazy_static! {
	pub static ref VARS: Mutex<HashMap<String, usize>> = Mutex::new(HashMap::new());
	pub static ref STACKSIZE: Mutex<usize> = Mutex::new(0);
}

pub fn walk(node: &Node) -> Node {
	match &node.ty {
		Num => { return Node::new_node_num(); }
		BinaryTree(_, lhs, rhs)  => {
			walk(lhs.as_ref().unwrap());
			walk(rhs.as_ref().unwrap());
		}
		EqTree(_, lhs, rhs) | LogAnd(lhs, rhs) 
		| LogOr(lhs, rhs) => {
			walk(lhs);
			walk(rhs);
		}
		Ret(lhs) => { walk(lhs); }
		Expr(lhs) => { walk(lhs); }
		CompStmt(lhsv) => {
			for lhs in lhsv {
				walk(lhs);
			}
		}
		Ident(name) => {
			if let Some(off) = VARS.lock().unwrap().get(name) {
				
			}
			panic!("\"{}\" is not defined.", name);
		}
	}
}