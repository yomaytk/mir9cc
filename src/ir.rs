use super::token::{*, TokenType::*};
use IrType::*;
use super::parse::*;

use std::sync::Mutex;
use std::collections::HashMap;

lazy_static! {
	pub static ref REGNO: Mutex<usize> = Mutex::new(0);
	pub static ref VARS: Mutex<HashMap<String, usize>> = Mutex::new(HashMap::new());
	pub static ref BASEREG: Mutex<usize> = Mutex::new(0);
	pub static ref BPOFF: Mutex<usize> = Mutex::new(0);
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum IrType {
	IrImm,
	IrMov,
	IrAdd,
	IrAddImm,
	IrSub,
	IrMul,
	IrDiv,
	IrRet,
	IrExpr,
	IrAlloc,
	IrStore,
	IrLoad,
	IrKill,
	IrNop,
}

#[derive(Debug)]
pub struct Ir {
	pub ty: IrType,
	pub lhs: usize,
	pub rhs: usize,
}

impl Ir {
	fn new(ty: IrType, lhs: usize, rhs: usize) -> Self {
		Self {
			ty: ty,
			lhs: lhs,
			rhs: rhs,
		}
	}
	fn tokentype2irtype(ty: TokenType) -> IrType {
		match ty {
			TokenAdd => { IrAdd },
			TokenSub => {  IrSub },
			TokenMul => { IrMul },
			TokenDiv => { IrDiv },
			TokenEof => { panic!("tokeneof!!!"); }
			_ => { panic!("tokentype2irtype error."); }
		}
	}
}

fn gen_lval(node: &Node, code: &mut Vec<Ir>) -> usize {
	
	match &node.ty {
		NodeType::Ident(s) => {
			if VARS.lock().unwrap().get(s).is_none() {
				VARS.lock().unwrap().insert(
					s.clone(),
					*BPOFF.lock().unwrap(),
				);
				*BPOFF.lock().unwrap() += 8;
			}
			*REGNO.lock().unwrap() += 1;
			let r1 = *REGNO.lock().unwrap();
			code.push(Ir::new(IrMov, r1, *BASEREG.lock().unwrap()));	// mov rbp to the register 
			
			let off = *VARS.lock().unwrap().get(s).unwrap();
			code.push(Ir::new(IrAddImm, r1, off));
			return r1;
		},
		_ => { panic!("not an lvalue")}
	}
}

// allocate of index for register to NodeNum
fn gen_expr(node: &Node, code: &mut Vec<Ir>) -> usize {

	match &node.ty {
		NodeType::Num => {
			*REGNO.lock().unwrap() += 1;
			let r = *REGNO.lock().unwrap();
			let ir = Ir::new(IrImm, r, node.val as usize);
			code.push(ir);
			return r;
		},
		NodeType::BinaryTree(ty, lhs, rhs) => {
			let lhi = gen_expr(lhs.as_ref().unwrap(), code);
			let rhi = gen_expr(rhs.as_ref().unwrap(), code);
			code.push(Ir::new(Ir::tokentype2irtype(ty.clone()), lhi, rhi));
			code.push(Ir::new(IrKill, rhi, 0));
			return lhi;
		},
		NodeType::Ident(_) => {
			let lhi = gen_lval(node, code);
			code.push(Ir::new(IrLoad, lhi, lhi));
			return lhi;
		},
		NodeType::EqTree(_, lhs, rhs) => {
			let lhi = gen_lval(lhs, code);
			let rhi = gen_expr(rhs, code);
			code.push(Ir::new(IrStore, lhi, rhi));
			code.push(Ir::new(IrKill, rhi, 0));
			return lhi;
		}
		_ => { panic!("gen_expr error."); }
	}

}


fn gen_stmt(node: &Node, code: &mut Vec<Ir>) {
	match &node.ty {
		NodeType::Ret(lhs) => {
			let lhi= gen_expr(lhs.as_ref(), code);
			code.push(Ir::new(IrRet, lhi, 0));
			code.push(Ir::new(IrKill, lhi, 0));
		},
		NodeType::Expr(lhs) => {
			let _ = gen_expr(lhs.as_ref(), code);
		},
		NodeType::CompStmt(lhs) => {
			for expr in lhs {
				gen_stmt(expr, code);
			}
		},
		enode => { panic!("unexpeceted node {:?}", enode); }
	}
}

// generate IR Vector
pub fn gen_ir(node: &Node) -> Vec<Ir>{
	let mut code = vec![];
	code.push(Ir::new(IrAlloc, *BASEREG.lock().unwrap(), 0));
	gen_stmt(node, &mut code);
	code[0].rhs = *BPOFF.lock().unwrap();
	return code;
}