use super::token::{*, TokenType::*};
use IrType::*;
use super::parse::*;

#[allow(dead_code)]
#[derive(Debug)]
pub enum IrType {
	IrImm,
	IrMov,
	IrPlus,
	IrMinus,
	IrMul,
	IrDiv,
	IrRet,
	IrExpr,
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
			TokenPlus => { IrPlus },
			TokenMinus => {  IrMinus },
			TokenMul => { IrMul },
			TokenDiv => { IrDiv },
			TokenEof => { panic!("tokeneof!!!"); }
			_ => { panic!("tokentype2irtype error."); }
		}
	}
}

// allocate of index for register to NodeNum
fn gen_expr(node: &Node, code: &mut Vec<Ir>) -> usize {

	match &node.ty {
		NodeType::Num => {
			let r = code.len();
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
		_ => { panic!("gen_expr error."); }
	}

}

fn gen_stmt(node: &Node, code: &mut Vec<Ir>) {
	match &node.ty {
		NodeType::Ret(lhs) => {
			let lhi= gen_expr(lhs.as_ref(), code);
			code.push(Ir::new(IrRet, lhi, 0));
		},
		NodeType::Expr(lhs) => {
			let lhi = gen_expr(lhs.as_ref(), code);
			code.push(Ir::new(IrExpr, lhi, 0));
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
	gen_stmt(node, &mut code);
	code
}