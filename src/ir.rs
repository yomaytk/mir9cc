use super::token::{*, TokenType::*};
use IrType::*;
use super::parse::*;
use super::parse::NodeType::*;

#[allow(dead_code)]
#[derive(Debug)]
pub enum IrType {
	IrImm,
	IrMov,
	IrPlus,
	IrMinus,
	IrReturn,
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
	fn tokentype2irtype(ty: &TokenType) -> IrType {
		match ty {
			TokenPlus => { IrPlus },
			TokenMinus => {  IrMinus },
			_ => { panic!("tokentype2irtype error."); }
		}
	}
}

// allocate of index for register to NodeNum
fn gen_ir_sub(node: &Node, ins: &mut Vec<Ir>, regno: usize) -> (usize, usize) {

	match &node.ty {
		NodeNum => {
			let ir = Ir::new(IrImm, regno, node.val as usize);
			ins.push(ir);
			return (regno, regno+1);
		},
		BinaryTree(ty, lhs, rhs) => {
			let (lhi, lreg) = gen_ir_sub(lhs.as_ref().unwrap(), ins, regno);
			let (rhi, rreg) = gen_ir_sub(rhs.as_ref().unwrap(), ins, lreg);
			ins.push(Ir::new(Ir::tokentype2irtype(ty), lhi, rhi));
			ins.push(Ir::new(IrKill, rhi, 0));
			return (lhi, rreg);
		}
	}
}

// generate IR Vector
pub fn gen_ir(node: &Node, ins: &mut Vec<Ir>) {
	let (r, _) = gen_ir_sub(node, ins, 0);
	ins.push(Ir::new(IrReturn, r, 0));
}