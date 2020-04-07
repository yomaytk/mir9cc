use super::gen_ir::*;
use std::collections::HashMap;
use std::sync::Mutex;

macro_rules! hash {
	( $( $t:expr),* ) => {
		{
			let mut temp_hash = HashMap::new();
			$(
				temp_hash.insert($t.0, $t.1);
			)*
			temp_hash
		}
	};
}

lazy_static! {
    pub static ref IRINFO: Mutex<HashMap<IrOp, IrInfo>> = Mutex::new(hash![
		(IrOp::IrAdd, IrInfo::new("ADD", IrType::RegReg)),
		(IrOp::IrSub, IrInfo::new("SUB", IrType::RegReg)),
		(IrOp::IrMul, IrInfo::new("MUL", IrType::RegReg)),
		(IrOp::IrDiv, IrInfo::new("DIV", IrType::RegReg)),
		(IrOp::IrLt, IrInfo::new("LT", IrType::RegReg)),
		(IrOp::IrImm, IrInfo::new("MOV", IrType::RegImm)),
		(IrOp::IrBpRel, IrInfo::new("BPREL", IrType::RegImm)),
		(IrOp::IrMov, IrInfo::new("MOV", IrType::RegReg)),
		(IrOp::IrLabel, IrInfo::new("", IrType::Label)),
		(IrOp::IrUnless, IrInfo::new("UNLESS", IrType::RegLabel)),
		(IrOp::IrRet, IrInfo::new("RET", IrType::Reg)),
		(IrOp::IrLoad8, IrInfo::new("LOAD8", IrType::RegReg)),
		(IrOp::IrLoad32, IrInfo::new("LOAD32", IrType::RegReg)),
		(IrOp::IrLoad64, IrInfo::new("LOAD64", IrType::RegReg)),
		(IrOp::IrStore8, IrInfo::new("STORE8", IrType::RegReg)),
		(IrOp::IrStore32, IrInfo::new("STORE32", IrType::RegReg)),
		(IrOp::IrStore64, IrInfo::new("STORE64", IrType::RegReg)),
		(IrOp::IrJmp, IrInfo::new("JMP", IrType::Label)),
		(IrOp::IrCall { name: format!(""), len: 0, args: vec![] }, IrInfo::new("CALL", IrType::Call)),
		(IrOp::IrStoreArgs8, IrInfo::new("STOREARGS8", IrType::ImmImm)),
		(IrOp::IrStoreArgs32, IrInfo::new("STOREARGS32", IrType::ImmImm)),
		(IrOp::IrStoreArgs64, IrInfo::new("STOREARGS64", IrType::ImmImm)),
		(IrOp::IrLabelAddr(String::new()), IrInfo::new("LABELADDR", IrType::LabelAddr)),
		(IrOp::IrEqEq, IrInfo::new("EqEq", IrType::RegReg)),
		(IrOp::IrNe, IrInfo::new("Neq", IrType::RegReg)),
		(IrOp::IrIf, IrInfo::new("If", IrType::Reg)),
		(IrOp::IrKill, IrInfo::new("KILL", IrType::Reg)),
		(IrOp::IrNop, IrInfo::new("NOP", IrType::NoArg))
	]);
}

#[derive(Debug, PartialEq, Clone)]
pub struct IrInfo {
	pub name: &'static str,
	pub ty: IrType,
}

impl IrInfo {
	fn new(name: &'static str, ty: IrType) -> Self {
		Self {
			name,
			ty,
		}
	}
	pub fn dump_ir(irv: &Vec<Function>, dump_option: &str){
		println!("{}: ", dump_option);
		for fun in irv {
			println!("{}():", fun.name);
			for ir in &fun.irs {
				println!("{}", ir.tostr());
			}
		}
	}
}
