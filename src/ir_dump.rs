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
		(IrOp::IrLe, IrInfo::new("LE", IrType::RegReg)),
		(IrOp::IrImm, IrInfo::new("IMM", IrType::RegImm)),
		(IrOp::IrBpRel, IrInfo::new("BPREL", IrType::RegImm)),
		(IrOp::IrMov, IrInfo::new("MOV", IrType::RegReg)),
		(IrOp::IrLabel, IrInfo::new("", IrType::Label)),
		(IrOp::IrUnless, IrInfo::new("UNLESS", IrType::RegLabel)),
		(IrOp::IrRet, IrInfo::new("RET", IrType::Reg)),
		(IrOp::IrLoad(0), IrInfo::new("LOAD", IrType::RegReg)),
		(IrOp::IrStore(0), IrInfo::new("STORE", IrType::RegReg)),
		(IrOp::IrJmp, IrInfo::new("JMP", IrType::Label)),
		(IrOp::IrCall { name: format!(""), len: 0, args: vec![] }, IrInfo::new("CALL", IrType::Call)),
		(IrOp::IrStoreArg(0), IrInfo::new("STOREARG", IrType::ImmImm)),
		(IrOp::IrLabelAddr(String::new()), IrInfo::new("LABELADDR", IrType::LabelAddr)),
		(IrOp::IrEqual, IrInfo::new("Equal", IrType::RegReg)),
		(IrOp::IrNe, IrInfo::new("Ne", IrType::RegReg)),
		(IrOp::IrIf, IrInfo::new("IF", IrType::Reg)),
		(IrOp::IrOr, IrInfo::new("OR", IrType::RegReg)),
		(IrOp::IrXor, IrInfo::new("XOR", IrType::RegReg)),
		(IrOp::IrAnd, IrInfo::new("AND", IrType::RegReg)),
		(IrOp::IrShl, IrInfo::new("SHL", IrType::RegReg)),
		(IrOp::IrShr, IrInfo::new("SHR", IrType::RegReg)),
		(IrOp::IrMod, IrInfo::new("MOD", IrType::RegReg)),
		(IrOp::IrNeg, IrInfo::new("NEG", IrType::Reg)),
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
