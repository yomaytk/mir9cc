use super::token::{*, TokenType::*};
use IrOp::*;
use IrType::*;
use super::parse::*;

use std::sync::Mutex;
use std::collections::HashMap;
use std::fmt;

lazy_static! {
	pub static ref REGNO: Mutex<usize> = Mutex::new(1);
	pub static ref VARS: Mutex<HashMap<String, usize>> = Mutex::new(HashMap::new());
	pub static ref STACKSIZE: Mutex<usize> = Mutex::new(0);
	pub static ref IRINFO: [IrInfo; 18] = [
		IrInfo::new(IrOp::IrAdd, "ADD", IrType::RegReg),
		IrInfo::new(IrOp::IrSub, "SUB", IrType::RegReg),
		IrInfo::new(IrOp::IrMul, "MUL", IrType::RegReg),
		IrInfo::new(IrOp::IrDiv, "DIV", IrType::RegReg),
		IrInfo::new(IrOp::IrImm, "MOV", IrType::RegImm),
		IrInfo::new(IrOp::IrSubImm, "SUB", IrType::RegImm),
		IrInfo::new(IrOp::IrMov, "MOV", IrType::RegReg),
		IrInfo::new(IrOp::IrLabel, "", IrType::Label),
		IrInfo::new(IrOp::IrUnless, "UNLESS", IrType::RegLabel),
		IrInfo::new(IrOp::IrRet, "RET", IrType::Reg),
		IrInfo::new(IrOp::IrAlloc, "ALLOCA", IrType::RegImm),
		IrInfo::new(IrOp::IrLoad, "LOAD", IrType::RegReg),
		IrInfo::new(IrOp::IrStore, "STORE", IrType::RegReg),
		IrInfo::new(IrOp::IrJmp, "JMP", IrType::Label),
		IrInfo::new(IrOp::IrCall{ name: String::from(""), len: 0, args: vec![] }, "CALL", IrType::Call),
		IrInfo::new(IrOp::IrSaveArgs, "SAVEARGS", IrType::Imm),
		IrInfo::new(IrOp::IrKill, "KILL", IrType::Reg),
		IrInfo::new(IrOp::IrNop, "NOP", IrType::NoArg)
	];
	pub static ref LABEL: Mutex<usize> = Mutex::new(0);
}

#[allow(dead_code)]
#[derive(Debug, PartialEq)]
pub enum IrOp {
	IrImm,
	IrMov,
	IrAdd,
	IrSubImm,
	IrSub,
	IrMul,
	IrDiv,
	IrRet,
	IrExpr,
	IrAlloc,
	IrStore,
	IrLoad,
	IrLabel,
	IrUnless,
	IrJmp,
	IrCall { name: String, len: usize, args: Vec<usize> },
	IrSaveArgs,
	IrKill,
	IrNop,
}

#[derive(Debug)]
pub struct Ir {
	pub op: IrOp,
	pub lhs: usize,
	pub rhs: usize,
}

impl Ir {
	fn new(ty: IrOp, lhs: usize, rhs: usize) -> Self {
		Self {
			op: ty,
			lhs: lhs,
			rhs: rhs,
		}
	}
	fn fouroperator2irop(ty: TokenType) -> IrOp {
		match ty {
			TokenAdd => { IrAdd },
			TokenSub => { IrSub },
			TokenMul => { IrMul },
			TokenDiv => { IrDiv },
			TokenEof => { panic!("tokeneof!!!"); }
			_ => { panic!("fouroperator2irop error."); }
		}
	}
	pub fn get_irinfo(&self) -> &IrInfo {
		for i in 0..IRINFO.len() {
			match &self.op {
				IrCall { name, len, args } => {
					let _name = name;
					let _len = len;
					let _args = args;
					let op = &IRINFO[14];
					assert_eq!(op, &IrInfo::new(IrOp::IrCall{ name: String::from(""), len: 0, args: vec![] }, "CALL", IrType::Call));
					return op;
				},
				_ => {
					if self.op == IRINFO[i].op {
						return &IRINFO[i];
					}
				}
			}
		}
		panic!("wrong IrOp found");
	}
	fn tostr(&self) -> String {
		let irinfo = self.get_irinfo();
		match irinfo.ty.clone() {
			NoArg => { format!("Nop") },
			Reg => { format!("{}, r{}", irinfo.ty, self.lhs) },
			Label => { format!("{}", self.lhs) },
			RegReg => { format!("{} r{}, r{}", irinfo.name, self.lhs, self.rhs) },
			RegImm => { format!("{} r{}, {}", irinfo.name, self.lhs, self.rhs) },
			RegLabel => { format!("{} r{}, .L{}", irinfo.name, self.lhs, self.rhs) },
			Call => {
				match &self.op {
					IrCall{ name, len, args } => {
						let _len = len;
						let mut s = String::from(format!("{} {}(", irinfo.name, name));
						for arg in args {
							s += &format!(", {}", arg);
						}
						s += &format!("), {}, {})", self.lhs, self.rhs);
						return s;
					}
					_ => { panic!("tostr call error {}"); }
				}
			}
			Imm => { format!("{} {}", irinfo.name, self.lhs) },
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum IrType {
	NoArg,
	Reg,
	Label,
	RegReg,
	RegImm,
	RegLabel,
	Call,
	Imm,
}

impl fmt::Display for IrType {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			NoArg => { write!(f, "NoArg") },
			Reg => { write!(f, "Reg") },
			Label => { write!(f, "Label") },
			RegReg => { write!(f, "RegReg") },
			RegImm => { write!(f, "RegImm") },
			RegLabel => { write!(f, "RegLabel") },
			Call => { write!(f, "Call") },
			Imm => { write!(f, "Imm") },
		}
	}
}

#[derive(Debug, PartialEq)]
pub struct IrInfo {
	pub op: IrOp,
	pub name: &'static str,
	pub ty: IrType,
}

impl IrInfo {
	fn new(op: IrOp, name: &'static str, ty: IrType) -> Self {
		Self {
			op,
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

pub struct Function {
	pub name: String,
	pub irs: Vec<Ir>,
	pub stacksize: usize,
}

impl Function {
	fn new(name: String, irs: Vec<Ir>, stacksize: usize) -> Self {
		Self {
			name,
			irs,
			stacksize
		}
	}
}

fn gen_lval(node: &Node, code: &mut Vec<Ir>) -> usize {
	
	match &node.ty {
		NodeType::Ident(s) => {
			if VARS.lock().unwrap().get(s).is_none() {
				*STACKSIZE.lock().unwrap() += 8;
				VARS.lock().unwrap().insert(
					s.clone(),
					*STACKSIZE.lock().unwrap(),
				);
			}
			*REGNO.lock().unwrap() += 1;
			let r1 = *REGNO.lock().unwrap();
			code.push(Ir::new(IrMov, r1, 0)); 
			
			let off = *VARS.lock().unwrap().get(s).unwrap();
			code.push(Ir::new(IrSubImm, r1, off));
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
			code.push(Ir::new(Ir::fouroperator2irop(ty.clone()), lhi, rhi));
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
		},
		NodeType::Call(ident, callarg) => {
			let mut args = vec![];
			for arg in callarg {
				args.push(gen_expr(arg, code));
			}
			*REGNO.lock().unwrap() += 1;
			let r = *REGNO.lock().unwrap();
			code.push(Ir::new(IrCall{ 
				name: (*ident).clone(), 
				len: args.len(),
				args: args 
			} , r, 0));
			return r;
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
			gen_expr(lhs.as_ref(), code);
		},
		NodeType::IfThen(cond, then, elthen) => {
			let lhi = gen_expr(cond, code);
			*LABEL.lock().unwrap() += 1;
			code.push(Ir::new(IrUnless, lhi, *LABEL.lock().unwrap()));
			code.push(Ir::new(IrKill, lhi, 0));
			gen_stmt(then, code);
			match elthen {
				Some(elnode) => {
					code.push(Ir::new(IrJmp, *LABEL.lock().unwrap(), 0));
					code.push(Ir::new(IrLabel, *LABEL.lock().unwrap(), 0));
					gen_stmt(elnode, code);
					*LABEL.lock().unwrap() += 1;
					code.push(Ir::new(IrLabel, *LABEL.lock().unwrap(), 0));
				},
				None => {
					code.push(Ir::new(IrLabel, *LABEL.lock().unwrap(), 0));
				}
			}
		}
		NodeType::CompStmt(lhs) => {
			for expr in lhs {
				gen_stmt(expr, code);
			}
		},
		enode => { panic!("unexpeceted node {:?}", enode); }
	}
}

pub fn gen_args(args: &Vec<Node>, code: &mut Vec<Ir>) {
	
	code.push(Ir::new(IrSaveArgs, args.len(), 0));

	for arg in args {
		match &arg.ty {
			NodeType::Ident(s) => {
				*STACKSIZE.lock().unwrap() += 8;
				VARS.lock().unwrap().insert(
					s.clone(),
					*STACKSIZE.lock().unwrap(),
				);
			}
			_ => { panic!("dummy argument should be ident."); }
		}
	}
}

// generate IR Vector
pub fn gen_ir(funcs: &Vec<Node>) -> Vec<Function> {
	
	let mut v = vec![];

	for funode in funcs {
		
		let mut code = vec![];
		*REGNO.lock().unwrap() = 1;
		*VARS.lock().unwrap() = HashMap::new();
		*STACKSIZE.lock().unwrap() = 0;
		
		match &funode.ty {
			NodeType::Func(name, args, body) => {
				gen_args(args, &mut code);
				gen_stmt(body, &mut code);
				let func = Function::new(name.clone(), code, *STACKSIZE.lock().unwrap());
				v.push(func);
			}
			_ => { panic!(" should be func node at gen_ir: {:?}", funode); }
		}
	}

	return v;
}