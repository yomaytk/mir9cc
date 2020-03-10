use super::token::{*, TokenType::*};
use IrOp::*;
use IrType::*;
use super::parse::*;

use std::sync::Mutex;
use std::collections::HashMap;
use std::fmt;

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
	pub static ref REGNO: Mutex<usize> = Mutex::new(1);
	pub static ref VARS: Mutex<HashMap<String, usize>> = Mutex::new(HashMap::new());
	pub static ref STACKSIZE: Mutex<usize> = Mutex::new(0);
	pub static ref IRINFO: Mutex<HashMap<IrOp, IrInfo>> = Mutex::new(hash![
		(IrOp::IrAdd, IrInfo::new("ADD", IrType::RegReg)),
		(IrOp::IrSub, IrInfo::new("SUB", IrType::RegReg)),
		(IrOp::IrMul, IrInfo::new("MUL", IrType::RegReg)),
		(IrOp::IrDiv, IrInfo::new("DIV", IrType::RegReg)),
		(IrOp::IrImm, IrInfo::new("MOV", IrType::RegImm)),
		(IrOp::IrSubImm, IrInfo::new("SUB", IrType::RegImm)),
		(IrOp::IrMov, IrInfo::new("MOV", IrType::RegReg)),
		(IrOp::IrLabel, IrInfo::new("", IrType::Label)),
		(IrOp::IrUnless, IrInfo::new("UNLESS", IrType::RegLabel)),
		(IrOp::IrRet, IrInfo::new("RET", IrType::Reg)),
		(IrOp::IrLoad, IrInfo::new("LOAD", IrType::RegReg)),
		(IrOp::IrStore, IrInfo::new("STORE", IrType::RegReg)),
		(IrOp::IrJmp, IrInfo::new("JMP", IrType::Label)),
		(IrOp::IrCall { name: format!(""), len: 0, args: vec![] }, IrInfo::new("CALL", IrType::Call)),
		(IrOp::IrSaveArgs, IrInfo::new("SAVEARGS", IrType::Imm)),
		(IrOp::IrKill, IrInfo::new("KILL", IrType::Reg)),
		(IrOp::IrNop, IrInfo::new("NOP", IrType::NoArg))
	]);
	pub static ref LABEL: Mutex<usize> = Mutex::new(0);
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, std::cmp::Eq, std::hash::Hash)]
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
	pub fn get_irinfo(&self) -> IrInfo {
		match &self.op {
			IrCall { name, len, args } => {
				let _name = name;
				let _len = len;
				let _args = args;
				return IRINFO.lock().unwrap().get(&IrOp::IrCall { name: format!(""), len: 0, args: vec![] }).unwrap().clone();
			},
			_ => {
				return IRINFO.lock().unwrap().get(&self.op).unwrap().clone();
			}
		}
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
		NodeType::LogAnd(lhs, rhs) => {
			let r1 = gen_expr(lhs, code);
			*LABEL.lock().unwrap() += 1;
			let x = *LABEL.lock().unwrap();
			code.push(Ir::new(IrUnless, r1, x));
			let r2 = gen_expr(rhs, code);
			code.push(Ir::new(IrMov, r1, r2));
			code.push(Ir::new(IrKill, r2, 0));
			code.push(Ir::new(IrUnless, r1, x));
			code.push(Ir::new(IrImm, r1, 1));
			code.push(Ir::new(IrLabel, x, 0));
			return r1;
		}
		NodeType::LogOr(lhs, rhs) => {
			let r1 = gen_expr(lhs, code);
			*LABEL.lock().unwrap() += 2;
			let x = *LABEL.lock().unwrap()-1;
			let y = x+1;
			code.push(Ir::new(IrUnless, r1, x));
			code.push(Ir::new(IrImm, r1, 1));
			code.push(Ir::new(IrJmp, y, 0));
			code.push(Ir::new(IrLabel, x, 0));
			let r2 = gen_expr(rhs, code);
			code.push(Ir::new(IrMov, r1, r2));
			code.push(Ir::new(IrKill, r2, 0));
			code.push(Ir::new(IrUnless, r1, y));
			code.push(Ir::new(IrImm, r1, 1));
			code.push(Ir::new(IrLabel, y, 0));
			return r1;
		}
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