use super::token::{*, TokenType::*};
use IrOp::*;
use IrType::*;
use super::parse::*;
use super::ir_dump::{IrInfo, IRINFO};
// use super::lib::*;
use super::mir::*;

use std::sync::Mutex;
use std::fmt;

// mir9cc's code generation is two-pass. In the first pass, abstract
// syntax trees are compiled to IR (intermediate representation).
//
// IR resembles the real x86-64 instruction set, but it has infinite
// number of registers. We don't try too hard to reuse registers in
// this pass. Instead, we "kill" registers to mark them as dead when
// we are done with them and use new registers.
//
// Such infinite number of registers are mapped to a finite registers
// in a later pass.

lazy_static! {
	pub static ref REGNO: Mutex<usize> = Mutex::new(1);
	pub static ref RETURN_LABEL: Mutex<usize> = Mutex::new(0);
	pub static ref RETURN_REG: Mutex<usize> = Mutex::new(0);
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, std::cmp::Eq, std::hash::Hash)]
pub enum IrOp {
	IrImm,
	IrMov,
	IrAdd(bool),
	IrBpRel,
	IrSub(bool),
	IrMul(bool),
	IrDiv,
	IrRet,
	IrExpr,
	IrStore(usize),
	IrLoad(usize),
	IrLabel,
	IrUnless,
	IrJmp,
	IrCall { name: String, len: usize, args: Vec<usize> },
	IrStoreArg(usize),
	IrLt,
	IrEqual, 
	IrNe,
	IrIf,
	IrLabelAddr(String),
	IrOr,
	IrXor(bool, i32),
	IrAnd,
	IrLe,
	IrShl,
	IrShr,
	IrMod,
	IrNeg,
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
	pub fn new(ty: IrOp, lhs: usize, rhs: usize) -> Self {
		Self {
			op: ty,
			lhs: lhs,
			rhs: rhs,
		}
	}
	fn bittype(ty: &TokenType) -> IrOp {
		match ty {
			TokenAdd => { IrAdd(false) },
			TokenSub => { IrSub(false) },
			TokenStar => { IrMul(false) },
			TokenDiv => { IrDiv },
			TokenLt => { IrLt },
			TokenLe => { IrLe },
			TokenShl => { IrShl },
			TokenShr => { IrShr },
			TokenMod => { IrMod },
			TokenAmpersand => { IrAnd },
			TokenOr => { IrOr },
			TokenXor => { IrXor(false, 1) },
			TokenEof => { panic!("tokeneof!!!"); }
			_ => { panic!("bittype error."); }
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
			IrLabelAddr(_) => {
				return IRINFO.lock().unwrap().get(&IrOp::IrLabelAddr(String::new())).unwrap().clone();
			}
			IrLoad(_) => {
				return IRINFO.lock().unwrap().get(&IrOp::IrLoad(0)).unwrap().clone();
			}
			IrStore(_) => {
				return IRINFO.lock().unwrap().get(&IrOp::IrStore(0)).unwrap().clone();
			}
			IrStoreArg(_) => {
				return IRINFO.lock().unwrap().get(&IrOp::IrStoreArg(0)).unwrap().clone();
			}
			IrAdd(_) => {
				return IRINFO.lock().unwrap().get(&IrOp::IrAdd(true)).unwrap().clone();
			}
			IrSub(_) => {
				return IRINFO.lock().unwrap().get(&IrOp::IrSub(true)).unwrap().clone();
			}
			IrMul(_) => {
				return IRINFO.lock().unwrap().get(&IrOp::IrMul(true)).unwrap().clone();
			}
			IrXor(_, _) => {
				return IRINFO.lock().unwrap().get(&IrOp::IrXor(true, 0)).unwrap().clone();
			}
			_ => {
				return IRINFO.lock().unwrap().get(&self.op).unwrap().clone();
			}
		}
	}
	pub fn tostr(&self) -> String {
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
			ImmImm => { format!("{} {} {}", irinfo.name, self.lhs, self.rhs) }
			LabelAddr => { format!("{} r{} .L.str{}", irinfo.name, self.lhs, self.rhs) }
			Mem => { 
				match self.op {
					IrLoad(size) | IrStore(size) | IrStoreArg(size) => { format!("{}{} r{} r{}", irinfo.name, size, self.lhs, self.rhs) }
					_ => { panic!("tostr Mem error."); }
				} 
			}
			Binary => {
				match self.op {
					IrAdd(is_imm) | IrSub(is_imm) | IrMul(is_imm) => {
						if is_imm { format!("{} r{}, {}", irinfo.name, self.lhs, self.rhs) }
						else { format!("{} r{}, r{}", irinfo.name, self.lhs, self.rhs) }
					}
					_ => { panic!("tostr IrBinary error."); }
				}
			}
		}
	}
	fn add(op: IrOp, lhs: usize, rhs: usize, code: &mut Vec<Ir>) {
		code.push(Ir::new(op, lhs, rhs));
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
	ImmImm,
	LabelAddr,
	Mem,
	Binary,
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
			ImmImm => { write!(f, "ImmImm") },
			LabelAddr => { write!(f, "LabelAddr") },
			Mem => { write!(f, "Mem") },
			Binary => { write!(f, "Binary") },
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
			stacksize,
		}
	}
}

fn kill(r: usize, code: &mut Vec<Ir>) {
	Ir::add(IrKill, r, 0, code);
}

fn label(r: usize, code: &mut Vec<Ir>) {
	Ir::add(IrLabel, r, 0, code);
}

fn jmp(x: usize, code: &mut Vec<Ir>) {
	Ir::add(IrJmp, x, 0, code);
}

fn load(ctype: &Type, dst: usize, src: usize, code: &mut Vec<Ir>) {
	Ir::add(IrOp::IrLoad(ctype.size), dst, src, code);
}

fn store(ctype: &Type, dst: usize, src: usize, code: &mut Vec<Ir>) {
	Ir::add(IrOp::IrStore(ctype.size), dst, src, code);
}

fn store_arg(ctype: &Type, offset: usize, id: usize, code: &mut Vec<Ir>) {
	Ir::add(IrOp::IrStoreArg(ctype.size), offset, id, code);
}

fn gen_binop(irop: IrOp, lhs: &Node, rhs: &Node, code: &mut Vec<Ir>) -> usize {
	let r1 = gen_expr(lhs, code);
	let r2 = gen_expr(rhs, code);
	Ir::add(irop, r1, r2, code);
	kill(r2, code);
	return r1;
}

fn gen_inc_scale(ctype: &Type) -> usize {
	match ctype.ty {
		Ty::PTR => { return ctype.ptr_to.as_ref().unwrap().size; }
		_ => { return 1; }
	}
}

fn gen_pre_inc(ctype: &Type, lhs: &Node, code: &mut Vec<Ir>, num: i32) -> usize {
	let r1 = gen_lval(lhs, code);
	let r2 = new_regno();
	load(ctype, r2, r1, code);
	Ir::add(IrAdd(true), r2, num as usize * gen_inc_scale(ctype), code);
	store(ctype, r1, r2, code);
	kill(r1, code);
	return r2;
}

fn gen_post_inc(ctype: &Type, lhs: &Node, code: &mut Vec<Ir>, num: i32) -> usize {
	let r = gen_pre_inc(ctype, lhs, code, num);
	Ir::add(IrSub(true), r, num as usize * gen_inc_scale(ctype), code);
	return r;
}

fn new_regno() -> usize {
	*REGNO.lock().unwrap() += 1;
	return *REGNO.lock().unwrap();
}

// In C, all expressions that can be written on the left-hand side of
// the '=' operator must have an address in memory. In other words, if
// you can apply the '&' operator to take an address of some
// expression E, you can assign E to a new value.
//
// Other expressions, such as `1+2`, cannot be written on the lhs of
// '=', since they are just temporary values that don't have an address.
//
// The stuff that can be written on the lhs of '=' is called lvalue.
// Other values are called rvalue. An lvalue is essentially an address.
//
// When lvalues appear on the rvalue context, they are converted to
// rvalues by loading their values from their addresses. You can think
// '&' as an operator that suppresses such automatic lvalue-to-rvalue
// conversion.
//
// This function evaluates a given node as an lvalue.

fn gen_lval(node: &Node, code: &mut Vec<Ir>) -> usize {
	
	match &node.op {
		NodeType::Deref(_, expr) => {
			return gen_expr(expr, code);
		}
		NodeType::VarRef(var) => {
			let r = new_regno();
			if var.is_local {
				Ir::add(IrBpRel, r, var.offset, code);
			} else {
				Ir::add(IrLabelAddr(var.labelname.clone().unwrap()), r, 0, code);
			}
			return r;
		}
		NodeType::Dot(ctype, expr, _) => {
			let r1 = gen_lval(expr, code);
			Ir::add(IrAdd(true), r1, ctype.offset, code);
			return r1;
		}
		_ => { panic!("not an lvalue")}
	}
}

// allocate of index for register to NodeNum
fn gen_expr(node: &Node, code: &mut Vec<Ir>) -> usize {

	match &node.op {
		NodeType::Num(val) => {
			let r = new_regno();
			let ir = Ir::new(IrImm, r, *val as usize);
			code.push(ir);
			return r;
		},
		NodeType::BinaryTree(_, ty, lhs, rhs) => {
			match ty {
				TokenLogAnd => {
					let r1 = gen_expr(lhs, code);
					let x = new_label();
					Ir::add(IrUnless, r1, x, code);
					let r2 = gen_expr(rhs, code);
					Ir::add(IrMov, r1, r2, code);
					kill(r2, code);
					Ir::add(IrUnless, r1, x, code);
					Ir::add(IrImm, r1, 1, code);
					label(x, code);
					return r1;
				}
				TokenLogOr => {
					let r1 = gen_expr(lhs, code);
					let x = new_label();
					let y = new_label();
					Ir::add(IrUnless, r1, x, code);
					Ir::add(IrImm, r1, 1, code);
					jmp(y, code);
					label(x, code);
					let r2 = gen_expr(rhs, code);
					Ir::add(IrMov, r1, r2, code);
					kill(r2, code);
					Ir::add(IrUnless, r1, y, code);
					Ir::add(IrImm, r1, 1, code);
					label(y, code);
					return r1;
				}
				TokenTilde => {
					let lhi = gen_expr(lhs, code);
					Ir::add(IrXor(true, -1), lhi, 1, code);
					return lhi;
				}
				_ => {
					return gen_binop(Ir::bittype(ty), lhs, rhs, code);
				}
			}
		},
		NodeType::VarRef(var) => {
			let lhi = gen_lval(node, code);
			load(&var.ctype, lhi, lhi, code);
			return lhi;
		}
		NodeType::Dot(ctype, ..) => {
			let lhi = gen_lval(node, code);
			load(ctype, lhi, lhi, code);
			return lhi;
		},
		NodeType::Assign(ctype, lhs, rhs) => {
			let rhi = gen_expr(rhs, code);
			let lhi = gen_lval(lhs, code);
			store(ctype, lhi, rhi, code);
			kill(lhi, code);
			return rhi;
		},
		NodeType::Call(_, ident, callarg) => {
			let mut args = vec![];
			for arg in callarg {
				args.push(gen_expr(arg, code));
			}
			let r = new_regno();
			Ir::add(
				IrCall{ 
					name: (*ident).clone(), 
					len: args.len(),
					args: args.clone()
				}, 
				r, 
				0, 
				code
			);
			for arg in args {
				kill(arg, code);
			}
			return r;
		}
		NodeType::Deref(_, lhs) => {
			let r = gen_expr(lhs, code);
			load(lhs.nodesctype(None).ptr_to.unwrap().as_ref(), r, r, code);
			return r;
		}
		NodeType::Addr(_, lhs) => {
			return gen_lval(lhs, code);
		}
		NodeType::Equal(lhs, rhs) => {
			return gen_binop(IrEqual, lhs, rhs, code);
		}
		NodeType::Ne(lhs, rhs) => {
			return gen_binop(IrNe, lhs, rhs, code);
		}
		NodeType::Not(expr) => {
			let r1 = gen_expr(expr, code);
			let r2 = new_regno();
			Ir::add(IrImm, r2, 0, code);
			Ir::add(IrEqual, r1, r2, code);
			kill(r2, code);
			return r1;
		}
		NodeType::Ternary(_, cond, then, els) => {
			let x = new_label();
			let y = new_label();
			let r = gen_expr(cond, code);
			Ir::add(IrUnless, r, x, code);
			let r2 = gen_expr(then, code);
			Ir::add(IrMov, r, r2, code);
			kill(r2, code);
			jmp(y, code);
			label(x, code);
			let r3 = gen_expr(els, code);
			Ir::add(IrMov, r, r3, code);
			kill(r3, code);
			label(y, code);
			return r;
		}
		NodeType::TupleExpr(_, lhs, rhs) => {
			kill(gen_expr(lhs, code), code);
			return gen_expr(rhs, code);
		}
		NodeType::IncDec(ctype, selector, lhs) => {
			if *selector == 1 { return gen_post_inc(ctype, lhs, code, 1); }
			else { return gen_post_inc(ctype, lhs, code, -1); }
		}
		NodeType::Cast(ctype, expr) => {
			let r1 = gen_expr(expr, code);
			if ctype.ty != Ty::BOOL {
				return r1;
			}
			let r2 = new_regno();
			Ir::add(IrImm, r2, 0, code);
			Ir::add(IrNe, r1, r2, code);
			kill(r2, code);
			return r1;
		}
		NodeType::StmtExpr(_, body) => {
			if let NodeType::CompStmt(stmts) = &body.op {
				let len = stmts.len();
				for i in 0..len-1 {
					gen_stmt(&stmts[i], code);
				}
				if len > 0 {
					if let NodeType::Expr(ref expr) = stmts.last().unwrap().op {
						return gen_expr(expr, code);
					}
				}
			}
			let r = new_regno();
			Ir::add(IrImm, r, 0, code);
			return r;
		}
		_ => { panic!("gen_expr NodeType error at {:?}", node.op); }
	}

}


fn gen_stmt(node: &Node, code: &mut Vec<Ir>) {
	match &node.op {
		NodeType::NULL => { 
			return; 
		}
		NodeType::Ret(lhs) => {
			
			let lhi = gen_expr(lhs.as_ref(), code);
			
			if *RETURN_LABEL.lock().unwrap() > 0 {
				Ir::add(IrMov, *RETURN_REG.lock().unwrap(), lhi, code);
				kill(lhi, code);
				jmp(*RETURN_LABEL.lock().unwrap(), code);
				return;
			}
			Ir::add(IrRet, lhi, 0, code);
			kill(lhi, code);
		}
		NodeType::Expr(lhs) => {
			kill(gen_expr(lhs.as_ref(), code), code);
		}
		NodeType::IfThen(cond, then, elthen) => {
			let lhi = gen_expr(cond, code);
			let x1 = new_label();
			let x2 = new_label();
			Ir::add(IrUnless, lhi, x1, code);
			kill(lhi, code);
			gen_stmt(then, code);
			match elthen {
				Some(elnode) => {
					jmp(x2, code);
					label(x1, code);
					gen_stmt(elnode, code);
					label(x2, code);
				},
				None => {
					label(x1, code);
				}
			}
		}
		NodeType::CompStmt(lhs) => {
			for stmt in lhs {
				gen_stmt(stmt, code);
			}
		}
		NodeType::For(init, cond, inc, body, break_label, continue_label) => {
			let x = new_label();
			gen_stmt(init, code);
			label(x, code);
			match cond.op {
				NodeType::NULL => {}
				_ => {
					let r2 = gen_expr(cond, code);
					Ir::add(IrUnless, r2, *break_label, code);
					kill(r2, code);
				}
			}
			gen_stmt(body, code);
			label(*continue_label, code);
			gen_stmt(inc, code);
			jmp(x, code);
			label(*break_label, code);
		}
		NodeType::DoWhile(body, cond, break_label, continue_label) => {
			let x = new_label();
			label(x, code);
			gen_stmt(body, code);
			label(*continue_label, code);
			let r = gen_expr(cond, code);
			Ir::add(IrIf, r, x, code);
			kill(r, code);
			label(*break_label, code);
		}
		NodeType::Break(break_label) => {
			jmp(*break_label, code);
		}
		NodeType::Continue(continue_label) => {
			jmp(*continue_label, code);
		}
		enode => { panic!("unexpeceted node {:?}", enode); }
	}
}

// generate IR Vector
pub fn gen_ir(program: &mut Program) {
	
	for funode in &mut program.nodes {
		
		let mut code = vec![];
		*REGNO.lock().unwrap() = 1;
		
		match &funode.op {
			NodeType::Func(ctype, name, args, body, stacksize) => {
				if ctype.is_extern {
					continue;
				}
				for i in 0..args.len() {
					store_arg(&args[i].ctype, args[i].offset, i, &mut code);
				}
				gen_stmt(body, &mut code);
				let func = Function::new(name.clone(), code, *stacksize);
				program.funs.push(func);
			}
			_ => { panic!(" should be func node at gen_ir: {:?}", funode); }
		}
	}
}