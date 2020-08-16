use super::token::{*, TokenType::*};
use IrOp::*;
use super::parse::*;
// use super::lib::*;
use super::mir::*;

use std::sync::Mutex;

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
	pub static ref REGNO: Mutex<i32> = Mutex::new(1);
	pub static ref CONTINUE_VEC: Mutex<Vec<i32>> = Mutex::new(vec![]);
	pub static ref BREAK_VEC: Mutex<Vec<i32>> = Mutex::new(vec![]);
	pub static ref SWITCHES: Mutex<Vec<Vec<BB>>> = Mutex::new(vec![]);
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, std::cmp::Eq, std::hash::Hash)]
pub enum IrOp {
	IrImm,
	IrMov,
	IrAdd,
	IrBpRel,
	IrSub,
	IrMul,
	IrDiv,
	IrRet,
	IrStore(i32),
	IrLoad(i32),
	IrJmp,
	IrCall { name: String, len: usize, args: Vec<i32> },
	IrStoreArg(i32),
	IrLt,
	IrEqual, 
	IrNe,
	IrLabelAddr(String),
	IrOr,
	IrXor,
	IrAnd,
	IrLe,
	IrShl,
	IrShr,
	IrMod,
	IrNeg,
	IrBr,
}

#[derive(Debug)]
pub struct Ir {
	pub op: IrOp,
	pub r0: i32,
	pub r2: i32,
	pub bb1_label: i32,
	pub bb2_label: i32,
	pub imm: i32,
	pub imm2: i32,
	pub kills: Vec<i32>		// For liveness tracking
}

impl Ir {
	pub fn new(ty: IrOp, lhs: i32, rhs: i32, bb1_label: i32, bb2_label: i32, imm: i32, imm2: i32) -> Self {
		Self {
			op: ty,
			r0: lhs,
			r2: rhs,
			bb1_label,
			bb2_label,
			imm, 
			imm2,
			kills: vec![],
		}
	}
	fn bittype(ty: &TokenType) -> IrOp {
		match ty {
			TokenAdd => { IrAdd },
			TokenSub => { IrSub },
			TokenStar => { IrMul },
			TokenDiv => { IrDiv },
			TokenLt => { IrLt },
			TokenLe => { IrLe },
			TokenShl => { IrShl },
			TokenShr => { IrShr },
			TokenMod => { IrMod },
			TokenAmpersand => { IrAnd },
			TokenOr => { IrOr },
			TokenXor => { IrXor },
			TokenEof => { panic!("tokeneof!!!"); }
			_ => { panic!("bittype error."); }
		}
	}
	pub fn tostr(&self) -> String {
		match &self.op {
			IrImm => { return format!("Imm r{}, {}", self.r0, self.imm); }
			IrMov => { return format!("Mov r{}, r{}", self.r0, self.r2); }
			IrAdd => { return format!("Add r{}, r{}", self.r0, self.r2); }
			IrBpRel => { return format!("Lea r{}, [rbp-{}]", self.r0, self.imm); }
			IrSub => { return format!("Sub r{}, r{}", self.r0, self.r2); }
			IrMul => { return format!("Mul r{}, r{}", self.r0, self.r2); }
			IrDiv => { return format!("Div r{}, r{}", self.r0, self.r2); }
			IrRet => { return format!("Return {}", self.r0); }
			IrStore(ir_size) => { return format!("Store{} [r{}], r{}", ir_size, self.r0, self.r2); }
			IrLoad(ir_size) => { return format!("Load{} r{}, [r{}]", ir_size, self.r0, self.r2); }
			IrJmp => { return format!("Jmp .L{}", self.imm); }
			IrCall { name, len, args } => { 
				let _len = len;
				let mut call_s = format!("Call {}(", name);
				for arg in args {
					call_s.push_str(&format!("r{}, ", arg));
				}
				if call_s.len() > 0 {
					call_s.pop();
					call_s.pop();
				}
				call_s.push_str(")");
				return call_s; 
			}
			IrStoreArg(ir_size) => { return format!("STORE_ARG{}, {}, {}", ir_size, self.imm, self.imm2); }
			IrLt => { return format!("Lt r{}, r{}", self.r0, self.r2); }
			IrEqual => { return format!("Equal r{}, r{}", self.r0, self.r2); }
			IrNe => { return format!("Ne r{}, r{}", self.r0, self.r2); }
			IrLabelAddr(labelname) => { return format!("Label_Addr r{}, .L{}", self.r0, labelname); }
			IrOr => { return format!("Or r{}, r{}", self.r0, self.r2); }
			IrXor => { return format!("Xor r{}, r{}", self.r0, self.r2); }
			IrAnd => { return format!("And r{}, r{}", self.r0, self.r2); }
			IrLe => { return format!("Le r{}, r{}", self.r0, self.r2); }
			IrShl => { return format!("Shl r{}, r{}", self.r0, self.r2); }
			IrShr => { return format!("Shr r{}, r{}", self.r0, self.r2); }
			IrMod => { return format!("Mod r{}, r{}", self.r0, self.r2); }
			IrNeg => { return format!("Neg r{}", self.r0); }
			IrBr => { return format!("Br r{}, .L{}, .L{}", self.r0, self.bb1_label, self.bb2_label); }
		}
	}
	fn emit(op: IrOp, lhs: i32, rhs: i32, fun: &mut Function) {
		fun.bbs.last_mut().unwrap().irs.push(Ir::new(op, lhs, rhs, -1, -1, -1, -1));
	}
	fn bb_emit(op: IrOp, lhs: i32, rhs: i32, bb1_label: i32, bb2_label: i32, fun: &mut Function) {
		fun.bbs.last_mut().unwrap().irs.push(Ir::new(op, lhs, rhs, bb1_label, bb2_label, -1, -1));
	}
	fn br(r: i32, then_label: i32, els_label: i32, fun: &mut Function) {
		Ir::bb_emit(IrBr, r, -1, then_label, els_label, fun);
	}
	fn imm_emit(op: IrOp, lhs: i32, imm: i32, imm2: i32, fun: &mut Function) {
		fun.bbs.last_mut().unwrap().irs.push(Ir::new(op, lhs, -1, -1, -1, imm, imm2));
	}
}

pub struct Function {
	pub name: String,
	pub bbs: Vec<BB>,
	pub stacksize: i32,
}

impl Function {
	pub fn new(name: String, bbs: Vec<BB>, stacksize: i32) -> Self {
		Self {
			name,
			bbs,
			stacksize,
		}
	}
	pub fn bb_push(&mut self, bb: BB) {
		self.bbs.push(bb);
	}
}

fn kill(r: i32, fun: &mut Function) {
	if let Some(_) = fun.bbs.last() {
		fun.bbs.last_mut().unwrap().irs.last_mut().unwrap().kills.push(r);
	} else {
		panic!("kill fun error.");
	}
}

fn jmp(x: i32, fun: &mut Function) {
	Ir::bb_emit(IrJmp, -1, -1, x, -1, fun);
}

fn load(ctype: &Type, dst: i32, src: i32, fun: &mut Function) {
	Ir::emit(IrOp::IrLoad(ctype.size), dst, src, fun);
}

fn store(ctype: &Type, dst: i32, src: i32, fun: &mut Function) {
	Ir::emit(IrOp::IrStore(ctype.size), dst, src, fun);
}

fn store_arg(ctype: &Type, offset: i32, id: i32, fun: &mut Function) {
	Ir::imm_emit(IrOp::IrStoreArg(ctype.size), -1, offset, id, fun);
}

fn gen_binop(irop: IrOp, lhs: &Node, rhs: &Node, fun: &mut Function) -> i32 {
	let r1 = gen_expr(lhs, fun);
	let r2 = gen_expr(rhs, fun);
	Ir::emit(irop, r1, r2, fun);
	kill(r2, fun);
	return r1;
}

fn gen_inc_scale(ctype: &Type) -> i32 {
	match ctype.ty {
		Ty::PTR => { return ctype.ptr_to.as_ref().unwrap().size; }
		_ => { return 1; }
	}
}

fn imm(op: IrOp, r: i32, imm: i32, imm2: i32, fun: &mut Function) {
	Ir::imm_emit(op, r, imm, imm2, fun);
}

fn gen_pre_inc(ctype: &Type, lhs: &Node, fun: &mut Function, num: i32) -> i32 {
	let r1 = gen_lval(lhs, fun);
	let r2 = new_regno();
	load(ctype, r2, r1, fun);
	let r3 = new_regno();
	imm(IrImm, r3, num * gen_inc_scale(ctype), -1, fun);
	Ir::emit(IrAdd, r2, r3, fun);
	kill(r3, fun);
	store(ctype, r1, r2, fun);
	kill(r1, fun);
	return r2;
}

fn gen_post_inc(ctype: &Type, lhs: &Node, fun: &mut Function, num: i32) -> i32 {
	let r = gen_pre_inc(ctype, lhs, fun, num);
	let r2 = new_regno();
	imm(IrImm, r2, num * gen_inc_scale(ctype), -1, fun);
	Ir::emit(IrSub, r, r2, fun);
	kill(r2, fun);
	return r;
}

fn new_regno() -> i32 {
	*REGNO.lock().unwrap() += 1;
	return *REGNO.lock().unwrap();
}

fn get_break_label() -> i32 {
	if let Some(break_label) = BREAK_VEC.lock().unwrap().last() {
		return *break_label;
	} else {
		eprintln!("cannot find jmp point of break.");
		std::process::exit(0);
	}
}

fn get_continue_label() -> i32 {
	if let Some(continue_label) = CONTINUE_VEC.lock().unwrap().last() {
		return *continue_label;
	} else {
		eprintln!("cannot find jmp point of break.");
		std::process::exit(0);
	}
}

fn loop_inc(continue_label: i32, break_label: i32) {
	CONTINUE_VEC.lock().unwrap().push(continue_label);
	BREAK_VEC.lock().unwrap().push(break_label);
}

fn loop_dec() {
	if let None = BREAK_VEC.lock().unwrap().pop() {
		eprintln!("cannot find jmp point of break.");
		std::process::exit(0);
	}
	if let None = CONTINUE_VEC.lock().unwrap().pop() {
		eprintln!("cannot find jmp point of continue.");
		std::process::exit(0);
	}
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

fn gen_lval(node: &Node, fun: &mut Function) -> i32 {
	
	match &node.op {
		NodeType::Deref(_, expr) => {
			return gen_expr(expr, fun);
		}
		NodeType::VarRef(var) => {
			let r = new_regno();
			if var.is_local {
				imm(IrBpRel, r, var.offset, -1, fun);
			} else {
				Ir::emit(IrLabelAddr(var.labelname.clone().unwrap()), r, -1, fun);
			}
			return r;
		}
		NodeType::Dot(ctype, expr, _) => {
			let r1 = gen_lval(expr, fun);
			let r2 = new_regno();
			imm(IrImm, r2, ctype.offset, -1, fun);
			Ir::emit(IrAdd, r1, r2, fun);
			kill(r2, fun);
			return r1;
		}
		_ => { panic!("not an lvalue")}
	}
}

// allocate of index for register to NodeNum
fn gen_expr(node: &Node, fun: &mut Function) -> i32 {

	match &node.op {
		NodeType::Num(val) => {
			let r = new_regno();
			imm(IrImm, r, *val, -1, fun);
			return r;
		},
		NodeType::BinaryTree(_, ty, lhs, rhs) => {
			match ty {
				// a && b
				TokenLogAnd => {
					let bb1 = BB::new();
					let bb2 = BB::new();
					let last = BB::new();

					let r = gen_expr(lhs, fun);
					Ir::br(r, bb1.label, last.label, fun);

					fun.bb_push(bb1);
					let r1 = gen_expr(rhs, fun);
					Ir::emit(IrMov, r, r1, fun);
					kill(r1, fun);
					Ir::br(r, bb2.label, last.label, fun);

					fun.bb_push(bb2);
					imm(IrImm, r, 1, -1, fun);
					jmp(last.label, fun);

					fun.bb_push(last);

					return r;
				}
				// a || b
				TokenLogOr => {
					let bb1 = BB::new();
					let bb2 = BB::new();
					let last = BB::new();

					let r = gen_expr(lhs, fun);
					Ir::br(r, bb2.label, bb1.label, fun);

					fun.bb_push(bb1);
					let r1 = gen_expr(rhs, fun);
					Ir::emit(IrMov, r, r1, fun);
					kill(r1, fun);
					Ir::br(r, bb2.label, last.label, fun);

					fun.bb_push(bb2);
					imm(IrImm, r, 1, -1, fun);
					jmp(last.label, fun);

					fun.bb_push(last);

					return r;
				}
				_ => {
				// a R b (R != &&, ||)
					return gen_binop(Ir::bittype(ty), lhs, rhs, fun);
				}
			}
		},
		// a
		NodeType::VarRef(var) => {
			let lhi = gen_lval(node, fun);
			load(&var.ctype, lhi, lhi, fun);
			return lhi;
		}
		// a.b (struct member)
		NodeType::Dot(ctype, ..) => {
			let lhi = gen_lval(node, fun);
			load(ctype, lhi, lhi, fun);
			return lhi;
		},
		// a = b
		NodeType::Assign(ctype, lhs, rhs) => {
			let rhi = gen_expr(rhs, fun);
			let lhi = gen_lval(lhs, fun);
			store(ctype, lhi, rhi, fun);
			kill(lhi, fun);
			return rhi;
		},
		// fun(...)
		NodeType::Call(_, ident, callarg) => {
			let mut args = vec![];
			for arg in callarg {
				args.push(gen_expr(arg, fun));
			}
			let r = new_regno();
			Ir::emit(
				IrCall{ 
					name: (*ident).clone(), 
					len: args.len(),
					args: args.clone()
				}, 
				r, 
				-1, 
				fun
			);
			for arg in args {
				kill(arg, fun);
			}
			return r;
		}
		// *a
		NodeType::Deref(_, lhs) => {
			let r = gen_expr(lhs, fun);
			load(lhs.nodesctype(None).ptr_to.unwrap().as_ref(), r, r, fun);
			return r;
		}
		// &a
		NodeType::Addr(_, lhs) => {
			return gen_lval(lhs, fun);
		}
		// a == b
		NodeType::Equal(lhs, rhs) => {
			return gen_binop(IrEqual, lhs, rhs, fun);
		}
		// a != b
		NodeType::Ne(lhs, rhs) => {
			return gen_binop(IrNe, lhs, rhs, fun);
		}
		// !a
		NodeType::Not(expr) => {
			let r1 = gen_expr(expr, fun);
			let r2 = new_regno();
			imm(IrImm, r2, 0, -1, fun);
			Ir::emit(IrEqual, r1, r2, fun);
			kill(r2, fun);
			return r1;
		}
		// a ? b : c
		NodeType::Ternary(_, cond, then, els) => {
			let bb1 = BB::new();
			let bb2 = BB::new();
			let last = BB::new();

			let r = gen_expr(cond, fun);
			Ir::br(r, bb1.label, bb2.label, fun);

			fun.bb_push(bb1);
			let r1 = gen_expr(then, fun);
			Ir::emit(IrMov, r, r1, fun);
			kill(r1, fun);
			jmp(last.label, fun);

			fun.bb_push(bb2);
			let r2 = gen_expr(els, fun);
			Ir::emit(IrMov, r, r2, fun);
			kill(r2, fun);
			jmp(last.label, fun);

			fun.bb_push(last);

			return r;
		}
		// (a, b)
		NodeType::TupleExpr(_, lhs, rhs) => {
			kill(gen_expr(lhs, fun), fun);
			return gen_expr(rhs, fun);
		}
		// ++a, a++
		NodeType::IncDec(ctype, selector, lhs) => {
			if *selector == 1 { return gen_post_inc(ctype, lhs, fun, 1); }
			else { return gen_post_inc(ctype, lhs, fun, -1); }
		}
		// _Bool x = 2; -> x == 1;
		NodeType::Cast(ctype, expr) => {
			let r1 = gen_expr(expr, fun);
			if ctype.ty != Ty::BOOL {
				return r1;
			}
			let r2 = new_regno();
			imm(IrImm, r2, 0, -1, fun);
			Ir::emit(IrNe, r1, r2, fun);
			kill(r2, fun);
			return r1;
		}
		NodeType::StmtExpr(_, body) => {
			if let NodeType::CompStmt(stmts) = &body.op {
				let len = stmts.len();
				for i in 0..len-1 {
					gen_stmt(&stmts[i], fun);
				}
				if len > 0 {
					if let NodeType::Expr(ref expr) = stmts.last().unwrap().op {
						return gen_expr(expr, fun);
					}
				}
			}
			let r = new_regno();
			imm(IrImm, r, 0, -1, fun);
			return r;
		}
		_ => { panic!("gen_expr NodeType error at {:?}", node.op); }
	}

}


fn gen_stmt(node: &Node, fun: &mut Function) {
	match &node.op {
		NodeType::NULL => { 
			return; 
		}
		NodeType::Ret(lhs) => {
			
			let lhi = gen_expr(lhs.as_ref(), fun);
			Ir::emit(IrRet, lhi, 0, fun);
			kill(lhi, fun);

			let bb = BB::new();
			jmp(bb.label, fun);
			fun.bb_push(bb);

			return;
		}
		NodeType::Expr(lhs) => {
			kill(gen_expr(lhs.as_ref(), fun), fun);
			return;
		}
		NodeType::IfThen(cond, then, els) => {
			let bb_then = BB::new();
			let bb_els = BB::new();
			let last = BB::new();

			let r = gen_expr(cond, fun);
			Ir::br(r, bb_then.label, bb_els.label, fun);
			kill(r, fun);

			fun.bb_push(bb_then);
			gen_stmt(then, fun);
			jmp(last.label, fun);

			fun.bb_push(bb_els);
			if let Some(elsth) = els {
				gen_stmt(elsth, fun);
			}
			jmp(last.label, fun);

			fun.bb_push(last);

			return;
		}
		NodeType::CompStmt(lhs) => {
			for stmt in lhs {
				gen_stmt(stmt, fun);
			}
			return;
		}
		NodeType::For(init, cond, inc, body) => {
			let cond_bb = BB::new();
			let body_bb = BB::new();
			let continue_bb = BB::new();
			let break_bb = BB::new();
			loop_inc(continue_bb.label, break_bb.label);
			
			let cond_bb_label = cond_bb.label;
			
			gen_stmt(init, fun);
			
			fun.bb_push(cond_bb);
			match cond.op {
				NodeType::NULL => {}
				_ => {
					let r = gen_expr(cond, fun);
					Ir::br(r, body_bb.label, break_bb.label, fun);
					kill(r, fun);
				}
			}
			jmp(body_bb.label, fun);

			fun.bb_push(body_bb);
			gen_stmt(body, fun);
			jmp(continue_bb.label, fun);

			fun.bb_push(continue_bb);
			gen_stmt(inc, fun);
			jmp(cond_bb_label, fun);

			fun.bb_push(break_bb);

			loop_dec();

			return;
		}
		NodeType::DoWhile(body, cond) => {
			let body_bb = BB::new();
			let continue_bb = BB::new();
			let break_bb = BB::new();
			
			loop_inc(continue_bb.label, break_bb.label);
			let body_bb_label = body_bb.label;

			jmp(body_bb.label, fun);

			fun.bb_push(body_bb);
			gen_stmt(body, fun);
			jmp(continue_bb.label, fun);

			fun.bb_push(continue_bb);
			let r = gen_expr(cond, fun);
			Ir::br(r, body_bb_label, break_bb.label, fun);
			kill(r, fun);

			fun.bb_push(break_bb);

			loop_dec();

			return;

		}
		NodeType::Switch(cond, body, case_conds) => {
			let continue_bb = BB::new();
			let break_bb = BB::new();
			loop_inc(continue_bb.label, break_bb.label);
			SWITCHES.lock().unwrap().push(vec![]);

			let r = gen_expr(cond, fun);
			
			for val in case_conds {
				let case_bb = BB::new();
				let next_bb = BB::new();

				let x = gen_expr(val, fun);
				Ir::emit(IrEqual, x, r, fun);
				Ir::br(x, case_bb.label, next_bb.label, fun);
				kill(x, fun);
				
				fun.bb_push(next_bb);
				SWITCHES.lock().unwrap().last_mut().unwrap().push(case_bb);
			}
			SWITCHES.lock().unwrap().last_mut().unwrap().reverse();
			jmp(break_bb.label, fun);
			kill(r, fun);
			gen_stmt(body, fun);
			
			fun.bb_push(break_bb);

			loop_dec();

			return;
		}
		NodeType::Case(_, body) => {
			if let Some(case_bb) = SWITCHES.lock().unwrap().last_mut().unwrap().pop() {
				jmp(case_bb.label, fun);
				fun.bb_push(case_bb);
				gen_stmt(body, fun);
			} else {
				panic!("gen_ir Case error.");
			}
		}
		NodeType::Break => {
			jmp(get_break_label(), fun);
		}
		NodeType::Continue => {
			jmp(get_continue_label(), fun);
		}
		enode => { panic!("unexpeceted node {:?}", enode); }
	}
}

// generate IR Vector
pub fn gen_ir(program: &mut Program) {
	
	*REGNO.lock().unwrap() = 1;

	for funode in &mut program.nodes {
		
		match &funode.op {
			NodeType::Func(_, name, args, body, stacksize) => {
				let mut fun = Function::new(name.clone(), vec![BB::new()], *stacksize);
				for i in 0..args.len() {
					store_arg(&args[i].ctype, args[i].offset, i as i32, &mut fun);
				}
				gen_stmt(body, &mut fun);
				program.funs.push(fun);
			}
			_ => { panic!(" should be func node at gen_ir: {:?}", funode); }
		}
	}
}