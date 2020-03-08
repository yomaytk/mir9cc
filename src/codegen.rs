use super::ir::{*, IrOp::*};

static REG: [&str; 7] = ["r10", "r11", "rbx", "r12", "r13", "r14", "r15"];

pub fn gen(fun: &Function, label: usize) {

	println!(".global {}", fun.name);
	println!("{}:", fun.name);
	println!("\tpush r12");
	println!("\tpush r13");
	println!("\tpush r14");
	println!("\tpush r15");
	println!("\tpush rbp");
	println!("\tmov rbp, rsp");

	let ret = format!(".Lend{}", label);

	for ir in &fun.irs {
		match &ir.op {
			IrImm => {
				println!("\tmov {}, {}", REG[ir.lhs], ir.rhs);
			}
			IrMov => {
				println!("\tmov {}, {}", REG[ir.lhs], REG[ir.rhs]);
			}
			IrAdd => {
				println!("\tadd {}, {}", REG[ir.lhs], REG[ir.rhs]);
			}
			IrAddImm => {
				println!("\tadd {}, {}", REG[ir.lhs], ir.rhs);
			}
			IrSub => {
				println!("\tsub {}, {}", REG[ir.lhs], REG[ir.rhs]);
			}
			IrMul => {
				println!("\tmov rax, {}", REG[ir.rhs]);
				println!("\tmul {}", REG[ir.lhs]);
				println!("\tmov {}, rax", REG[ir.lhs]);
			}
			IrDiv => {
				println!("\tmov rax, {}", REG[ir.lhs]);
				println!("\tcqo");
				println!("\tdiv {}", REG[ir.rhs]);
				println!("\tmov {}, rax", REG[ir.lhs]);
			}
			IrRet => {
				*LABEL.lock().unwrap() += 1;
				println!("\tmov rax, {}", REG[ir.lhs]);
				println!("\tjmp {}", ret);
			}
			IrAlloc => {
				println!("\tsub rsp, {}", ir.rhs);
				println!("\tmov {}, rsp", REG[ir.lhs]);
			}
			IrStore => {
				println!("\tmov [{}], {}", REG[ir.lhs], REG[ir.rhs]);
			}
			IrLoad => {
				println!("\tmov {}, [{}]", REG[ir.lhs], REG[ir.rhs]);
			}
			IrUnless => {
				println!("\tcmp {}, 0", REG[ir.lhs]);
				println!("\tje .L{}", ir.rhs);
			}
			IrLabel => {
				println!(".L{}:", ir.lhs);
			}
			IrJmp => {
				println!("\tjmp .L{}", ir.lhs);
			}
			IrCall { name, len , args } => {

				let callarg = vec!["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
				for i in 0..*len {
					println!("\tmov {}, {}", callarg[i], REG[args[i]]);
				}
				println!("\tpush r10");
				println!("\tpush r11");
				println!("\tmov rax, 0");
				println!("\tcall {}", name);
				println!("\tpop r11");
				println!("\tpop r10");
				println!("\tmov {}, rax", REG[ir.lhs]);
			}
			IrNop => {},
			_ => { panic!("unexpected IrOp in gen_x86"); }
		}
	}
	
	println!("{}:", ret);
	println!("\tmov rsp, rbp");
	println!("\tpop rbp");
	println!("\tpop r15");
	println!("\tpop r14");
	println!("\tpop r13");
	println!("\tpop r12");
	println!("\tret");
}

pub fn gen_x86(funcs: &Vec<Function>) {
	
    println!(".intel_syntax noprefix");

	for i in 0..funcs.len() {
		gen(&funcs[i], i);
	}
}