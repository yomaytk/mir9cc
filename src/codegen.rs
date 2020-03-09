use super::ir::{*, IrOp::*};

pub static REG: [&str; 8] = ["rbp", "r10", "r11", "rbx", "r12", "r13", "r14", "r15"];
static ARGREG: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

pub fn gen(fun: &Function, label: usize) {

	println!(".global {}", fun.name);
	println!("{}:", fun.name);
	println!("\tpush rbp");
	println!("\tmov rbp, rsp");
	println!("\tsub rsp, {}", fun.stacksize);
	println!("\tpush r12");
	println!("\tpush r13");
	println!("\tpush r14");
	println!("\tpush r15");

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
			IrSub => {
				println!("\tsub {}, {}", REG[ir.lhs], REG[ir.rhs]);
			}
			IrSubImm => {
				println!("\tsub {}, {}", REG[ir.lhs], ir.rhs);
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

				for i in 0..*len {
					println!("\tmov {}, {}", ARGREG[i], REG[args[i]]);
				}
				
				println!("\tpush r10");
				println!("\tpush r11");
				println!("\tmov rax, 0");
				println!("\tcall {}", name);
				println!("\tpop r11");
				println!("\tpop r10");
				
				println!("\tmov {}, rax", REG[ir.lhs]);
			}
			IrSaveArgs => {
				let len = ir.lhs;
				for i in 0..len {
					println!("\tmov [rbp-{}], {}", (i+1)*8, ARGREG[i]);
				}
			}
			IrNop => {},
			_ => { panic!("unexpected IrOp in gen_x86"); }
		}
	}
	
	println!("{}:", ret);
	println!("\tpop r15");
	println!("\tpop r14");
	println!("\tpop r13");
	println!("\tpop r12");
	println!("\tmov rsp, rbp");
	println!("\tpop rbp");
	println!("\tret");
}

pub fn gen_x86(funcs: &Vec<Function>) {
	
    println!(".intel_syntax noprefix");

	for i in 0..funcs.len() {
		gen(&funcs[i], i);
	}
}