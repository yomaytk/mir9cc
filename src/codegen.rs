use super::ir::{*, IrOp::*};

static REG: [&str; 8] = ["rdi", "rsi", "r10", "r11", "r12", "r13", "r14", "r15"];

pub fn gen_x86(code: &Vec<Ir>) {

	println!("\tpush rbp");
	println!("\tmov rbp, rsp");

	let ret = ".Lend";

	for ir in code {
		match &ir.op {
			IrImm => {
				println!("\tmov {}, {}", REG[ir.lhs], ir.rhs);
			},
			IrMov => {
				println!("\tmov {}, {}", REG[ir.lhs], REG[ir.rhs]);
			}, 
			IrAdd => {
				println!("\tadd {}, {}", REG[ir.lhs], REG[ir.rhs]);
			},
			IrAddImm => {
				println!("\tadd {}, {}", REG[ir.lhs], ir.rhs);
			}
			IrSub => {
				println!("\tsub {}, {}", REG[ir.lhs], REG[ir.rhs]);
			},
			IrMul => {
				println!("\tmov rax, {}", REG[ir.rhs]);
				println!("\tmul {}", REG[ir.lhs]);
				println!("\tmov {}, rax", REG[ir.lhs]);
			},
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
			},
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
			IrNop => {},
			_ => { panic!("unexpected IrOp in gen_x86"); }
		}
	}
	
	println!("{}:", ret);
	println!("\tmov rsp, rbp");
	println!("\tmov rsp, rbp");
	println!("\tpop rbp");
	println!("\tret");
}