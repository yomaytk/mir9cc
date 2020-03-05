use super::ir::{*, IrType::*};

use std::sync::Mutex;

static REG: [&str; 8] = ["rdi", "rsi", "r10", "r11", "r12", "r13", "r14", "r15"];

lazy_static! {
	pub static ref LABEL: Mutex<usize> = Mutex::new(0);
}

pub fn gen_x86(code: &Vec<Ir>) {

	println!("\tpush rbp");
	println!("\tmov rbp, rsp");

	for ir in code {
		match &ir.ty {
			IrImm => {
				println!("\tmov {}, {}", REG[ir.lhs], ir.rhs);
			},
			IrMov => {
				println!("\tmov {}, {}", REG[ir.lhs], REG[ir.rhs]);
			}, 
			IrAdd(imm) => {
				match imm {
					Some(im) => {
						println!("\tadd {}, {}", REG[ir.lhs], im);
					},
					None => {
						println!("\tadd {}, {}", REG[ir.lhs], REG[ir.rhs]);
					}
				}
			},
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
				println!("\tjmp .L{}", *LABEL.lock().unwrap());
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
			IrNop => {},
			_ => { panic!("unexpected IrType in gen_x86"); }
		}
	}
	
	println!(".L{}:", *LABEL.lock().unwrap());
	println!("\tmov rsp, rbp");
	println!("\tmov rsp, rbp");
	println!("\tpop rbp");
	println!("\tret");
}