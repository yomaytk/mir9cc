use super::ir::{*, IrType::*};

static REG: [&str; 8] = ["rdi", "rsi", "r10", "r11", "r12", "r13", "r14", "r15"];

pub fn gen_x86(ins: &Vec<Ir>) {
	for ir in ins {
		match &ir.ty {
			IrImm => {
				println!("\tmov {}, {}", REG[ir.lhs], ir.rhs);
			},
			IrMov => {
				println!("\tmov {}, {}", REG[ir.lhs], REG[ir.rhs]);
			}, 
			IrPlus => {
				println!("\tadd {}, {}", REG[ir.lhs], REG[ir.rhs]);
			},
			IrMinus => {
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
			IrReturn => {
				println!("\tmov rax, {}", REG[ir.lhs]);
				println!("\tret");
			},
			IrNop => {},
			_ => { panic!("unexpected IrType in gen_x86"); }
		}
	}
}