use super::ir::IrType::*;
use super::ir::*;

static REG: [&str; 8] = ["rdi", "rsi", "r10", "r11", "r12", "r13", "r14", "r15"];
static REG_SIZE: usize = 8;

// allocate the register can be used
fn alloc(reg_map: &mut [i32], used: &mut [bool], ir_reg: usize) -> usize {
	
	if reg_map[ir_reg] != -1 {
		if !used[reg_map[ir_reg] as usize] { panic!("the register allocated is not used. at reg_map[{}]", ir_reg); }
		return reg_map[ir_reg] as usize;
	}

	let mut i: usize = 0;
	while i < REG_SIZE {
		if used[i] {
			i += 1;
			continue;
		}
		reg_map[ir_reg] = i as i32;
		used[i] = true;
		return i;
	}
	panic!("register exhausted.");
}

fn kill(used: &mut [bool], i: i32){
	let id: usize = i as usize;
	if !used[id] { panic!("cannot release the register not allocated."); }
	used[id] = false;
}

// do allocating register to reg_map 
pub fn alloc_regs(reg_map: &mut [i32], used: &mut [bool], ins: &mut Vec<Ir>) {
	for ir in ins {
		match ir.ty {
			IrImm => {
				ir.lhs = alloc(reg_map, used, ir.lhs);
			},
			IrMov | IrPlus | IrMinus => {
				ir.lhs = alloc(reg_map, used, ir.lhs);
				ir.rhs = alloc(reg_map, used, ir.rhs);
			},
			IrReturn => {
				kill(used, reg_map[ir.lhs]);
			},
			IrKill => {
				kill(used, reg_map[ir.lhs]);
				ir.ty = IrNop;
			},
			_ => { panic!("unknown operator."); }
		}
	}
}

pub fn gen_x86(ins: &Vec<Ir>) {
	for ir in ins {
		match ir.ty {
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
			IrReturn => {
				println!("\tmov rax, {}", REG[ir.lhs]);
				println!("\tret");
			},
			IrNop => {},
			_ => { panic!("unexpected IrType in gen_x86"); }
		}
	}
}