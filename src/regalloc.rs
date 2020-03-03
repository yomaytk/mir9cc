use super::ir::IrType::*;
use super::ir::*;

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
pub fn alloc_regs(reg_map: &mut [i32], used: &mut [bool], code: &mut Vec<Ir>) {
	for ir in code {
		// println!("ererere");
		match ir.ty {
			IrImm => {
				ir.lhs = alloc(reg_map, used, ir.lhs);
			},
			IrMov | IrPlus | IrMinus | IrMul | IrDiv => {
				ir.lhs = alloc(reg_map, used, ir.lhs);
				ir.rhs = alloc(reg_map, used, ir.rhs);
			},
			IrRet => {
				ir.lhs = alloc(reg_map, used, ir.lhs);
				kill(used, reg_map[ir.lhs]);
			},
			IrKill | IrExpr => {
				kill(used, reg_map[ir.lhs]);
				ir.ty = IrNop;
			},
			_ => { panic!("unknown operator."); }
		}
	}
}