use super::ir::{*, IrOp::*, IrType::*};

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

fn kill(used: &mut [bool], id: usize){
	if !used[id] { panic!("cannot release the register not allocated."); }
	used[id] = false;
}

// do allocating register to reg_map 
pub fn alloc_regs(reg_map: &mut [i32], used: &mut [bool], code: &mut Vec<Ir>) {
	for ir in code {
		let info = ir.get_irinfo();
		match info.ty {
			Reg | RegImm | RegLabel => {
				ir.lhs = alloc(reg_map, used, ir.lhs);
			},
			RegReg => {
				ir.lhs = alloc(reg_map, used, ir.lhs);
				ir.rhs = alloc(reg_map, used, ir.rhs);
			},
			Call => {
				match &mut ir.op {
					IrCall { name, len, args } => {
						let _name = name;
						ir.lhs = alloc(reg_map, used, ir.lhs);
						for i in 0..*len {
							args[i] = alloc(reg_map, used, args[i]);
						}
					},
					_ => { panic!("alloc_regs call error"); }
				}
			}
			Label | NoArg => {}
		}
		if ir.op == IrKill {
			kill(used, ir.lhs);
			ir.op = IrNop;
		}
	}
}