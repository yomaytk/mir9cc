use super::gen_ir::{*, IrOp::*, IrType::*};

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

// do allocating register to reg_map 
pub fn visit(reg_map: &mut Vec<i32>, used: &mut Vec<bool>, irs: &mut Vec<Ir>) {
	for ir in irs {
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
			Label | NoArg | Imm => {}
		}
		if ir.op == IrKill {
			assert!(used[ir.lhs]);
			used[ir.lhs] = false;
			ir.op = IrNop;
		}
	}
}

pub fn alloc_regs(funcs: &mut Vec<Function>) {

	for fun in funcs {
		let mut reg_map: Vec<i32> = vec![-1; 10000];
		let mut used: Vec<bool> = vec![false; 8];
		reg_map[0] = 0;
		used[0] = true;
		visit(&mut reg_map, &mut used, &mut fun.irs);
	}
}