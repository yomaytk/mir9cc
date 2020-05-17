use super::gen_ir::{*, IrOp::*, IrType::*};
use super::mir::*;

// Register allocator.
//
// Before this pass, it is assumed that we have infinite number of
// registers. This pass maps them to a finite number of registers.
// We actually have only 7 registers.
//
// We allocate registers only within a single expression. In other
// words, there are no registers that live beyond semicolons.
// This design choice simplifies the implementation a lot, since
// practically we don't have to think about the case in which
// registers are exhausted and need to be spilled to memory.

static REG_SIZE: usize = 7;
static REGMAP_SZ: usize = 8192;

// allocate the register can be used
fn alloc(reg_map: &mut [i32], used: &mut [bool], ir_reg: usize) -> usize {
	
	if REGMAP_SZ < ir_reg {
		panic!("program too big.");
	}
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
			Binary => {
				match ir.op {
					IrAdd(is_imm) | IrSub(is_imm) | IrMul(is_imm) | IrXor(is_imm, _)=> {
						if is_imm {
							ir.lhs = alloc(reg_map, used, ir.lhs);
						} else {
							ir.lhs = alloc(reg_map, used, ir.lhs);
							ir.rhs = alloc(reg_map, used, ir.rhs);
						}
					}
					_ => { panic!("binary visit error."); }
				}
			}
			Reg | RegImm | RegLabel | LabelAddr => {
				ir.lhs = alloc(reg_map, used, ir.lhs);
			},
			RegReg | Mem => {
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
			Label | NoArg | Imm | ImmImm => {}
		}
		if ir.op == IrKill {
			assert!(used[ir.lhs]);
			used[ir.lhs] = false;
			ir.op = IrNop;
		}
	}
}

pub fn alloc_regs(program: &mut Program) {

	for fun in &mut program.funs {
		let mut reg_map: Vec<i32> = vec![-1; 8192];
		let mut used: Vec<bool> = vec![false; 8];
		visit(&mut reg_map, &mut used, &mut fun.irs);
	}
}