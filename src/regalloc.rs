use super::gen_ir::{*, IrOp::*};
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
static REGMAP_SZ: i32 = 8192;

fn kill(used: &mut Vec<bool>, r: usize) {
	assert!(used[r]);
	used[r] = false;
}

// allocate the register can be used
fn alloc(reg_map: &mut [i32], used: &mut [bool], ir_reg: i32) -> i32 {
	
	if REGMAP_SZ < ir_reg {
		eprintln!("program too big.");
		std::process::exit(0);
	}
	if reg_map[ir_reg as usize] != -1 {
		if !used[reg_map[ir_reg as usize] as usize] { panic!("the register allocated is not used. at reg_map[{}]", ir_reg); }
		return reg_map[ir_reg as usize];
	}

	let mut i: usize = 0;
	while i < REG_SIZE {
		if used[i] {
			i += 1;
			continue;
		}
		reg_map[ir_reg as usize] = i as i32;
		used[i] = true;
		return i as i32;
	}
	panic!("register exhausted.");
}

// do allocating register to reg_map 
pub fn visit(reg_map: &mut Vec<i32>, used: &mut Vec<bool>, ir: &mut Ir) {
	if ir.lhs > 0 {
		ir.lhs = alloc(reg_map, used, ir.lhs);
	}
	if ir.rhs > 0 {
		ir.rhs = alloc(reg_map, used, ir.rhs);
	}
	if let IrCall{ name, len, args } = &mut ir.op {
		let _name = name;
		for i in 0..*len {
			args[i] = alloc(reg_map, used, args[i]);
		}
	}
	for r in &ir.kills {
		let lhs = alloc(reg_map, used, *r);
		kill(used, lhs as usize);
	}
}

pub fn alloc_regs(program: &mut Program) {

	for fun in &mut program.funs {
		let mut reg_map: Vec<i32> = vec![-1; 8192];
		let mut used: Vec<bool> = vec![false; 8];
		for bb in &mut fun.bbs {
			for ir in &mut bb.irs {
				visit(&mut reg_map, &mut used, ir);
			}
		}
	}
}