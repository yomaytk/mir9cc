use super::gen_ir::{*, IrOp::*};
use super::mir::*;
use std::rc::Rc;
use std::cell:: RefCell;

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

fn kill(reg_map: &mut [i32], used: &mut Vec<bool>, r: &mut Reg) {
	if r.rn < 0 { return; }
	let vn = r.vn as usize;
	let rn = r.rn as usize;
	assert!(used[rn]);
	reg_map[vn] = -2;
	used[rn] = false;
}

// in IR, A = B op C  ---> A = B; A = A op C;
fn three_two(bb: &Rc<RefCell<BB>>) {
	let irs = std::mem::replace(&mut bb.borrow_mut().irs, vec![]);
	let mut n_irs = vec![];
	for mut ir in irs {
		if ir.r0.vn < 0 || ir.r1.vn < 0 {
			n_irs.push(ir);
			continue;
		}
		assert!(ir.r0.vn != ir.r1.vn);
		// A = B;
		let mut ir1 = Ir::new(IrOp::IrMov, ir.r0.clone(), Reg::dummy(), ir.r1.clone(), Reg::dummy(), None, None, -1, -1);
		ir1.kills.push(ir.r1.clone());
		n_irs.push(ir1);
		// A = A op C;
		ir.r1 = ir.r0.clone();
		n_irs.push(ir);
	}
	// println!("{:#?}", &n_irs);
	bb.borrow_mut().irs = n_irs;
}

// allocate the register can be used
fn alloc(reg_map: &mut [i32], used: &mut [bool], r: &mut Reg) {
	
	let vn = r.vn as usize;

	if REGMAP_SZ < r.vn {
		eprintln!("program too big.");
		std::process::exit(0);
	}

	if r.vn < 0 { return; }
	
	if reg_map[vn] != -1 {
		if reg_map[vn] == -2 { return; }			// already killed
		if !used[reg_map[vn] as usize] { panic!("the register allocated is not used."); }
		r.rn = reg_map[vn];
		return;
	}

	let mut i: usize = 0;
	while i < REG_SIZE {
		if used[i] {
			i += 1;
			continue;
		}
		reg_map[vn] = i as i32;
		used[i] = true;
		r.rn = i as i32;
		return;
	}
	
	panic!("register exhausted.");
}

// do allocating register to reg_map 
fn visit(reg_map: &mut Vec<i32>, used: &mut Vec<bool>, ir: &mut Ir) {
	
	alloc(reg_map, used, &mut ir.r0);
	alloc(reg_map, used, &mut ir.r1);
	alloc(reg_map, used, &mut ir.r2);
	alloc(reg_map, used, &mut ir.bbarg);
	if let IrCall{ name, len, args } = &mut ir.op {
		let _name = name;
		for i in 0..*len {
			alloc(reg_map, used, &mut args[i]);
		}
	}

	for r in &mut ir.kills {
		alloc(reg_map, used, r);
		kill(reg_map, used, r);
	}
}

pub fn alloc_regs(program: &mut Program) {

	for fun in &mut program.funs {
		let mut reg_map: Vec<i32> = vec![-1; 8192];
		let mut used: Vec<bool> = vec![false; 8];
		for bb in &mut fun.bbs {
			three_two(&bb);
			let vn = bb.borrow().param.vn;
			if vn > 0 {
				alloc(&mut reg_map, &mut used, &mut bb.borrow_mut().param);
			}
			for ir in &mut bb.borrow_mut().irs {
				visit(&mut reg_map, &mut used, ir);
			}
		}
	}
}