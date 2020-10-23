// Linear scan register allocator.
//
// Before this pass, it is assumed that we have infinite number of
// registers. This pass maps them to finite number of registers.
// Here is the algorithm:
//
// First, we find the definition and the last use for each register.
// A register is considered "live" in the range. At the definition of
// some register R, if all physical registers are already allocated,
// one of them (including R itself) needs to be spilled to the stack.
// As long as one register is spilled, the algorithm is logically
// correct. As a heuristic, we spill a register whose last use is
// furthest.
//
// We then insert load and store instructions for spilled registesr.
// The last register (num_regs-1'th register) is reserved for that
// purpose.

use super::gen_ir::{*, IrOp::*};
use super::parse::{roundup};
use super::mir::*;
use std::rc::Rc;
use std::cell:: RefCell;
use std::collections::HashMap;
use linked_hash_map::LinkedHashMap;

static REG_SIZE: usize = 7;

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

// decide lifespan for all Reg
fn regs_life(fun: &mut Function) -> Vec<(i32, i32, i32)> {

	let mut ic = 1;
	let mut borned_map = LinkedHashMap::new();
	let mut died_map = HashMap::new();

	for bb in &fun.bbs {
		let param_vn = bb.borrow().param.vn;
		if param_vn > 0 {
			borned_map.insert(param_vn, ic);
			died_map.insert(param_vn, ic);
		}
		for ir in &bb.borrow_mut().irs {
			if !borned_map.contains_key(&ir.r0.vn) { 
				borned_map.insert(ir.r0.vn, ic); 
				died_map.insert(ir.r0.vn, ic);
			}
			if ir.r1.vn > 0 { died_map.insert(ir.r1.vn, ic); }
			if ir.r2.vn > 0 { died_map.insert(ir.r2.vn, ic); }
			if ir.bbarg.vn > 0 { died_map.insert(ir.bbarg.vn, ic); }
			if let IrCall(_, args) = &ir.op {
				for i in 0..args.len() {
					died_map.insert(args[i].vn, ic);
				}
			}
			ic += 1;
		}
	}
	let mut life_vec = vec![];
	for (vn, start) in borned_map {
		life_vec.push((vn, start, died_map[&vn]));
	}
	return life_vec;
}

// settings of register
fn regs_setting(fun: &mut Function, life_vec: Vec<(i32, i32, i32)>) {
	
	// save register as vn
	let mut regs = vec![-1;REG_SIZE];

	let mut stacksize = roundup(fun.stacksize, 8);

	let mut end_hash = HashMap::new();	// key -> vn; value -> end
	let mut reg_map = HashMap::new();
	let mut spill_offset_map = HashMap::new();

	// decide various datas of registers
	for life in life_vec {
		let (vn, start, end) = life;
		let mut rn = -1;
		end_hash.insert(vn, end);
		let mut found = false;
		// find an unused real register
		for i in 0..REG_SIZE-1 {
			if regs[i] < 0 || start > end_hash[&regs[i]] {
				rn = i as i32;
				regs[i] = vn;
				found = true;
				break;
			}
		}
		if !found {
			// choose to spill out
			let mut spill_id = 0;
			regs[REG_SIZE-1] = vn;
			for i in 0..regs.len() {
				if end_hash[&regs[spill_id]] < end_hash[&regs[i]] {
					spill_id = i;
				}
			}
			// allocate register spilled out to stack
			stacksize += 8;
			spill_offset_map.insert(regs[spill_id], stacksize);
			reg_map.insert(regs[spill_id], REG_SIZE as i32 - 1);
			rn = spill_id as i32;
			regs[spill_id] = vn;
		}
		reg_map.insert(vn, rn);
	}
	fun.stacksize = stacksize;
	// settings
	for bb in &mut fun.bbs {
		let param = bb.borrow().param.vn > 0;
		if param {
			let vn = bb.borrow().param.vn;
			bb.borrow_mut().param.rn = reg_map[&vn];
			bb.borrow_mut().param.spill = reg_map[&vn] == REG_SIZE as i32 - 1;
			let param_spill = bb.borrow().param.spill;
			if param_spill { bb.borrow_mut().param.spill_offset = spill_offset_map[&vn]; }
		}
		for ir in &mut bb.borrow_mut().irs {
			if ir.r0.vn > 0 {
				ir.r0.rn = reg_map[&ir.r0.vn];
				ir.r0.spill = ir.r0.rn == REG_SIZE as i32 - 1;
				if ir.r0.spill { ir.r0.spill_offset = spill_offset_map[&ir.r0.vn]; }
			}
			if ir.r1.vn > 0 {
				ir.r1.rn = reg_map[&ir.r1.vn];
				ir.r1.spill = ir.r1.rn == REG_SIZE as i32 - 1;
				if ir.r1.spill { ir.r1.spill_offset = spill_offset_map[&ir.r1.vn]; }
			}
			if ir.r2.vn > 0 {
				ir.r2.rn = reg_map[&ir.r2.vn];
				ir.r2.spill = ir.r2.rn == REG_SIZE as i32 - 1;
				if ir.r2.spill { ir.r2.spill_offset = spill_offset_map[&ir.r2.vn]; }
			}
			if ir.bbarg.vn > 0 {
				ir.bbarg.rn = reg_map[&ir.bbarg.vn];
				ir.bbarg.spill = ir.bbarg.rn == REG_SIZE as i32 - 1;
				if ir.bbarg.spill { ir.bbarg.spill_offset = spill_offset_map[&ir.bbarg.vn]; }
			}
			if let IrCall(_, args) = &mut ir.op {
				for i in 0..args.len() {
					args[i].rn = reg_map[&args[i].vn];
					args[i].spill = args[i].rn == REG_SIZE as i32 - 1;
					if args[i].spill { args[i].spill_offset = spill_offset_map[&args[i].vn]; }
				}
			}
		}
	}

}

fn spillout_load(n_irs: &mut Vec<Ir>, r: &Reg) {
	if r.vn < 0 || !r.spill {
		return;
	}
	n_irs.push(
		Ir::new(IrLoadSpill, r.clone(), Reg::dummy(), Reg::dummy(), Reg::dummy(), None, None, r.spill_offset, -1)
	);
}

fn spillout_store(n_irs: &mut Vec<Ir>, r: Reg) {
	if r.vn < 0 || !r.spill {
		return;
	}
	let spill_offset = r.spill_offset;
	n_irs.push(
		Ir::new(IrStoreSpill, Reg::dummy(), r, Reg::dummy(), Reg::dummy(), None, None, spill_offset, -1)
	);
}

pub fn alloc_regs(program: &mut Program) {

	// make three address form and register settings
	for fun in &mut program.funs {
		for bb in &mut fun.bbs {
			three_two(bb);
		}
		let life_vec = regs_life(fun);
		regs_setting(fun, life_vec);
	}

	// add spill instruction of load or store
	for fun in &mut program.funs {
		for bb in &mut fun.bbs {
			let irs = std::mem::replace(&mut bb.borrow_mut().irs, vec![]);
			let mut n_irs = vec![];
			for ir in irs {
				spillout_load(&mut n_irs, &ir.r1);
				spillout_load(&mut n_irs, &ir.r2);
				spillout_load(&mut n_irs, &ir.bbarg);
				let r0 = ir.r0.clone();
				n_irs.push(ir);
				spillout_store(&mut n_irs, r0);
			}
			bb.borrow_mut().irs = n_irs;
		}
	}
}