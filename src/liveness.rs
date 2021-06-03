use super::gen_ir::IrOp;
use super::mir::*;

use linked_hash_map::LinkedHashMap;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

// decide lifespan for all Reg
pub fn regs_life(
    bb: &Rc<RefCell<BB>>,
    ic: &mut i32,
    borned_map: &mut LinkedHashMap<i32, i32>,
    died_map: &mut HashMap<i32, i32>,
) {
    if bb.borrow().passed {
        return;
    }
    bb.borrow_mut().passed = true;

    // decide lifespan
    let param_vn = bb.borrow().param.vn;
    if param_vn > 0 {
        borned_map.insert(param_vn, *ic);
        died_map.insert(param_vn, *ic);
    }
    for ir in &bb.borrow().irs {
        if !borned_map.contains_key(&ir.r0.vn) {
            borned_map.insert(ir.r0.vn, *ic);
            died_map.insert(ir.r0.vn, *ic);
        }
        if ir.r1.active() {
            died_map.insert(ir.r1.vn, *ic);
        }
        if ir.r2.active() {
            died_map.insert(ir.r2.vn, *ic);
        }
        if ir.bbarg.active() {
            died_map.insert(ir.bbarg.vn, *ic);
        }
        if let IrOp::IrCall(_, args) = &ir.op {
            for i in 0..args.len() {
                died_map.insert(args[i].vn, *ic);
            }
        }
        *ic += 1;
    }
    // when jmp block, the starting ic of it is the ic of this block
    let irs = &bb.borrow().irs;
    let irie = irs.iter();
    if let Some(ir) = irie.last() {
        if let Some(bb1) = &ir.bb1 {
            regs_life(bb1, ic, borned_map, died_map);
        }
        if let Some(bb2) = &ir.bb1 {
            regs_life(bb2, ic, borned_map, died_map);
        }
    }
}
