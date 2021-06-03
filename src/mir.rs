use super::gen_ir::*;
use super::parse::*;
use std::cell::RefCell;
use std::rc::Rc;

fn new_regno() -> i32 {
    *REGNO.lock().unwrap() += 1;
    return *REGNO.lock().unwrap();
}

pub struct Program {
    pub gvars: Vec<Var>,
    pub nodes: Vec<Node>,
    pub funs: Vec<Function>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            gvars: vec![],
            nodes: vec![],
            funs: vec![],
        }
    }
}

#[derive(Debug)]
pub struct BB {
    pub label: i32,
    pub irs: Vec<Ir>,
    pub param: Reg,
    pub passed: bool,
}

impl BB {
    fn new() -> Self {
        Self {
            label: new_label(),
            irs: vec![],
            param: Reg::dummy(),
            passed: false,
        }
    }
    fn new_param() -> Self {
        Self {
            label: new_label(),
            irs: vec![],
            param: Reg::new(),
            passed: false,
        }
    }
    pub fn new_rc() -> Rc<RefCell<BB>> {
        Rc::new(RefCell::new(BB::new()))
    }
    pub fn new_param_rc() -> Rc<RefCell<BB>> {
        Rc::new(RefCell::new(BB::new_param()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Reg {
    pub vn: i32,
    pub rn: i32,
    pub spill: bool,
    pub spill_offset: i32,
}

impl Reg {
    pub fn new() -> Self {
        Self {
            vn: new_regno(),
            rn: -1,
            spill: false,
            spill_offset: -1,
        }
    }
    pub fn dummy() -> Self {
        Self {
            vn: -1,
            rn: -1,
            spill: false,
            spill_offset: -1,
        }
    }
    pub fn active(&self) -> bool {
        return self.vn > 0;
    }
}

impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "vn:{}, rn:{}", self.vn, self.rn)
    }
}

#[derive(Debug)]
pub struct RegLife {
    pub vn: i32,
    pub start: i32,
    pub end: i32,
}

impl RegLife {
    pub fn new(vn: i32, start: i32, end: i32) -> Self {
        Self { vn, start, end }
    }
}
