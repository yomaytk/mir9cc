use super::parse::*;
use super::gen_ir::*;

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

pub struct BB {
	pub label: i32,
	pub irs: Vec<Ir>,
}

impl BB {
	pub fn new() -> Self {
		Self {
			label: new_label(),
			irs: vec![],
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Reg {
	pub vn: i32,
	pub rn: i32
}

impl Reg {
	pub fn new() -> Self {
		Self {
			vn: new_regno(),
			rn: -1
		}
	}
	pub fn dummy() -> Self {
		Self {
			vn: -1,
			rn: -1,
		}
	}
}

impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "vn:{}, rn:{}", self.vn, self.rn)
    }
}