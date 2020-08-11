use super::parse::*;
use super::gen_ir::*;

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