use super::gen_ir::*;

pub fn dump_ir(irv: &Vec<Function>, dump_option: &str){
	println!("{}: ", dump_option);
	for fun in irv {
		println!("{}():", fun.name);
		for bb in &fun.bbs {
			for ir in &bb.borrow().irs {
				println!("{}", ir.tostr());
			}
		}
	}
}