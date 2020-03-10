use std::env;

pub mod token;
pub mod parse;
pub mod gen_ir;
pub mod regalloc;
pub mod gen_x86;
pub mod lib;

use token::*;
use parse::*;
use gen_ir::*;
use regalloc::*;
use gen_x86::*;

#[macro_use]
extern crate lazy_static;

#[allow(dead_code)]
fn print_typename<T>(_: T) {
    println!("{}", std::any::type_name::<T>());
}

fn main() {
	let args: Vec<String> = env::args().collect();
	
	let mut dump_ir1 = false;
	let mut dump_ir2 = false;

	if args.len() == 4 && args[1] == "-dump-ir1" && args[2] == "-dump-ir2" {
		dump_ir1 = true;
		dump_ir2 = true;
	} else if args.len() == 3 && args[1] == "-dump-ir1" {
		dump_ir1 = true;
	} else if args.len() == 3 {
		dump_ir2 = true;
	} else if args.len() == 2 {
	} else {
		panic!("Usage: mir9cc [-dump-ir1] [-dump-ir2] <code>");
	}

	let p:String = (&args[args.len()-1][..]).chars().collect();
	
	// lexical analysis
	let tokens = scan(&p);
	// for token in &tokens {
	// 	println!("{:?}", token);
	// }

	// parsing analysis
	let node = parse(&tokens, &mut 0);
	// println!("{:#?}", &node);

	
	// alloc index for register
	let mut funcs = gen_ir(&node);
	if dump_ir1 {
		IrInfo::dump_ir(&funcs, "-dump-ir1");
	}
	// for func in &funcs {
	// 	for ir in &func.irs{
	// 		println!("{:?}", ir);
	// 	}
	// }
	alloc_regs(&mut funcs);
	if dump_ir2 {
		IrInfo::dump_ir(&funcs, "-dump-ir2");
	}
	// for func in &funcs {
	// 	for ir in &func.irs{
	// 		println!("{:?}", ir);
	// 	}
	// }
	
	// code generator
	gen_x86(&funcs);
}
