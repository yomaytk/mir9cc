use std::env;

pub mod token;
pub mod parse;
pub mod gen_ir;
pub mod regalloc;
pub mod gen_x86;
pub mod sema;
pub mod ir_dump;
pub mod lib;
pub mod preprocess;
pub mod mir;

use token::*;
use parse::*;
use gen_ir::*;
use regalloc::*;
use gen_x86::*;
use sema::*;
use ir_dump::*;
use preprocess::*;
use mir::*;

#[macro_use]
extern crate lazy_static;

#[allow(dead_code)]
fn print_typename<T>(_: T) {
    println!("{}", std::any::type_name::<T>());
}

fn main() {
	let mut args: Vec<String> = env::args().collect();
	
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
		println!("Usage: mir9cc [-dump-ir1] [-dump-ir2] <file>");
		std::process::exit(1);
	}

	add_program(args.pop().unwrap());

	// lexical analysis
	let tokens = tokenize(0, true);
	let mut tokenset = TokenSet::new(tokens);
	// let mut i = 0;
	// for token in &tokens {
	// 	println!("{:?}", token);
	// 	i += 1;
	// 	if i > 10 {
	// 		break;
	// 	}
	// }
	let mut program = Program::new();
	// parsing analysis
	parse(&mut tokenset, &mut program);
	// println!("{:#?}", &program.nodes);
	sema(&mut program);
	// println!("{:#?}", &program.nodes);

	// alloc index for register
	gen_ir(&mut program);
	if dump_ir1 {
		IrInfo::dump_ir(&program.funs, "-dump-ir1");
	}
	// for func in &program.funs {
	// 	for ir in &func.irs{
	// 		println!("{:?}", ir);
	// 	}
	// }
	alloc_regs(&mut program);
	if dump_ir2 {
		IrInfo::dump_ir(&program.funs, "-dump-ir2");
	}
	// for func in &program.funs {
	// 	for ir in &func.irs{
	// 		println!("{:?}", ir);
	// 	}
	// }
	
	// code generator
	gen_x86(&program);
}
