use std::env;
use std::fs;

pub mod token;
pub mod parse;
pub mod gen_ir;
pub mod regalloc;
pub mod gen_x86;
pub mod sema;
pub mod ir_dump;
pub mod lib;

use token::*;
use parse::*;
use gen_ir::*;
use regalloc::*;
use gen_x86::*;
use sema::*;
use ir_dump::*;

#[macro_use]
extern crate lazy_static;

#[allow(dead_code)]
fn print_typename<T>(_: T) {
    println!("{}", std::any::type_name::<T>());
}

fn read_file(filename: &str) -> Result<String, Box<dyn std::error::Error>>{
	let content = fs::read_to_string(filename)?;
	return Ok(content);
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
		println!("Usage: mir9cc [-dump-ir1] [-dump-ir2] <file>");
		std::process::exit(1);
	}

	// input program
	let p;
	match read_file(&args[args.len()-1][..]) {
		Ok(content) => { p = content; }
		Err(_) => { 
			println!("failed to read file.");
			std::process::exit(1);
		}
	}
	let t = Token::new(TokenType::TokenSub, -1, "->");
	let mut a = 1;
	if t.consume_ty(TokenType::TokenString(String::new()), &mut a) {
		panic!("jfeijfei");
	}
	// lexical analysis
	let tokens = tokenize(&p);
	// for token in &tokens {
	// 	println!("{:?}", token);
	// }

	// parsing analysis
	let nodes = parse(&tokens, &mut 0);
	// println!("{:#?}", &nodes);
	let (nodes, globals) = sema(&nodes);
	// println!("{:#?}", &nodes);

	// alloc index for register
	let mut funcs = gen_ir(&nodes);
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
	gen_x86(globals, funcs);
}
