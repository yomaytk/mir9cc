use std::env;
use std::process;

pub mod token;
pub mod parse;
pub mod ir;
pub mod regalloc;
pub mod codegen;
pub mod lib;

use token::*;
use parse::*;
use ir::*;
use regalloc::*;
use codegen::*;

#[macro_use]
extern crate lazy_static;

#[allow(dead_code)]
fn print_typename<T>(_: T) {
    println!("{}", std::any::type_name::<T>());
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("wrong arguments.");
        process::exit(1);
    }
	
	let p:String = (&args[1][..]).chars().collect();
	
	// lexical analysis
	let tokens = tokenize(&p);
	// for token in &tokens {
	// 	println!("{:?}", token);
	// }

	// parsing analysis
	let node = parse(&tokens, 0);
	// println!("{:#?}", &node);

	let mut reg_map: [i32; 10000] = [-1; 10000];
	let mut used: [bool; 8] = [false; 8];
	
	// alloc register
	let mut code = gen_ir(&node);
	// for ins in &code {
	// 	println!("{:?}", ins);
	// }
	alloc_regs(&mut reg_map, &mut used, &mut code);

    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");
	
	// code generator
	gen_x86(&code);
}
