use std::env;
use std::process;

pub mod token;
pub mod parse;
pub mod ir;
pub mod regalloc;
pub mod codegen;

use token::*;
use parse::*;
use ir::*;
use regalloc::*;
use codegen::*;

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
	
	let p:Vec<char> = (&args[1][..]).chars().collect();
	
	let mut tokens: Vec<Token> = vec![];
	
	// lexical analysis
	tokenize(&p, &mut tokens, 0);
	// for token in &tokens {
	// 	println!("{:?}", token);
	// }

	// parsing analysis
	let node = parse(&p, &tokens, 0);
	// println!("{:#?}", &node);

	let mut ins: Vec<Ir> = vec![];
	let mut reg_map: [i32; 10000] = [-1; 10000];
	let mut used: [bool; 8] = [false; 8];
	// alloc register
	gen_ir(&node, &mut ins);
	alloc_regs(&mut reg_map, &mut used, &mut ins);

    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");
	
	// code generator
	gen_x86(&ins);
}
