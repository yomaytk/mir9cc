use std::env;
use std::process;

mod lib;

use lib::TokenType;
use lib::TokenType::*;
use lib::Token;

#[allow(dead_code)]
fn print_typename<T>(_: T) {
    println!("{}", std::any::type_name::<T>());
}

// return next_number and position
fn next_number(p: &Vec<char>, mut pos: usize) -> (i32, usize) {
	let mut num = String::from("");
	for i in pos..p.len() {
		if p[i].is_digit(10) {
			num.push(p[i]);
			pos += 1;
		} else {
			break;
		}
	}
	(num.parse::<i32>().unwrap(), pos)
}

// return TokenType of given character
fn signal2token (p: char) -> TokenType {
	if p == '+' { TokenPlus }
	else if p == '-' { TokenMinus }
	else { panic!("signal2token error!"); }
}

fn tokenize(p: &Vec<char>, tokens: &mut Vec<Token>, mut pos: usize) {
	
	while pos < p.len() {

		if p[pos] == ' ' {
			pos += 1;
			continue;
		}
		
		if p[pos] == '+' || p[pos] == '-' {
			let token = Token::new(signal2token(p[pos]), 0, pos);
			tokens.push(token);
			pos += 1;
			continue;
		}
		
		if p[pos].is_digit(10) {
			let next = next_number(p, pos);
			let token = Token::new(TokenNum, next.0, pos);
			pos = next.1;
			tokens.push(token);
			continue;
		}

		eprintln!("cannot tokenize.");
		process::exit(1);
	}

	let token = Token::new(TokenEof, 0, 0);
	tokens.push(token);

}

fn fail(p: &Vec<char>, tokens: &Vec<Token>, i: usize) {
	eprintln!("unexpected token :{}", p[tokens[i].input]);
	process::exit(1);
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("wrong arguments.");
        process::exit(1);
    }
    
    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");

	let p:Vec<char> = (&args[1][..]).chars().collect();
	
	let mut tokens: Vec<Token> = vec![];

	// lexical analysis
	tokenize(&p, &mut tokens, 0);

	if tokens[0].ty != TokenNum { fail(&p, &tokens, 0); }
	println!("\tmov rax, {}", tokens[0].val);
	
	let mut tk_id = 1;

	while tokens[tk_id].ty != TokenEof {

		if tokens[tk_id].ty == TokenPlus {
			tk_id += 1;
			if tokens[tk_id].ty != TokenNum { fail(&p, &tokens, tk_id); }
			println!("\tadd rax, {}", tokens[tk_id].val);
			tk_id += 1;
			continue;
		}

		if tokens[tk_id].ty == TokenMinus {
			tk_id += 1;
			if tokens[tk_id].ty != TokenNum { fail(&p, &tokens, tk_id); }
			println!("\tsub rax, {}", tokens[tk_id].val);
			tk_id += 1;
			continue;
		}

		fail(&p, &tokens, tk_id);

	}

    println!("\tret");
}
