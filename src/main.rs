use std::env;
use std::process;

mod lib;

use lib::TokenType;
use lib::TokenType::*;
use lib::Token;
use lib::Node;
use lib::NodeType::*;

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

		if p[pos].is_whitespace() {
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

fn number(p: &Vec<char>, tokens: &Vec<Token>, pos: usize) -> Node {
	if tokens[pos].ty == TokenNum {
		return Node::new_node_num(tokens[pos].val);
	}
	eprintln!("number expected, but got {}", p[tokens[pos].input]);
	process::exit(1);
}

fn expr(p: &Vec<char>, tokens: &Vec<Token>, mut pos: usize) -> Node {
	let mut lhs = number(p, tokens, pos);
	pos += 1;
	while(true) {
		if tokens[pos].ty != TokenPlus && tokens[pos].ty != TokenMinus {
			break;
		}
		lhs = Node::bit_init(tokens[pos].ty.clone(), lhs, number(p, tokens, pos+1));
		pos += 2;
	}
	
	if tokens[pos].ty != TokenEof {
		eprintln!("stray token: {}", p[tokens[pos].input]);
	}
	lhs
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

	// parsing analysis
	let node = expr(&p, &tokens, 0);
	
    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");
	
	println!("\tmov rax, {}", node.gen(0));
    println!("\tret");
}
