use std::env;
use std::process;

fn print_typename<T>(_: T) {
    println!("{}", std::any::type_name::<T>());
}

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

	let mut pos;
	let first = next_number(&p, 0);
	pos = first.1;
	println!("\tmov rax, {}", first.0);

	while pos < p.len() {

		if p[pos] == '+' {
			pos += 1;
			let next = next_number(&p, pos);
			println!("\tadd rax, {}", next.0);
			pos = next.1;
			continue;
		} else if p[pos] == '-' {
			pos += 1;
			let next = next_number(&p, pos);
			println!("\tsub rax, {}", next.0);
			pos = next.1;
			continue;
		}

		eprintln!("unexpected character.");
		process::exit(1);
	}
    println!("\tret");
}
