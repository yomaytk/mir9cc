use std::env;
use std::process;

fn print_typename<T>(_: T) {
    println!("{}", std::any::type_name::<T>());
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("引数が不正です.");
        process::exit(1);
    }
    
    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");

    let mut p = (&args[1][..]).chars();

    let mut a = "hello".chars();

    match a.next() {
        Some(c) => {
            println!("{}   reererere", c);
        },
        _ => {}
    }
    // println!("\tmov rax, {}", )
    loop {
        match p.next() {
            Some(c) => {
                
            },
            None => {
                break;
            }
        } 
    }
    print_typename(p);
    println!("\tmov rax, {}", args[1].parse::<i32>().unwrap());
    println!("\tret");
}
