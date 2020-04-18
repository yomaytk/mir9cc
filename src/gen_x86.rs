use super::gen_ir::{*, IrOp::*};
use super::sema::Var;
use super::parse::roundup;
use std::collections::HashMap;
use std::sync::Mutex;

// This pass generates x86-64 assembly from IR.

macro_rules! hash {
	( $( $t:expr),* ) => {
		{
			let mut temp_hash = HashMap::new();
			$(
				temp_hash.insert($t.0, $t.1);
			)*
			temp_hash
		}
	};
}

macro_rules! emit{
    ($fmt:expr) => (print!(concat!("\t", $fmt, "\n")));
    ($fmt:expr, $($arg:tt)*) => (print!(concat!("\t", $fmt, "\n"), $($arg)*));
}

pub static REG8: [&str; 7] = ["r10b", "r11b", "bl", "r12b", "r13b", "r14b", "r15b"];
pub static REG32: [&str; 7] = ["r10d", "r11d", "ebx", "r12d", "r13d", "r14d", "r15d"];
pub static REG64: [&str; 7] = ["r10", "r11", "rbx", "r12", "r13", "r14", "r15"];
pub static ARGREG8: [&str; 6] = ["dil", "sil", "dl", "cl", "r8b", "r9b"];
pub static ARGREG32: [&str; 6] = ["edi", "esi", "edx", "ecx", "r8d", "r9d"];
pub static ARGREG64: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

lazy_static! {
	pub static ref BACKSLASH_ESCAPED: Mutex<HashMap<char, char>> = Mutex::new(hash![
		/*('b', '\b'), ('f', '\f'),*/ ('\n', 'n'), ('\r', 'r'),
		('\t', 't'), ('\\', '\\'), ('\'', '\''), ('\"', '\"')
	]);
}

fn escape(strname: String, len: usize) -> String {
	let mut p = strname.chars();
	let mut name = String::new();

	for _ in 0..len {
		if let Some(c) = p.next(){
			if let Some(c2) = BACKSLASH_ESCAPED.lock().unwrap().get(&c) {
				name.push('\\');
				name.push(*c2);
			} else if c.is_ascii_graphic() || c == ' ' {
				name.push(c);
			} else {
				name.push_str(&format!("\\{:o}", c as i8));
			}
		} else {
			name.push_str("\\000");
		}
	}
	name.push_str("\\000");
	return name;
}

fn emit_cmp(ir: &Ir, insn: String) {
	emit!("cmp {}, {}", REG64[ir.lhs], REG64[ir.rhs]);
	emit!("{} {}", insn, REG8[ir.lhs]);
	emit!("movzb {}, {}", REG64[ir.lhs], REG8[ir.lhs]);
}

fn reg(size: usize, r: usize) -> &'static str {
	if size == 1 { return REG8[r]; }
	else if size == 4 { return REG32[r]; }
	else { return REG64[r]; }
}

fn argreg(size: usize, r: usize) -> &'static str {
	if size == 1 { return ARGREG8[r]; }
	else if size == 4 { return ARGREG32[r]; }
	else { return ARGREG64[r]; }
}

pub fn gen(fun: &Function, label: usize) {

	// program
	println!(".text");
	println!(".global {}", fun.name);
	println!("{}:", fun.name);
	emit!("push rbp");
	emit!("mov rbp, rsp");
	emit!("sub rsp, {}", roundup(fun.stacksize, 16));
	emit!("push r12");
	emit!("push r13");
	emit!("push r14");
	emit!("push r15");

	let ret = format!(".Lend{}", label);

	for ir in &fun.irs {
		let lhs = ir.lhs;
		let rhs = ir.rhs;
		match &ir.op {
			IrImm => {
				emit!("mov {}, {}", REG64[lhs], rhs);
			}
			IrMov => {
				emit!("mov {}, {}", REG64[lhs], REG64[rhs]);
			}
			IrAdd(is_imm) => {
				if *is_imm {
					emit!("add {}, {}", REG64[lhs], rhs);
				} else {
					emit!("add {}, {}", REG64[lhs], REG64[rhs]);
				}
			}
			IrSub(is_imm) => {
				if *is_imm {
					emit!("sub {}, {}", REG64[lhs], rhs);
				} else {
					emit!("sub {}, {}", REG64[lhs], REG64[rhs]);
				}
			}
			IrBpRel => {
				emit!("lea {}, [rbp-{}]", REG64[lhs], rhs);
			}
			IrMul(is_imm) => {
				if !*is_imm {
					emit!("mov rax, {}", REG64[rhs]);
					emit!("mul {}", REG64[lhs]);
					emit!("mov {}, rax", REG64[lhs]);
				} else {
					if rhs < 256 && rhs.count_ones() == 1 {
						emit!("shl {}, {}", REG64[lhs], rhs.trailing_zeros());
						continue;
					}
					emit!("mov rax, {}", rhs);
					emit!("mul {}", REG64[lhs]);
					emit!("mov {}, rax", REG64[lhs]);
				}
			}
			IrDiv => {
				emit!("mov rax, {}", REG64[lhs]);
				emit!("cqo");
				emit!("div {}", REG64[rhs]);
				emit!("mov {}, rax", REG64[lhs]);
			}
			IrRet => {
				*LABEL.lock().unwrap() += 1;
				emit!("mov rax, {}", REG64[lhs]);
				emit!("jmp {}", ret);
			}
			IrStore(size) => {
				emit!("mov [{}], {}", REG64[lhs], reg(*size, rhs));
			}
			IrLoad(size) => {
				emit!("mov {}, [{}]", reg(*size, lhs), REG64[rhs]);
				if *size == 1 {
					emit!("movzb {}, {}", REG64[lhs], REG8[rhs]);
				}
			}
			IrUnless => {
				emit!("cmp {}, 0", REG64[lhs]);
				emit!("je .L{}", rhs);
			}
			IrLabel => {
				println!(".L{}:", lhs);
			}
			IrJmp => {
				emit!("jmp .L{}", lhs);
			}
			IrCall { name, len , args } => {

				for i in 0..*len {
					emit!("mov {}, {}", ARGREG64[i], REG64[args[i]]);
				}
				
				emit!("push r10");
				emit!("push r11");
				emit!("mov rax, 0");
				emit!("call {}", name);
				emit!("pop r11");
				emit!("pop r10");
				
				emit!("mov {}, rax", REG64[lhs]);
			}
			IrStoreArg(size) => {
				emit!("mov [rbp-{}], {}", lhs, argreg(*size, rhs));
			}
			IrLt => {
				emit_cmp(ir, String::from("setl"));
			}
			IrLe => {
				emit_cmp(ir, String::from("setle"));
			}
			IrEqEq => {
				emit_cmp(ir, String::from("sete"));
			}
			IrNe => {
				emit_cmp(ir, String::from("setne"));
			}
			IrIf => {
				emit!("cmp {}, 0", REG64[lhs]);
				emit!("jne .L{}", rhs);
			}
			IrLabelAddr(label) => {
				emit!("lea {}, {}", REG64[lhs], label);
			}
			IrOr => {
				emit!("or {}, {}", REG64[lhs], REG64[rhs]);
			}
			IrXor(is_imm, posneg) => {
				if *is_imm {
					emit!("xor {}, {}", REG64[lhs], rhs as i32 * *posneg);
				} else {
					emit!("xor {}, {}", REG64[lhs], REG64[rhs]);
				}
			}
			IrAnd => {
				emit!("and {}, {}", REG64[lhs], REG64[rhs]);
			}
			IrShl => {
				emit!("mov cl, {}", REG8[rhs]);
				emit!("shl {}, cl", REG64[lhs]);
			}
			IrShr => {
				emit!("mov cl, {}", REG8[rhs]);
				emit!("shr {}, cl", REG64[lhs]);
			}
			IrMod => {
				emit!("mov rax, {}", REG64[lhs]);
				emit!("cqo");
				emit!("div {}", REG64[rhs]);
				emit!("mov {}, rdx", REG64[lhs]);
			}
			IrNeg => {
				emit!("neg {}", REG64[lhs]);
			}
			IrNop => {},
			_ => { panic!("unexpected IrOp in gen_x86"); }
		}
	}
	
	println!("{}:", ret);
	emit!("pop r15");
	emit!("pop r14");
	emit!("pop r13");
	emit!("pop r12");
	emit!("mov rsp, rbp");
	emit!("pop rbp");
	emit!("ret");
}

pub fn gen_x86(globals: Vec<Var>, funcs: Vec<Function>) {
	
	println!(".intel_syntax noprefix");
	
	// global variable
	println!(".data");
	for gvar in &globals {
		if gvar.is_extern {
			continue;
		}
		println!("{}:", gvar.ident.clone());
		emit!(".ascii \"{}\"", escape(gvar.strname.clone(), gvar.ctype.size));
	}

	for i in 0..funcs.len() {
		gen(&funcs[i], i);
	}
}