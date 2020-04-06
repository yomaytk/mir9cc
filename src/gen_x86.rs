use super::gen_ir::{*, IrOp::*};
use super::sema::Var;

pub static REG8: [&str; 8] = ["bpl", "r10b", "r11b", "bl", "r12b", "r13b", "r14b", "r15b"];
pub static REG32: [&str; 8] = ["ebp", "r10d", "r11d", "ebx", "r12d", "r13d", "r14d", "r15d"];
pub static REG64: [&str; 8] = ["rbp", "r10", "r11", "rbx", "r12", "r13", "r14", "r15"];
pub static ARGREG8: [&str; 6] = ["dil", "sil", "dl", "cl", "r8b", "r9b"];
pub static ARGREG32: [&str; 6] = ["edi", "esi", "edx", "ecx", "r8d", "r9d"];
pub static ARGREG64: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

fn escape(strname: String, len: usize) -> String {
	let mut p = strname.chars();
	let mut name = String::new();

	for i in 0..len {
		if let Some(c) = p.next(){
			if c == '\\' || c == '"'{
				name.push('\\');
				name.push(p.next().unwrap());
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
	println!("\tcmp {}, {}", REG64[ir.lhs], REG64[ir.rhs]);
	println!("\t{} {}", insn, REG8[ir.lhs]);
	println!("\tmovzb {}, {}", REG64[ir.lhs], REG8[ir.lhs]);
}

pub fn gen(fun: &Function, label: usize) {

	// program
	println!(".text");
	println!(".global {}", fun.name);
	println!("{}:", fun.name);
	println!("\tpush rbp");
	println!("\tmov rbp, rsp");
	println!("\tsub rsp, {}", fun.stacksize);
	println!("\tpush r12");
	println!("\tpush r13");
	println!("\tpush r14");
	println!("\tpush r15");

	let ret = format!(".Lend{}", label);

	for ir in &fun.irs {
		match &ir.op {
			IrImm => {
				println!("\tmov {}, {}", REG64[ir.lhs], ir.rhs);
			}
			IrMov => {
				println!("\tmov {}, {}", REG64[ir.lhs], REG64[ir.rhs]);
			}
			IrAdd => {
				println!("\tadd {}, {}", REG64[ir.lhs], REG64[ir.rhs]);
			}
			IrSub => {
				println!("\tsub {}, {}", REG64[ir.lhs], REG64[ir.rhs]);
			}
			IrSubImm => {
				println!("\tsub {}, {}", REG64[ir.lhs], ir.rhs);
			}
			IrMul => {
				println!("\tmov rax, {}", REG64[ir.rhs]);
				println!("\tmul {}", REG64[ir.lhs]);
				println!("\tmov {}, rax", REG64[ir.lhs]);
			}
			IrDiv => {
				println!("\tmov rax, {}", REG64[ir.lhs]);
				println!("\tcqo");
				println!("\tdiv {}", REG64[ir.rhs]);
				println!("\tmov {}, rax", REG64[ir.lhs]);
			}
			IrRet => {
				*LABEL.lock().unwrap() += 1;
				println!("\tmov rax, {}", REG64[ir.lhs]);
				println!("\tjmp {}", ret);
			}
			IrStore8 => {
				println!("\tmov [{}], {}", REG64[ir.lhs], REG8[ir.rhs]);
			}
			IrStore32 => {
				println!("\tmov [{}], {}", REG64[ir.lhs], REG32[ir.rhs]);
			}
			IrStore64 => {
				println!("\tmov [{}], {}", REG64[ir.lhs], REG64[ir.rhs]);
			}
			IrLoad8 => {
				println!("\tmov {}, [{}]", REG8[ir.lhs], REG64[ir.rhs]);
				println!("\tmovzb {}, {}", REG64[ir.lhs], REG8[ir.rhs]);
			}
			IrLoad32 => {
				println!("\tmov {}, [{}]", REG32[ir.lhs], REG64[ir.rhs]);
			}
			IrLoad64 => {
				println!("\tmov {}, [{}]", REG64[ir.lhs], REG64[ir.rhs]);
			}
			IrUnless => {
				println!("\tcmp {}, 0", REG64[ir.lhs]);
				println!("\tje .L{}", ir.rhs);
			}
			IrLabel => {
				println!(".L{}:", ir.lhs);
			}
			IrJmp => {
				println!("\tjmp .L{}", ir.lhs);
			}
			IrCall { name, len , args } => {

				for i in 0..*len {
					println!("\tmov {}, {}", ARGREG64[i], REG64[args[i]]);
				}
				
				println!("\tpush r10");
				println!("\tpush r11");
				println!("\tmov rax, 0");
				println!("\tcall {}", name);
				println!("\tpop r11");
				println!("\tpop r10");
				
				println!("\tmov {}, rax", REG64[ir.lhs]);
			}
			IrStoreArgs8 => {
				println!("\tmov [rbp-{}], {}", ir.lhs, ARGREG8[ir.rhs]);
			}
			IrStoreArgs32 => {
				println!("\tmov [rbp-{}], {}", ir.lhs, ARGREG32[ir.rhs]);
			}
			IrStoreArgs64 => {
				println!("\tmov [rbp-{}], {}", ir.lhs, ARGREG64[ir.rhs]);
			}
			IrLt => {
				emit_cmp(ir, String::from("setl"));
			}
			IrEqEq => {
				emit_cmp(ir, String::from("sete"));
			}
			IrNe => {
				emit_cmp(ir, String::from("setne"));
			}
			IrIf => {
				println!("\tcmp {}, 0", REG64[ir.lhs]);
				println!("\tjne .L{}", ir.rhs);
			}
			IrLabelAddr(label) => {
				println!("\tlea {}, {}", REG64[ir.lhs], label);
			}
			IrNop => {},
			_ => { panic!("unexpected IrOp in gen_x86"); }
		}
	}
	
	println!("{}:", ret);
	println!("\tpop r15");
	println!("\tpop r14");
	println!("\tpop r13");
	println!("\tpop r12");
	println!("\tmov rsp, rbp");
	println!("\tpop rbp");
	println!("\tret");
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
		println!("  .ascii \"{}\"", escape(gvar.strname.clone(), gvar.ctype.size_of()));
	}

	for i in 0..funcs.len() {
		gen(&funcs[i], i);
	}
}