static REG: [&str; 8] = ["rdi", "rsi", "r10", "r11", "r12", "r13", "r14", "r15"];
static REG_SIZE: usize = 8;

use NodeType::*;
use TokenType::*;
use IrType::*;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
	TokenNum,
	TokenPlus,
	TokenMinus,
	TokenEof,
}

pub struct Token {
	pub ty: TokenType,
	pub val: i32,
	pub input: usize,
}

impl Token {
	pub fn new(ty: TokenType, val: i32, input: usize) -> Token {
		Token {
			ty: ty,
			val: val,
			input: input,
		}
	}
}

#[allow(dead_code)]
pub enum NodeType {
	NodeNum,
	BinaryTree(TokenType, Option<Box<Node>>, Option<Box<Node>>),
}

#[allow(dead_code)]
impl NodeType {
	fn bit_new(tk_ty: TokenType) -> Self {
		NodeType::BinaryTree(tk_ty, None, None)
	}

	fn bit_init(tk_ty: TokenType, lhs: Node, rhs: Node) -> Self {
		NodeType::BinaryTree(tk_ty, Some(Box::new(lhs)), Some(Box::new(rhs)))
	}
}

pub struct Node {
	pub val: i32,
	pub ty: NodeType,
}

#[allow(dead_code)]
impl Node {
	pub fn new(tk_ty: TokenType) -> Self {
		Self {
			val: 0,
			ty: NodeType::bit_new(tk_ty),
		}
	}

	pub fn bit_init(tk_ty: TokenType, lhs: Node, rhs: Node) -> Self {
		Self {
			val: 0,
			ty: NodeType::bit_init(tk_ty, lhs, rhs),
		}
	}
	
	pub fn new_node_num(val: i32) -> Self {
		Self {
			val: val,
			ty: NodeType::NodeNum,
		}
	}
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum IrType {
	IrImm,
	IrMov,
	IrPlus,
	IrMinus,
	IrReturn,
	IrKill,
	IrNop,
}

#[derive(Debug)]
pub struct Ir {
	pub ty: IrType,
	pub lhs: usize,
	pub rhs: usize,
}

impl Ir {
	fn new(ty: IrType, lhs: usize, rhs: usize) -> Self {
		Self {
			ty: ty,
			lhs: lhs,
			rhs: rhs,
		}
	}
	fn tokentype2irtype(ty: &TokenType) -> IrType {
		match ty {
			TokenPlus => { IrPlus },
			TokenMinus => { IrMinus },
			_ => { panic!("tokentype2irtype error."); }
		}
	}
}

// allocate of index for register to NodeNum
fn gen_ir_sub(node: &Node, ins: &mut Vec<Ir>, regno: usize) -> (usize, usize) {

	match &node.ty {
		NodeNum => {
			let ir = Ir::new(IrImm, regno, node.val as usize);
			ins.push(ir);
			return (regno, regno+1);
		},
		BinaryTree(ty, lhs, rhs) => {
			let (lhi, lreg) = gen_ir_sub(lhs.as_ref().unwrap(), ins, regno);
			let (rhi, rreg) = gen_ir_sub(rhs.as_ref().unwrap(), ins, lreg);
			ins.push(Ir::new(Ir::tokentype2irtype(ty), lhi, rhi));
			ins.push(Ir::new(IrKill, rhi, 0));
			return (lhi, rreg);
		}
	}
}

// generate IR Vector
pub fn gen_ir(node: &Node, ins: &mut Vec<Ir>) {
	let (r, _) = gen_ir_sub(node, ins, 0);
	ins.push(Ir::new(IrReturn, r, 0));
}

// allocate the register can be used
fn alloc(reg_map: &mut [i32], used: &mut [bool], ir_reg: usize) -> usize {
	
	if reg_map[ir_reg] != -1 {
		if !used[reg_map[ir_reg] as usize] { panic!("the register allocated is not used. at reg_map[{}]", ir_reg); }
		return reg_map[ir_reg] as usize;
	}

	let mut i: usize = 0;
	while i < REG_SIZE {
		if used[i] {
			i += 1;
			continue;
		}
		reg_map[ir_reg] = i as i32;
		used[i] = true;
		return i;
	}
	panic!("register exhausted.");
}

fn kill(used: &mut [bool], i: i32){
	let id: usize = i as usize;
	if !used[id] { panic!("cannot release the register not allocated."); }
	used[id] = false;
}

// do allocating register to reg_map 
pub fn alloc_regs(reg_map: &mut [i32], used: &mut [bool], ins: &mut Vec<Ir>) {
	for ir in ins {
		match ir.ty {
			IrImm => {
				ir.lhs = alloc(reg_map, used, ir.lhs);
			},
			IrMov | IrPlus | IrMinus => {
				ir.lhs = alloc(reg_map, used, ir.lhs);
				ir.rhs = alloc(reg_map, used, ir.rhs);
			},
			IrReturn => {
				kill(used, reg_map[ir.lhs]);
			},
			IrKill => {
				kill(used, reg_map[ir.lhs]);
				ir.ty = IrNop;
			},
			_ => { panic!("unknown operator."); }
		}
	}
}

pub fn gen_x86(ins: &Vec<Ir>) {
	for ir in ins {
		match ir.ty {
			IrImm => {
				println!("\tmov {}, {}", REG[ir.lhs], ir.rhs);
			},
			IrMov => {
				println!("\tmov {}, {}", REG[ir.lhs], REG[ir.rhs]);
			}, 
			IrPlus => {
				println!("\tadd {}, {}", REG[ir.lhs], REG[ir.rhs]);
			},
			IrMinus => {
				println!("\tsub {}, {}", REG[ir.lhs], REG[ir.rhs]);
			},
			IrReturn => {
				println!("\tmov rax, {}", REG[ir.lhs]);
				println!("\tret");
			},
			IrNop => {},
			_ => { panic!("unexpected IrType in gen_x86"); }
		}
	}
}