use super::parse::*;
use super::token::{TokenType::*, *};
use IrOp::*;
// use super::lib::*;
use super::mir::*;

use linked_hash_map::LinkedHashMap;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Mutex;

// mir9cc's code generation is two-pass. In the first pass, abstract
// syntax trees are compiled to IR (intermediate representation).
//
// IR resembles the real x86-64 instruction set, but it has infinite
// number of registers. We don't try too hard to reuse registers in
// this pass. Instead, we "kill" registers to mark them as dead when
// we are done with them and use new registers.
//
// Such infinite number of registers are mapped to a finite registers
// in a later pass.

lazy_static! {
    pub static ref REGNO: Mutex<i32> = Mutex::new(1);
}

thread_local!(pub static SWITCHES: Rc<RefCell<Vec<Vec<Rc<RefCell<BB>>>>>> = Rc::new(RefCell::new(vec![])));
thread_local!(pub static CONTINUE_VEC: Rc<RefCell<Vec<Rc<RefCell<BB>>>>> = Rc::new(RefCell::new(vec![])));
thread_local!(pub static BREAK_VEC: Rc<RefCell<Vec<Rc<RefCell<BB>>>>> = Rc::new(RefCell::new(vec![])));

#[allow(dead_code)]
#[derive(Debug, PartialEq, std::cmp::Eq, std::hash::Hash)]
pub enum IrOp {
    IrImm,
    IrMov,
    IrAdd,
    IrBpRel,
    IrSub,
    IrMul,
    IrDiv,
    IrRet,
    IrStore(i32),
    IrLoad(i32),
    IrJmp,
    IrCall(String, Vec<Reg>),
    IrStoreArg(i32),
    IrLt,
    IrEqual,
    IrNe,
    IrLabelAddr(String),
    IrOr,
    IrXor,
    IrAnd,
    IrLe,
    IrShl,
    IrShr,
    IrMod,
    IrNeg,
    IrBr,
    IrLoadSpill,
    IrStoreSpill,
}

#[derive(Debug)]
pub struct Ir {
    pub op: IrOp,
    pub r0: Reg,
    pub r1: Reg,
    pub r2: Reg,
    pub bbarg: Reg,
    pub bb1: Option<Rc<RefCell<BB>>>,
    pub bb2: Option<Rc<RefCell<BB>>>,
    pub imm: i32,
    pub imm2: i32,
}

impl Ir {
    pub fn new(
        op: IrOp,
        r0: Reg,
        r1: Reg,
        r2: Reg,
        bbarg: Reg,
        bb1: Option<Rc<RefCell<BB>>>,
        bb2: Option<Rc<RefCell<BB>>>,
        imm: i32,
        imm2: i32,
    ) -> Self {
        Self {
            op,
            r0,
            r1,
            r2,
            bbarg,
            bb1,
            bb2,
            imm,
            imm2,
        }
    }
    fn bittype(ty: &TokenType) -> IrOp {
        match ty {
            TokenAdd => IrAdd,
            TokenSub => IrSub,
            TokenStar => IrMul,
            TokenDiv => IrDiv,
            TokenLt => IrLt,
            TokenLe => IrLe,
            TokenShl => IrShl,
            TokenShr => IrShr,
            TokenMod => IrMod,
            TokenAmpersand => IrAnd,
            TokenOr => IrOr,
            TokenXor => IrXor,
            TokenEof => {
                panic!("tokeneof!!!");
            }
            _ => {
                panic!("bittype error.");
            }
        }
    }
    pub fn tostr(&self) -> String {
        match &self.op {
            IrImm => {
                return format!("Imm r{}, {}", self.r0, self.imm);
            }
            IrMov => {
                return format!("Mov r{}, r{}", self.r0, self.r2);
            }
            IrAdd => {
                return format!("Add r{}, r{}", self.r0, self.r2);
            }
            IrBpRel => {
                return format!("Lea r{}, [rbp-{}]", self.r0, self.imm);
            }
            IrSub => {
                return format!("Sub r{}, r{}", self.r0, self.r2);
            }
            IrMul => {
                return format!("Mul r{}, r{}", self.r0, self.r2);
            }
            IrDiv => {
                return format!("Div r{}, r{}", self.r0, self.r2);
            }
            IrRet => {
                return format!("Return {}", self.r0);
            }
            IrStore(ir_size) => {
                return format!("Store{} [r{}], r{}", ir_size, self.r0, self.r2);
            }
            IrLoad(ir_size) => {
                return format!("Load{} r{}, [r{}]", ir_size, self.r0, self.r2);
            }
            IrJmp => {
                return format!("Jmp .L{}", self.imm);
            }
            IrCall(name, args) => {
                let mut call_s = format!("Call {}(", name);
                for arg in args {
                    call_s.push_str(&format!("r{}, ", arg));
                }
                if call_s.len() > 0 {
                    call_s.pop();
                    call_s.pop();
                }
                call_s.push_str(")");
                return call_s;
            }
            IrStoreArg(ir_size) => {
                return format!("STORE_ARG{}, {}, {}", ir_size, self.imm, self.imm2);
            }
            IrLt => {
                return format!("Lt r{}, r{}", self.r0, self.r2);
            }
            IrEqual => {
                return format!("Equal r{}, r{}", self.r0, self.r2);
            }
            IrNe => {
                return format!("Ne r{}, r{}", self.r0, self.r2);
            }
            IrLabelAddr(labelname) => {
                return format!("Label_Addr r{}, .L{}", self.r0, labelname);
            }
            IrOr => {
                return format!("Or r{}, r{}", self.r0, self.r2);
            }
            IrXor => {
                return format!("Xor r{}, r{}", self.r0, self.r2);
            }
            IrAnd => {
                return format!("And r{}, r{}", self.r0, self.r2);
            }
            IrLe => {
                return format!("Le r{}, r{}", self.r0, self.r2);
            }
            IrShl => {
                return format!("Shl r{}, r{}", self.r0, self.r2);
            }
            IrShr => {
                return format!("Shr r{}, r{}", self.r0, self.r2);
            }
            IrMod => {
                return format!("Mod r{}, r{}", self.r0, self.r2);
            }
            IrNeg => {
                return format!("Neg r{}", self.r0);
            }
            IrBr => {
                return format!(
                    "Br r{}, .L{}, .L{}",
                    self.r0,
                    self.bb1.clone().unwrap().borrow().label,
                    self.bb2.clone().unwrap().borrow().label
                );
            }
            IrLoadSpill => {
                return format!("LoadSpill");
            }
            IrStoreSpill => {
                return format!("StoreSpill");
            }
        }
    }
    fn emit(op: IrOp, r0: Reg, r1: Reg, r2: Reg, fun: &mut Function) {
        fun.bbs.last_mut().unwrap().borrow_mut().irs.push(Ir::new(
            op,
            r0,
            r1,
            r2,
            Reg::dummy(),
            None,
            None,
            -1,
            -1,
        ));
    }
    fn bb_emit(
        op: IrOp,
        r0: Reg,
        r2: Reg,
        bbarg: Reg,
        bb1: Option<Rc<RefCell<BB>>>,
        bb2: Option<Rc<RefCell<BB>>>,
        fun: &mut Function,
    ) {
        fun.bbs.last_mut().unwrap().borrow_mut().irs.push(Ir::new(
            op,
            r0,
            Reg::dummy(),
            r2,
            bbarg,
            bb1,
            bb2,
            -1,
            -1,
        ));
    }
    fn br(r: Reg, then: Option<Rc<RefCell<BB>>>, els: Option<Rc<RefCell<BB>>>, fun: &mut Function) {
        Ir::bb_emit(IrBr, Reg::dummy(), r, Reg::dummy(), then, els, fun);
    }
    fn imm_emit(op: IrOp, r0: Reg, imm: i32, imm2: i32, fun: &mut Function) {
        fun.bbs.last_mut().unwrap().borrow_mut().irs.push(Ir::new(
            op,
            r0,
            Reg::dummy(),
            Reg::dummy(),
            Reg::dummy(),
            None,
            None,
            imm,
            imm2,
        ));
    }
}

pub struct Function {
    pub name: String,
    pub bbs: Vec<Rc<RefCell<BB>>>,
    pub args: LinkedHashMap<String, Var>,
    pub stacksize: i32,
}

impl Function {
    pub fn new(
        name: String,
        bbs: Vec<Rc<RefCell<BB>>>,
        args: LinkedHashMap<String, Var>,
        stacksize: i32,
    ) -> Self {
        Self {
            name,
            bbs,
            args,
            stacksize,
        }
    }
    pub fn bb_push(&mut self, bb: Rc<RefCell<BB>>) {
        self.bbs.push(bb);
    }
}

fn get_switches_rc_mut() -> Rc<RefCell<Vec<Vec<Rc<RefCell<BB>>>>>> {
    SWITCHES.with(|rc| rc.clone())
}

fn get_continue_vec_rc_mut() -> Rc<RefCell<Vec<Rc<RefCell<BB>>>>> {
    CONTINUE_VEC.with(|rc| rc.clone())
}

fn get_break_vec_rc_mut() -> Rc<RefCell<Vec<Rc<RefCell<BB>>>>> {
    BREAK_VEC.with(|rc| rc.clone())
}

fn jmp(x_bb: Option<Rc<RefCell<BB>>>, bbarg: Reg, fun: &mut Function) {
    Ir::bb_emit(IrJmp, Reg::dummy(), Reg::dummy(), bbarg, x_bb, None, fun);
}

fn load(ctype: &Type, dst: Reg, src: Reg, fun: &mut Function) {
    Ir::emit(IrOp::IrLoad(ctype.size), dst, Reg::dummy(), src, fun);
}

fn store(ctype: &Type, dst: Reg, src: Reg, fun: &mut Function) {
    Ir::emit(IrOp::IrStore(ctype.size), Reg::dummy(), dst, src, fun);
}

fn store_arg(size: i32, offset: i32, id: i32, fun: &mut Function) {
    Ir::imm_emit(IrOp::IrStoreArg(size), Reg::dummy(), offset, id, fun);
}

fn gen_binop(irop: IrOp, lhs: &Node, rhs: &Node, fun: &mut Function) -> Reg {
    let r0 = Reg::new();
    Ir::emit(
        irop,
        r0.clone(),
        gen_expr(lhs, fun),
        gen_expr(rhs, fun),
        fun,
    );
    return r0;
}

fn gen_inc_scale(ctype: &Type) -> i32 {
    match ctype.ty {
        Ty::PTR => {
            return ctype.ptr_to.as_ref().unwrap().size;
        }
        _ => {
            return 1;
        }
    }
}

fn imm(op: IrOp, imm: i32, fun: &mut Function) -> Reg {
    let r = Reg::new();
    Ir::imm_emit(op, r.clone(), imm, -1, fun);
    return r;
}

fn gen_pre_inc(ctype: &Type, lhs: &Node, fun: &mut Function, num: i32) -> Reg {
    let r1 = gen_lval(lhs, fun);
    let r2 = Reg::new();
    load(ctype, r2.clone(), r1.clone(), fun);
    let r3 = imm(IrImm, num * gen_inc_scale(ctype), fun);
    let r4 = Reg::new();
    Ir::emit(IrAdd, r4.clone(), r2, r3, fun);
    store(ctype, r1, r4.clone(), fun);
    return r4;
}

fn gen_post_inc(ctype: &Type, lhs: &Node, fun: &mut Function, num: i32) -> Reg {
    let r1 = gen_pre_inc(ctype, lhs, fun, num);
    let r2 = imm(IrImm, num * gen_inc_scale(ctype), fun);
    let r3 = Reg::new();
    Ir::emit(IrSub, r3.clone(), r1, r2, fun);
    return r3;
}

fn get_bb_break() -> Rc<RefCell<BB>> {
    let break_vec = get_break_vec_rc_mut();
    if let Some(bb_break) = break_vec.borrow().last() {
        return bb_break.clone();
    } else {
        eprintln!("cannot find jmp point of break.");
    }
    std::process::exit(0);
}

fn get_bb_continue() -> Rc<RefCell<BB>> {
    let continue_vec = get_continue_vec_rc_mut();
    if let Some(bb_continue) = continue_vec.borrow().last() {
        return bb_continue.clone();
    } else {
        eprintln!("cannot find jmp point of break.");
    }
    std::process::exit(0);
}

fn loop_inc(bb_continue: Rc<RefCell<BB>>, bb_break: Rc<RefCell<BB>>) {
    let continue_vec = get_continue_vec_rc_mut();
    let break_vec = get_break_vec_rc_mut();
    continue_vec.borrow_mut().push(bb_continue);
    break_vec.borrow_mut().push(bb_break);
}

fn loop_dec() {
    let break_vec = get_break_vec_rc_mut();
    let continue_vec = get_continue_vec_rc_mut();
    if let None = break_vec.borrow_mut().pop() {
        eprintln!("cannot find jmp point of break.");
        std::process::exit(0);
    }
    if let None = continue_vec.borrow_mut().pop() {
        eprintln!("cannot find jmp point of continue.");
        std::process::exit(0);
    }
    return;
}

// In C, all expressions that can be written on the left-hand side of
// the '=' operator must have an address in memory. In other words, if
// you can apply the '&' operator to take an address of some
// expression E, you can assign E to a new value.
//
// Other expressions, such as `1+2`, cannot be written on the lhs of
// '=', since they are just temporary values that don't have an address.
//
// The stuff that can be written on the lhs of '=' is called lvalue.
// Other values are called rvalue. An lvalue is essentially an address.
//
// When lvalues appear on the rvalue context, they are converted to
// rvalues by loading their values from their addresses. You can think
// '&' as an operator that suppresses such automatic lvalue-to-rvalue
// conversion.
//
// This function evaluates a given node as an lvalue.

fn gen_lval(node: &Node, fun: &mut Function) -> Reg {
    match &node.op {
        NodeType::Deref(_, expr) => {
            return gen_expr(expr, fun);
        }
        NodeType::VarRef(var) => {
            if var.is_local {
                let r = imm(IrBpRel, var.offset, fun);
                return r;
            } else {
                let r = Reg::new();
                Ir::emit(
                    IrLabelAddr(var.labelname.clone().unwrap()),
                    r.clone(),
                    Reg::dummy(),
                    Reg::dummy(),
                    fun,
                );
                return r;
            }
        }
        NodeType::Dot(ctype, expr, _) => {
            let r1 = gen_lval(expr, fun);
            let r2 = imm(IrImm, ctype.offset, fun);
            let r3 = Reg::new();
            Ir::emit(IrAdd, r3.clone(), r1, r2, fun);
            return r3;
        }
        _ => {
            panic!("not an lvalue")
        }
    }
}

// allocate of index for register to NodeNum
fn gen_expr(node: &Node, fun: &mut Function) -> Reg {
    match &node.op {
        NodeType::Num(val) => {
            let r = imm(IrImm, *val, fun);
            return r;
        }
        NodeType::BinaryTree(_, ty, lhs, rhs) => {
            match ty {
                // a && b
                TokenLogAnd => {
                    let bb = BB::new_rc();
                    let set0 = BB::new_rc();
                    let set1 = BB::new_rc();
                    let last = BB::new_param_rc();
                    let ret = last.borrow().param.clone();

                    Ir::br(
                        gen_expr(lhs, fun),
                        Some(Rc::clone(&bb)),
                        Some(Rc::clone(&set0)),
                        fun,
                    );

                    fun.bb_push(bb);
                    Ir::br(
                        gen_expr(rhs, fun),
                        Some(Rc::clone(&set1)),
                        Some(Rc::clone(&set0)),
                        fun,
                    );

                    fun.bb_push(set0);
                    jmp(Some(Rc::clone(&last)), imm(IrImm, 0, fun), fun);

                    fun.bb_push(set1);
                    jmp(Some(Rc::clone(&last)), imm(IrImm, 1, fun), fun);

                    fun.bb_push(last);

                    return ret;
                }
                // a || b
                TokenLogOr => {
                    let bb = BB::new_rc();
                    let set0 = BB::new_rc();
                    let set1 = BB::new_rc();
                    let last = BB::new_param_rc();
                    let ret = last.borrow().param.clone();

                    Ir::br(
                        gen_expr(lhs, fun),
                        Some(Rc::clone(&set1)),
                        Some(Rc::clone(&bb)),
                        fun,
                    );

                    fun.bb_push(bb);
                    Ir::br(
                        gen_expr(rhs, fun),
                        Some(Rc::clone(&set1)),
                        Some(Rc::clone(&last)),
                        fun,
                    );

                    fun.bb_push(set0);
                    jmp(Some(Rc::clone(&last)), imm(IrImm, 0, fun), fun);

                    fun.bb_push(set1);
                    jmp(Some(Rc::clone(&last)), imm(IrImm, 1, fun), fun);

                    fun.bb_push(last);

                    return ret;
                }
                _ => {
                    // a R b (R != &&, ||)
                    return gen_binop(Ir::bittype(ty), lhs, rhs, fun);
                }
            }
        }
        // a
        NodeType::VarRef(var) => {
            let r0 = Reg::new();
            load(&var.ctype, r0.clone(), gen_lval(node, fun), fun);
            return r0;
        }
        // a.b (struct member)
        NodeType::Dot(ctype, ..) => {
            let r0 = Reg::new();
            load(ctype, r0.clone(), gen_lval(node, fun), fun);
            return r0;
        }
        // a = b
        NodeType::Assign(ctype, lhs, rhs) => {
            let r2 = gen_expr(rhs, fun);
            store(ctype, gen_lval(lhs, fun), r2.clone(), fun);
            return r2;
        }
        // fun(...)
        NodeType::Call(_, ident, callarg) => {
            let mut args = vec![];
            for arg in callarg {
                args.push(gen_expr(arg, fun));
            }
            let r = Reg::new();
            Ir::emit(
                IrCall((*ident).clone(), args.clone()),
                r.clone(),
                Reg::dummy(),
                Reg::dummy(),
                fun,
            );
            return r;
        }
        // *a
        NodeType::Deref(_, lhs) => {
            let r0 = Reg::new();
            load(
                lhs.nodesctype(None).ptr_to.unwrap().as_ref(),
                r0.clone(),
                gen_expr(lhs, fun),
                fun,
            );
            return r0;
        }
        // &a
        NodeType::Addr(_, lhs) => {
            return gen_lval(lhs, fun);
        }
        // a == b
        NodeType::Equal(lhs, rhs) => {
            return gen_binop(IrEqual, lhs, rhs, fun);
        }
        // a != b
        NodeType::Ne(lhs, rhs) => {
            return gen_binop(IrNe, lhs, rhs, fun);
        }
        // !a
        NodeType::Not(expr) => {
            let r0 = Reg::new();
            Ir::emit(
                IrEqual,
                r0.clone(),
                gen_expr(expr, fun),
                imm(IrImm, 0, fun),
                fun,
            );
            return r0;
        }
        // a ? b : c
        NodeType::Ternary(_, cond, then, els) => {
            let bb1 = BB::new_rc();
            let bb2 = BB::new_rc();
            let last = BB::new_param_rc();
            let ret = last.borrow().param.clone();

            Ir::br(
                gen_expr(cond, fun),
                Some(Rc::clone(&bb1)),
                Some(Rc::clone(&bb2)),
                fun,
            );

            fun.bb_push(bb1);
            jmp(Some(Rc::clone(&last)), gen_expr(then, fun), fun);

            fun.bb_push(bb2);
            jmp(Some(Rc::clone(&last)), gen_expr(els, fun), fun);

            fun.bb_push(last);

            return ret;
        }
        // (a, b)
        NodeType::TupleExpr(_, lhs, rhs) => {
            gen_expr(lhs, fun);
            return gen_expr(rhs, fun);
        }
        // ++a, a++
        NodeType::IncDec(ctype, selector, lhs) => {
            if *selector == 1 {
                return gen_post_inc(ctype, lhs, fun, 1);
            } else {
                return gen_post_inc(ctype, lhs, fun, -1);
            }
        }
        // _Bool x = 2; -> x == 1;
        NodeType::Cast(ctype, expr) => {
            let r1 = gen_expr(expr, fun);
            if ctype.ty != Ty::BOOL {
                return r1;
            }
            let r0 = Reg::new();
            Ir::emit(IrNe, r0.clone(), r1, imm(IrImm, 0, fun), fun);
            return r0;
        }
        NodeType::StmtExpr(_, body) => {
            if let NodeType::CompStmt(stmts) = &body.op {
                let len = stmts.len();
                for i in 0..len - 1 {
                    gen_stmt(&stmts[i], fun);
                }
                if len > 0 {
                    if let NodeType::Expr(ref expr) = stmts.last().unwrap().op {
                        return gen_expr(expr, fun);
                    }
                }
            }
            let r0 = imm(IrImm, 0, fun);
            return r0;
        }
        _ => {
            panic!("gen_expr NodeType error at {:?}", node.op);
        }
    }
}

fn gen_stmt(node: &Node, fun: &mut Function) {
    match &node.op {
        NodeType::NULL => {
            return;
        }
        NodeType::Ret(lhs) => {
            Ir::emit(
                IrRet,
                Reg::dummy(),
                Reg::dummy(),
                gen_expr(lhs.as_ref(), fun),
                fun,
            );
            fun.bb_push(BB::new_rc());
        }
        NodeType::Expr(lhs) => {
            if let NodeType::ArrIni(_) = lhs.op {
                gen_stmt(lhs, fun);
                return;
            }
            gen_expr(lhs.as_ref(), fun);
        }
        NodeType::IfThen(cond, then, els) => {
            let bbt = BB::new_rc();
            let bbe = BB::new_rc();
            let last = BB::new_rc();

            Ir::br(
                gen_expr(cond, fun),
                Some(Rc::clone(&bbt)),
                Some(Rc::clone(&bbe)),
                fun,
            );

            fun.bb_push(bbt);
            gen_stmt(then, fun);
            jmp(Some(Rc::clone(&last)), Reg::dummy(), fun);

            fun.bb_push(bbe);
            if let Some(elsth) = els {
                gen_stmt(elsth, fun);
            }
            jmp(Some(Rc::clone(&last)), Reg::dummy(), fun);

            fun.bb_push(last);
        }
        NodeType::CompStmt(lhs) => {
            for stmt in lhs {
                gen_stmt(stmt, fun);
            }
        }
        NodeType::For(init, cond, inc, body) => {
            let bb_cond = BB::new_rc();
            let bb_body = BB::new_rc();
            let bb_continue = BB::new_rc();
            let bb_break = BB::new_rc();
            let bb_cond_rc = Rc::clone(&bb_cond);

            loop_inc(bb_continue.clone(), bb_break.clone());
            gen_stmt(init, fun);

            fun.bb_push(bb_cond);
            match cond.op {
                NodeType::NULL => {}
                _ => {
                    Ir::br(
                        gen_expr(cond, fun),
                        Some(Rc::clone(&bb_body)),
                        Some(Rc::clone(&bb_break)),
                        fun,
                    );
                }
            }
            jmp(Some(Rc::clone(&bb_body)), Reg::dummy(), fun);

            fun.bb_push(bb_body);
            gen_stmt(body, fun);
            jmp(Some(Rc::clone(&bb_continue)), Reg::dummy(), fun);

            fun.bb_push(bb_continue);
            gen_stmt(inc, fun);
            jmp(Some(bb_cond_rc), Reg::dummy(), fun);

            fun.bb_push(bb_break);

            loop_dec();
        }
        NodeType::DoWhile(body, cond) => {
            let bb_body = BB::new_rc();
            let bb_continue = BB::new_rc();
            let bb_break = BB::new_rc();
            let bb_body_rc = Rc::clone(&bb_body);

            loop_inc(bb_continue.clone(), bb_break.clone());

            fun.bb_push(bb_body);
            gen_stmt(body, fun);
            jmp(Some(Rc::clone(&bb_continue)), Reg::dummy(), fun);

            fun.bb_push(bb_continue);
            Ir::br(
                gen_expr(cond, fun),
                Some(bb_body_rc),
                Some(Rc::clone(&bb_break)),
                fun,
            );

            fun.bb_push(bb_break);

            loop_dec();
        }
        NodeType::Switch(cond, body, case_conds) => {
            let bb_continue = BB::new_rc();
            let bb_break = BB::new_rc();
            loop_inc(bb_continue.clone(), bb_break.clone());
            let switches = get_switches_rc_mut();
            switches.borrow_mut().push(vec![]);

            let r = gen_expr(cond, fun);

            for val in case_conds {
                let bbc = BB::new_rc();
                let bbn = BB::new_rc();

                let r0 = Reg::new();
                Ir::emit(IrEqual, r0.clone(), gen_expr(val, fun), r.clone(), fun);
                Ir::br(r0, Some(Rc::clone(&bbc)), Some(Rc::clone(&bbn)), fun);

                fun.bb_push(bbn);
                switches.borrow_mut().last_mut().unwrap().push(bbc);
            }
            switches.borrow_mut().last_mut().unwrap().reverse();
            jmp(Some(Rc::clone(&bb_break)), Reg::dummy(), fun);
            gen_stmt(body, fun);

            fun.bb_push(bb_break);

            loop_dec();
        }
        NodeType::Case(_, body) => {
            if let Some(bb_case) = get_switches_rc_mut().borrow_mut().last_mut().unwrap().pop() {
                fun.bb_push(bb_case);
                gen_stmt(body, fun);
            } else {
                panic!("gen_ir Case error.");
            }
        }
        NodeType::ArrIni(arrini) => {
            for (lhs, rhs) in arrini {
                let r2 = gen_expr(rhs, fun);
                store(&rhs.nodesctype(None), gen_lval(lhs, fun), r2.clone(), fun);
            }
        }
        NodeType::Break => {
            jmp(Some(Rc::clone(&get_bb_break())), Reg::dummy(), fun);
            fun.bb_push(BB::new_rc());
        }
        NodeType::Continue => {
            jmp(Some(Rc::clone(&get_bb_continue())), Reg::dummy(), fun);
            fun.bb_push(BB::new_rc());
        }
        enode => {
            panic!("unexpeceted node {:?}", enode);
        }
    }
}

// generate IR Vector
pub fn gen_ir(program: &mut Program) {
    *REGNO.lock().unwrap() = 1;

    for funode in &mut program.nodes {
        match &mut funode.op {
            NodeType::Func(_, name, args, body, stacksize) => {
                let mut fun = Function::new(
                    name.clone(),
                    vec![BB::new_rc()],
                    LinkedHashMap::new(),
                    *stacksize,
                );
                for i in 0..args.len() {
                    store_arg(
                        args[i].ctype.size as i32,
                        args[i].offset,
                        i as i32,
                        &mut fun,
                    );
                }
                gen_stmt(body, &mut fun);
                program.funs.push(fun);
            }
            _ => {
                panic!(" should be func node at gen_ir: {:?}", funode);
            }
        }
    }
}
