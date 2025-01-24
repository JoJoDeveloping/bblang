use std::{
    collections::HashMap,
    fmt::Debug,
    ops::{BitAnd, BitOr, BitXor, Not},
};

use crate::utils::{SwitchToDisplay, string_interner::IStr};

use super::{
    ast::{BinOp, Operand, Pointer, Program, Statement, Terminator, UnOp, Value},
    errors::{MachineError, MachineErrorKind},
    extcalls::ExtcallHandler,
    memory::Memory,
};

#[derive(Debug)]
pub struct CallFrame {
    function: IStr,
    bb: usize,
    offset: usize,
    locals: HashMap<u32, Value>,
}

pub struct MachineState {
    frame: Vec<CallFrame>,
    memory: Memory,
    code: Program,
    extcalls: ExtcallHandler,
}

impl Debug for MachineState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MachineState")
            .field("frame", &self.frame)
            .field("memory", &SwitchToDisplay(&self.memory))
            .finish()
    }
}

impl CallFrame {
    pub fn new(function: IStr, locals: HashMap<u32, Value>) -> Self {
        Self {
            function,
            bb: 0,
            offset: 0,
            locals,
        }
    }
}

impl Terminator {
    fn step(&self, cf: &mut CallFrame) -> Result<(), MachineError> {
        match self {
            Terminator::Jump(nb) => {
                cf.bb = *nb;
                cf.offset = 0;
                Ok(())
            }
            Terminator::CondJump(operand, iftrue, iffalse) => {
                cf.bb = if operand.eval(cf)?.truthiness() {
                    *iftrue
                } else {
                    *iffalse
                };
                cf.offset = 0;
                Ok(())
            }
            Terminator::Return(_) => unreachable!(),
        }
    }
}

impl Value {
    fn truthiness(self) -> bool {
        match self {
            Value::Int(0) => false,
            Value::NullPointer => false,
            _ => true,
        }
    }

    fn eval_unop(self, un_op: UnOp) -> Result<Self, MachineError> {
        Ok(match (un_op, self) {
            (UnOp::Move, x) => x,
            (UnOp::BitwiseNot, Value::Int(x)) => Value::Int(x.not()),
            (UnOp::Negate, Value::Int(x)) => Value::Int(x.wrapping_neg()),
            _ => {
                return Err(MachineError::new(
                    MachineErrorKind::MismatchedArgs,
                    format!(
                        "Unary operator {:?} can not be invoked with {:?}",
                        un_op, self
                    ),
                ));
            }
        })
    }

    fn eval_binop(self, bin_op: BinOp, other: Self) -> Result<Self, MachineError> {
        Ok(match (bin_op, self, other) {
            (BinOp::Add, Value::Int(i1), Value::Int(i2)) => Value::Int(i1.wrapping_add(i2)),
            (BinOp::Sub, Value::Int(i1), Value::Int(i2)) => Value::Int(i1.wrapping_sub(i2)),
            (BinOp::Mul, Value::Int(i1), Value::Int(i2)) => Value::Int(i1.wrapping_mul(i2)),
            (BinOp::Eq, Value::Int(i1), Value::Int(i2)) => Value::Int(if i1 == i2 { 1 } else { 0 }),
            (BinOp::Lt, Value::Int(i1), Value::Int(i2)) => Value::Int(if i1 < i2 { 1 } else { 0 }),
            (BinOp::Le, Value::Int(i1), Value::Int(i2)) => Value::Int(if i1 <= i2 { 1 } else { 0 }),
            (BinOp::BitwiseAnd, Value::Int(i1), Value::Int(i2)) => Value::Int(i1.bitand(i2)),
            (BinOp::BitwiseOr, Value::Int(i1), Value::Int(i2)) => Value::Int(i1.bitor(i2)),
            (BinOp::BitwiseXor, Value::Int(i1), Value::Int(i2)) => Value::Int(i1.bitxor(i2)),
            (BinOp::Add, Value::Loc(Pointer(blk, off)), Value::Int(i2))
            | (BinOp::Add, Value::Int(i2), Value::Loc(Pointer(blk, off))) => Value::Loc(Pointer(
                blk,
                off.checked_add_signed(i2).ok_or_else(|| {
                    MachineError::new(
                        MachineErrorKind::Overflow,
                        format!("Overflow when computing {off} + {i2} in pointer add"),
                    )
                })?,
            )),
            (BinOp::Sub, Value::Loc(Pointer(blk, off)), Value::Int(i2)) => Value::Loc(Pointer(
                blk,
                off.checked_sub_signed(i2).ok_or_else(|| {
                    MachineError::new(
                        MachineErrorKind::Overflow,
                        format!("Overflow when computing {off} - {i2} in pointer sub"),
                    )
                })?,
            )),
            (BinOp::Sub, Value::Loc(Pointer(blk1, off1)), Value::Loc(Pointer(blk2, off2)))
                if blk1 == blk2 =>
            {
                Value::Int(off1.checked_signed_diff(off2).ok_or_else(|| {
                    MachineError::new(
                        MachineErrorKind::Overflow,
                        format!("Overflow when computing {off1} - {off2} in pointer diff"),
                    )
                })?)
            }
            (BinOp::Eq, Value::NullPointer, Value::NullPointer) => Value::Int(1),
            (BinOp::Eq, Value::Loc(_), Value::NullPointer)
            | (BinOp::Eq, Value::NullPointer, Value::Loc(_)) => Value::Int(0),
            (BinOp::Eq, Value::Loc(ptr1), Value::Loc(ptr2)) => {
                Value::Int(if ptr1 == ptr2 { 1 } else { 0 })
            }
            (BinOp::Le, Value::Loc(Pointer(blk1, off1)), Value::Loc(Pointer(blk2, off2)))
                if blk1 == blk2 =>
            {
                Value::Int(if off1 <= off2 { 1 } else { 0 })
            }
            (BinOp::Lt, Value::Loc(Pointer(blk1, off1)), Value::Loc(Pointer(blk2, off2)))
                if blk1 == blk2 =>
            {
                Value::Int(if off1 <= off2 { 1 } else { 0 })
            }
            _ => {
                return Err(MachineError::new(
                    MachineErrorKind::MismatchedArgs,
                    format!(
                        "Binary operator {:?} can not be invoked with {:?} and {:?}",
                        bin_op, self, other
                    ),
                ));
            }
        })
    }

    fn as_pointer(self) -> Result<Pointer, MachineError> {
        match self {
            Value::Loc(pointer) => Ok(pointer),
            _ => Err(MachineError::new(
                MachineErrorKind::MismatchedArgs,
                "expected pointer".to_string(),
            )),
        }
    }

    fn as_integer(self) -> Result<i32, MachineError> {
        match self {
            Value::Int(i) => Ok(i),
            _ => Err(MachineError::new(
                MachineErrorKind::MismatchedArgs,
                "expected integer".to_string(),
            )),
        }
    }
}

impl CallFrame {
    fn set_local(&mut self, loc: u32, to: Value) {
        self.locals.insert(loc, to);
    }
}

impl Statement {
    fn step(&self, cf: &mut CallFrame, mem: &mut Memory) -> Result<(), MachineError> {
        match self {
            Statement::UnOp(res, un_op, operand) => {
                cf.set_local(*res, operand.eval(cf)?.eval_unop(*un_op)?)
            }
            Statement::BinOp(res, bin_op, op1, op2) => {
                cf.set_local(*res, op1.eval(cf)?.eval_binop(*bin_op, op2.eval(cf)?)?)
            }
            Statement::Load(res, operand) => {
                cf.set_local(*res, mem.load(operand.eval(cf)?.as_pointer()?)?)
            }
            Statement::Store(addr, data) => {
                mem.store(addr.eval(cf)?.as_pointer()?, data.eval(cf)?)?
            }
            Statement::FunCall(_, _, _) => unreachable!(),
            Statement::Alloc(res, operand) => {
                let x = operand.eval(cf)?.as_integer()?;
                let len: u32 = x.try_into().map_err(|_| {
                    MachineError::new(
                        MachineErrorKind::Overflow,
                        format!("requested allocation size {x} is negative"),
                    )
                })?;
                let blk = mem.alloc(len).ok_or_else(|| {
                    MachineError::new(
                        MachineErrorKind::MemoryUnsafety,
                        "out of memory".to_string(),
                    )
                })?;
                cf.set_local(*res, Value::Loc(Pointer(blk, 0)));
            }
            Statement::Free(operand) => {
                let ptr = operand.eval(cf)?.as_pointer()?;
                if ptr.1 != 0 {
                    return Err(MachineError::new(
                        MachineErrorKind::MemoryUnsafety,
                        format!("free called with {ptr:?}, which is offset"),
                    ));
                }
                mem.dealloc(ptr.0)?;
            }
        };
        Ok(())
    }
}

impl Operand {
    fn eval(&self, cf: &CallFrame) -> Result<Value, MachineError> {
        match self {
            Operand::Const(value) => Ok(*value),
            Operand::Local(idx) => cf.locals.get(idx).copied().ok_or_else(|| {
                MachineError::new(
                    MachineErrorKind::UnboundObject,
                    format!("local {} does not exist in function {}", *idx, cf.function),
                )
            }),
        }
    }
}

impl MachineState {
    pub fn step(&mut self) -> Result<Option<Value>, MachineError> {
        let Some(cf) = self.frame.last_mut() else {
            return Err(MachineError::new(
                MachineErrorKind::UnboundObject,
                "call frame is empty".to_string(),
            ));
        };
        let cur_func = self.code.funcs.get(&cf.function).ok_or_else(|| {
            MachineError::new(
                MachineErrorKind::UnboundObject,
                format!("function {} does not exist", cf.function),
            )
        })?;
        let cur_bb = cur_func.blocks.get(cf.bb).ok_or_else(|| {
            MachineError::new(
                MachineErrorKind::JumpOutOfBounds,
                format!("block {} does not exist in function {}", cf.bb, cf.function),
            )
        })?;
        if cf.offset > cur_bb.insns.len() {
            return Err(MachineError::new(
                MachineErrorKind::JumpOutOfBounds,
                format!(
                    "block {} in function {} does not have instruction {}",
                    cf.bb, cf.function, cf.offset
                ),
            ));
        } else if cf.offset == cur_bb.insns.len() {
            match cur_bb.term {
                Terminator::Return(operand) => {
                    let rv = operand.eval(cf)?;
                    self.frame.pop();

                    let Some(cf) = self.frame.last_mut() else {
                        // we arrived at the bottom of the call stack, forward result driver
                        return Ok(Some(rv));
                    };
                    let cur_func = self.code.funcs.get(&cf.function).unwrap();
                    let cur_bb = cur_func.blocks.get(cf.bb).unwrap();
                    match cur_bb.insns[cf.offset] {
                        Statement::FunCall(Some(idx), _, _) => {
                            cf.locals.insert(idx, rv);
                        }
                        Statement::FunCall(None, _, _) => {}
                        _ => unreachable!(),
                    }
                    cf.offset += 1;
                }
                _ => cur_bb.term.step(cf)?,
            }
        } else {
            let stmt = &cur_bb.insns[cf.offset];
            match stmt {
                Statement::FunCall(idx, fnname, args) => {
                    let Some(_) = self.code.funcs.get(fnname) else {
                        let args: Result<Vec<_>, _> = args.iter().map(|x| x.eval(cf)).collect();
                        let args = args?;
                        let rv = self.extcalls.handle(&mut self.memory, *fnname, args)?;
                        if let Some(idx) = idx {
                            cf.locals.insert(*idx, rv);
                        }
                        cf.offset += 1;
                        return Ok(None);
                    };
                    let mut ncf = CallFrame::new(*fnname, HashMap::new());
                    for (idx, arg) in args.iter().enumerate() {
                        let idx: u32 = idx.try_into().map_err(|_| {
                            MachineError::new(
                                MachineErrorKind::Overflow,
                                "Function called with too many arguments".to_string(),
                            )
                        })?;
                        let arg = arg.eval(cf)?;
                        ncf.locals.insert(idx, arg);
                    }
                    self.frame.push(ncf);
                    return Ok(None);
                }
                _ => {
                    stmt.step(cf, &mut self.memory)?;
                    cf.offset += 1
                }
            }
        }
        Ok(None)
    }

    pub fn run(&mut self) -> Result<Value, MachineError> {
        let mut _step = 0;
        loop {
            _step += 1;
            return match self.step() {
                Ok(None) => continue,
                Ok(Some(x)) => Ok(x),
                Err(x) => Err(x),
            };
        }
    }

    pub fn start(code: Program, func: IStr, args: Vec<Value>) -> Self {
        let locals = args
            .into_iter()
            .enumerate()
            .map(|(x, y)| (x.try_into().unwrap(), y))
            .collect();
        Self {
            frame: vec![CallFrame::new(func, locals)],
            memory: Memory::new(),
            code,
            extcalls: ExtcallHandler::new(),
        }
    }
}
