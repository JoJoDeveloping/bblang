use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    mem,
    ops::{BitAnd, BitOr, BitXor, Not},
    rc::Rc,
};

use crate::{
    specs::exec::{
        self, ExecCtx, ExecLocalCtx, InfPointer,
        monad::{MonadFailMode, MonadOwnershipArg, SpecMonad},
    },
    utils::{
        SwitchToDisplay,
        string_interner::{IStr, intern},
    },
};

use crate::ownership::OwnershipInfo;

use super::{
    ast::{
        BinOp, CompiledFunctionSpec, Operand, Pointer, Program, Statement, Terminator, UnOp, Value,
    },
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
    spec_stuff: (ExecLocalCtx<'static>, OwnershipInfo),
}

pub struct MachineState {
    frame: Vec<CallFrame>,
    memory: Memory,
    code: Program,
    extcalls: ExtcallHandler,
    specs_stuff: ExecCtx,
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
            spec_stuff: (ExecLocalCtx::new(), OwnershipInfo::new()),
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

pub fn value_to_value(value: Value) -> Rc<exec::Value> {
    Rc::new(match value {
        Value::Int(i) => exec::Value::InductiveVal {
            ty: intern("Val"),
            constr: intern("Int"),
            args: vec![Rc::new(exec::Value::NumValue(i.into()))],
        },
        Value::Loc(Pointer(blk, off)) => exec::Value::InductiveVal {
            ty: intern("Val"),
            constr: intern("Ptr"),
            args: vec![Rc::new(exec::Value::InductiveVal {
                ty: intern("Option"),
                constr: intern("Some"),
                args: vec![Rc::new(exec::Value::PtrValue(InfPointer(blk, off.into())))],
            })],
        },
        Value::NullPointer => exec::Value::InductiveVal {
            ty: intern("Val"),
            constr: intern("Ptr"),
            args: vec![Rc::new(exec::Value::InductiveVal {
                ty: intern("Option"),
                constr: intern("None"),
                args: vec![],
            })],
        },
    })
}

impl CallFrame {
    fn set_local(&mut self, loc: u32, to: Value) {
        self.locals.insert(loc, to);
    }

    fn execute_enter_spec(
        &mut self,
        old_ownership: &mut OwnershipInfo,
        global: &ExecCtx,
        prog: &CompiledFunctionSpec,
        args: Vec<Value>,
        global_mem: &Memory,
    ) -> SpecMonad<()> {
        let combined = MonadOwnershipArg {
            extract_from: old_ownership,
            extract_into: &mut self.spec_stuff.1,
            mode: MonadFailMode::Assume,
            memory: global_mem,
        };
        assert_eq!(args.len(), prog.arg_names.len());
        for (name, arg) in prog.arg_names.iter().copied().zip(args.into_iter()) {
            self.spec_stuff.0.push_arg(name, value_to_value(arg));
        }
        self.spec_stuff
            .0
            .execute_const_defs_as_lets(global, &mut Some(combined), &prog.pre)?;
        // println!(
        //     "Having just called {}, having ownership {:?}",
        //     self.function, self.spec_stuff.1
        // );
        Ok(())
    }

    fn execute_leave_spec(
        &mut self,
        parent_ownership: &mut OwnershipInfo,
        global: &ExecCtx,
        prog: &CompiledFunctionSpec,
        global_mem: &Memory,
        result: Value,
    ) -> SpecMonad<()> {
        // println!(
        //     "Before returning from {}, having ownership {:?}",
        //     self.function, self.spec_stuff.1
        // );
        let combined = MonadOwnershipArg {
            extract_from: &mut self.spec_stuff.1,
            extract_into: parent_ownership,
            mode: MonadFailMode::Assert,
            memory: global_mem,
        };
        self.spec_stuff
            .0
            .push_arg(intern("result"), value_to_value(result));
        self.spec_stuff
            .0
            .execute_const_defs_as_lets(global, &mut Some(combined), &prog.post)?;
        if self.spec_stuff.1.leaks() {
            println!(
                "Upon returning from {}, leaking ownership {:?}",
                self.function, self.spec_stuff.1
            );
        }
        Ok(())
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
                let ptr = operand.eval(cf)?.as_pointer()?;
                if !cf.spec_stuff.1.ensure(&InfPointer(ptr.0, ptr.1.into())) {
                    panic!("Read memory at {ptr:?} without owning it!")
                }
                cf.set_local(*res, mem.load(ptr)?)
            }
            Statement::Store(addr, data) => {
                let ptr = addr.eval(cf)?.as_pointer()?;
                if !cf.spec_stuff.1.ensure(&InfPointer(ptr.0, ptr.1.into())) {
                    panic!("Wrote memory at {ptr:?} without owning it!")
                }
                mem.store(ptr, data.eval(cf)?)?
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
                for i in 0..len {
                    cf.spec_stuff.1.insert(&InfPointer(blk, i.into()));
                }
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
                let waslen = mem.dealloc(ptr.0)?;

                for i in 0..waslen {
                    let ptr = InfPointer(ptr.0, i.into());
                    if !cf.spec_stuff.1.remove(&ptr) {
                        panic!("Freed memory at {ptr:?} without owning it!")
                    }
                }
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
                    let mut old_frame = self.frame.pop().unwrap();

                    let Some(cf) = self.frame.last_mut() else {
                        // we arrived at the bottom of the call stack, forward result driver
                        return Ok(Some(rv));
                    };

                    if let Some(spec) = &cur_func.spec {
                        let spec = old_frame.execute_leave_spec(
                            &mut cf.spec_stuff.1,
                            &self.specs_stuff,
                            spec,
                            &self.memory,
                            rv.clone(),
                        );

                        if let Err(e) = spec {
                            panic!("Postcondition of {} violated: {:?}", old_frame.function, e);
                        }
                    } else {
                        cf.spec_stuff.1 = old_frame.spec_stuff.1;
                    }

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
                    let Some(new_func) = self.code.funcs.get(fnname) else {
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
                    let mut newargs = Vec::new();
                    for (idx, arg) in args.iter().enumerate() {
                        let idx: u32 = idx.try_into().map_err(|_| {
                            MachineError::new(
                                MachineErrorKind::Overflow,
                                "Function called with too many arguments".to_string(),
                            )
                        })?;
                        let arg = arg.eval(cf)?;
                        newargs.push(arg);
                        ncf.locals.insert(idx, arg);
                    }
                    if let Some(spec) = &new_func.spec {
                        let spec = ncf.execute_enter_spec(
                            &mut cf.spec_stuff.1,
                            &self.specs_stuff,
                            spec,
                            newargs,
                            &self.memory,
                        );
                        if let Err(e) = spec {
                            panic!("Precondition of {} violated: {:?}", ncf.function, e);
                        }
                    } else {
                        let oi = mem::replace(&mut cf.spec_stuff.1, OwnershipInfo::new());
                        ncf.spec_stuff = (ExecLocalCtx::new(), oi);
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

    pub fn start(mut code: Program, func: IStr, args: Vec<Value>) -> Self {
        let locals = args
            .into_iter()
            .enumerate()
            .map(|(x, y)| (x.try_into().unwrap(), y))
            .collect();
        let mut execctx = ExecCtx::new();
        execctx
            .consume_globals(&mut None, mem::take(&mut code.spec_stuff.1))
            .unwrap();
        Self {
            frame: vec![CallFrame::new(func, locals)],
            memory: Memory::new(),
            code,
            extcalls: ExtcallHandler::new(),
            specs_stuff: execctx,
        }
    }
}

impl Display for MachineState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Stack trace:\n")?;
        for (idx, frame) in self.frame.iter().enumerate() {
            write!(
                f,
                "{idx:>5}: {} ({}: {})\n",
                frame.function, frame.bb, frame.offset
            )?;
            let Some(func) = self.code.funcs.get(&frame.function) else {
                write!(f, "      function does not exist!\n ")?;
                continue;
            };
            write!(f, "         locals:")?;
            for (idx, name) in func.debug_info.local_names.iter() {
                write!(f, " {name}=")?;
                match frame.locals.get(idx) {
                    Some(x) => write!(f, "{x:?} ")?,
                    None => write!(f, "? ")?,
                };
            }
            write!(f, "\n         position: ")?;
            match func.debug_info.data.get(&(frame.bb, frame.offset)) {
                Some((p1, p2)) => write!(f, "{p1} to {p2}")?,
                None => write!(f, "?")?,
            }
            write!(f, "\n")?;
        }
        write!(f, "Memory dump: \n{:#}", self.memory)?;
        Ok(())
    }
}
