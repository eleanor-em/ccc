use std::{collections::HashMap, intrinsics::transmute, path::Path, rc::Rc};

use inkwell::{IntPredicate, OptimizationLevel, basic_block::BasicBlock, builder::Builder, context::Context, execution_engine::JitFunction, module::Module, values::FunctionValue};

use crate::{ComplexInt, analyse::{ComplexPointer, ComplexValue, Type, Typed}, builtins::Builtins, error::CompileError, parse::{BinOp, Expr, UnOp, Func, Statement}};

struct SymbolTable<'ctx> {
    // TODO: function types
    func_map: HashMap<String, FunctionValue<'ctx>>,
    var_map: HashMap<String, Typed<ComplexPointer<'ctx>>>,
}

impl<'ctx> SymbolTable<'ctx> {
    fn new() -> Self {
        Self { func_map: HashMap::new(), var_map: HashMap::new(), }
    }

    fn add_func(&mut self, name: String, ptr: FunctionValue<'ctx>) {
        self.func_map.insert(name, ptr);
    }
    
    fn func(&self, name: &str) -> Option<&FunctionValue<'ctx>> {
        self.func_map.get(name)
    }

    fn add_var(&mut self, name: String, ptr: ComplexPointer<'ctx>, ty: Type) {
        self.var_map.insert(name, Typed::new(ptr, ty));
    }

    fn var(&self, name: &str) -> Option<&Typed<ComplexPointer<'ctx>>> {
        self.var_map.get(name)
    }
}

fn name_re(name: &str) -> String {
    format!("{}_re", name)
}

fn name_im(name: &str) -> String {
    format!("{}_im", name)
}

pub struct Compiler<'ctx> {
    ctx: &'ctx Context,
    module: Rc<Module<'ctx>>,
    builder: Rc<Builder<'ctx>>,
    builtins: Builtins<'ctx>,
    sym: SymbolTable<'ctx>,
    current_fp: Option<FunctionValue<'ctx>>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(ctx: &'ctx Context) -> Self {
        let module = Rc::new(ctx.create_module("primary"));
        let builder = Rc::new(ctx.create_builder());
        let builtins = Builtins::new(ctx, module.clone(), builder.clone());

        Self {
            ctx, module, builder, builtins,
            sym: SymbolTable::new(),
            current_fp: None,
        }
    }

    fn get_fp(&self) -> Result<FunctionValue<'ctx>, CompileError> {
        self.current_fp.ok_or(CompileError::InvalidState("no active function"))
    }

    fn get_entry_block(&self) -> Result<BasicBlock<'ctx>, CompileError> {
        self.get_fp()?.get_first_basic_block()
            .ok_or(CompileError::InvalidState("no blocks in active function"))
    }

    fn move_to_entry(&mut self) -> Result<(), CompileError> {
        let entry = self.get_entry_block()?;
        match entry.get_first_instruction() {
            Some(ins) => self.builder.position_before(&ins),
            None => self.builder.position_at_end(entry),
        }
        Ok(())
    }

    fn move_to_end(&mut self) -> Result<(), CompileError> {
        self.builder.position_at_end(self.get_entry_block()?);
        Ok(())
    }

    fn complex_imul(&self, lval: ComplexValue<'ctx>, rval: ComplexValue<'ctx>) -> ComplexValue<'ctx> {
        let re1 = self.builder.build_int_mul(lval.re, rval.re, "tmp_imul_re1");
        let re2 = self.builder.build_int_mul(lval.im, rval.im, "tmp_imul_re2");
        let re = self.builder.build_int_sub(re1, re2, "tmp_imul_re");
        let im1 = self.builder.build_int_mul(lval.re, rval.im, "tmp_imul_im1");
        let im2 = self.builder.build_int_mul(lval.im, rval.re, "tmp_imul_im2");
        let im = self.builder.build_int_add(im1, im2, "tmp_imul_im");
        ComplexValue { re, im }
    }

    fn complex_icmp(&mut self, op: IntPredicate, lval: ComplexValue<'ctx>, rval: ComplexValue<'ctx>)
            -> Result<ComplexValue<'ctx>, CompileError> {
        let cmp1 = self.builder.build_int_compare(op, lval.re, rval.re, "tmp_icmp");
        let cmp2 = self.builder.build_int_compare(op, lval.im, rval.im, "tmp_icmp");
        let res = match op {
            IntPredicate::EQ => self.builder.build_and(cmp1, cmp2, "tmp_res"),
            IntPredicate::NE => self.builder.build_or(cmp1, cmp2, "tmp_res"),
            _ => return Err(CompileError::unsupported(format!("{:?}", op)))
        };
        let res = self.builder.build_int_z_extend(res, self.ctx.i64_type(), "tmp_cast");
        Ok(ComplexValue {
            re: res,
            im: self.ctx.i64_type().const_int(0, true),
        })
    }

    #[inline]
    fn complex_conjugate(&self, val: ComplexValue<'ctx>) -> ComplexValue<'ctx> {
        (val.re, self.builder.build_int_neg(val.im, "tmp_ineg")).into()
    }

    fn complex_imodulus(&mut self, val: ComplexValue<'ctx>) -> Result<ComplexValue<'ctx>, CompileError> {
        let conj = self.complex_conjugate(val);
        let modsq = self.complex_imul(val, conj).re;
        let isqrt = self.builtins.isqrt()?;
        self.move_to_end()?;
        let res = self.builder.build_call(isqrt, &[modsq.into()], "tmp_isqrt")
            .try_as_basic_value().left()
                .ok_or(CompileError::InvalidState("failed to interpret return value of isqrt"))?
            .into_int_value();
        Ok(ComplexValue {
            re: res,
            im: self.ctx.i64_type().const_int(0, true),
        })
    }

    fn build_expr(&mut self, expr: Expr) -> Result<ComplexValue<'ctx>, CompileError> {
        match expr {
            Expr::Value(ComplexInt(re, im)) => {
                // Safety: always
                unsafe {
                    let re = transmute(re);
                    let im = transmute(im);
                    let re = self.ctx.i64_type().const_int(re, true);
                    let im = self.ctx.i64_type().const_int(im, true);
                    Ok(ComplexValue { re, im })
                }
            },
            Expr::Id(id) => {
                if let Some(var) = self.sym.var(&id) {
                    let re = self.builder.build_load(var.val().re, &name_re(&id))
                        .into_int_value();
                    let im = self.builder.build_load(var.val().im, &name_im(&id))
                        .into_int_value();
                    Ok(ComplexValue { re, im })
                } else {
                    Err(CompileError::unknown_symbol(id))
                }
            },
            Expr::BinOp(op, boxed) => {
                let (lhs, rhs) = *boxed;
                let lval = self.build_expr(lhs)?;
                let rval = self.build_expr(rhs)?;
                match op {
                    BinOp::Plus => Ok((self.builder.build_int_add(lval.re, rval.re, "tmp_iadd_re"),
                                    self.builder.build_int_add(lval.im, rval.im, "tmp_iadd_im")).into()),
                    BinOp::Minus => Ok((self.builder.build_int_sub(lval.re, rval.re, "tmp_isub_re"),
                                     self.builder.build_int_sub(lval.im, rval.im, "tmp_isub_im")).into()),
                    BinOp::Times => Ok(self.complex_imul(lval, rval)),
                    BinOp::Equals => self.complex_icmp(IntPredicate::EQ, lval, rval),
                    BinOp::NotEquals => self.complex_icmp(IntPredicate::NE, lval, rval),
                    BinOp::Divide => Err(CompileError::not_yet_impl_dbg(op)),
                    BinOp::Remainder => Err(CompileError::not_yet_impl_dbg(op)),
                }
            },
            Expr::UnOp(op, expr) => {
                let val = self.build_expr(*expr)?;
                let res = match op {
                    UnOp::Negate    => (self.builder.build_int_neg(val.re, "tmp_ineg"),
                                        self.builder.build_int_neg(val.im, "tmp_ineg")).into(),
                    UnOp::Conjugate => self.complex_conjugate(val),
                    UnOp::Modulus   => self.complex_imodulus(val)?,
                };
                Ok(res)
            }
            _ => Err(CompileError::not_yet_impl(format!("expression: {:?}", expr))),
        }
    }

    fn build_let_general(&mut self, id: String, value: Expr, ty: Type) -> Result<(), CompileError> {
        // allocate variable memory
        self.move_to_entry()?;
        let re = self.builder.build_alloca(self.ctx.i64_type(), &name_re(&id));
        let im = self.builder.build_alloca(self.ctx.i64_type(), &name_im(&id));

        // assign value
        let value = self.build_expr(value)?;
        self.builder.build_store(re, value.re);
        self.builder.build_store(im, value.im);

        // update symbol table
        self.sym.add_var(id, ComplexPointer { re, im }, ty);
        Ok(())

    }

    fn build_let(&mut self, id: String, value: Expr) -> Result<(), CompileError> {
        self.build_let_general(id, value, Type::IntScalar)
    }

    fn build_let_mut(&mut self, id: String, value: Expr) -> Result<(), CompileError> {
        self.build_let_general(id, value, Type::MutIntScalar)
    }

    fn build_print(&mut self, value: Expr) -> Result<(), CompileError> {
        let value = self.build_expr(value)?;
        self.move_to_end()?;
        let f = self.builtins.print_int();
        self.move_to_end()?;
        self.builder.build_call(f, &[value.re.into(), value.im.into()], "call");
        Ok(())
    }

    fn build_println(&mut self, value: Expr) -> Result<(), CompileError> {
        let value = self.build_expr(value)?;
        self.move_to_end()?;
        let f = self.builtins.println_int();
        self.move_to_end()?;
        self.builder.build_call(f, &[value.re.into(), value.im.into()], "call");
        Ok(())
    }

    fn build_print_str(&mut self, value: String) -> Result<(), CompileError> {
        self.move_to_end()?;
        let f = self.builtins.print_str();
        self.move_to_end()?;
        let ptr = self.builder.build_global_string_ptr(&value, ".str_arg").as_pointer_value();
        self.builder.build_call(f, &[ptr.into()], "call");
        Ok(())
    }

    fn build_println_str(&mut self, value: String) -> Result<(), CompileError> {
        self.move_to_end()?;
        let f = self.builtins.println_str();
        self.move_to_end()?;
        let ptr = self.builder.build_global_string_ptr(&value, ".str_arg").as_pointer_value();
        self.builder.build_call(f, &[ptr.into()], "call");
        Ok(())
    }

    fn build_assign(&mut self, id: String, expr: Expr) -> Result<(), CompileError> {
        let val = self.build_expr(expr)?;

        if let Some(var) = self.sym.var(&id) {
            if var.mutable() {
                self.builder.build_store(var.val().re, val.re);
                self.builder.build_store(var.val().im, val.im);
                Ok(())
            } else {
                Err(CompileError::Immutable(id))
            }
        } else {
            Err(CompileError::UnknownSymbol(id))
        }
    }

    fn build_statement(&mut self, statement: Statement) -> Result<(), CompileError> {
        match statement {
            Statement::Let(name, expr) => self.build_let(name, expr),
            Statement::LetMut(name, expr) => self.build_let_mut(name, expr),
            Statement::Print(expr) => self.build_print(expr),
            Statement::PrintLn(expr) => self.build_println(expr),
            Statement::PrintLit(val) => self.build_print_str(val),
            Statement::PrintLitLn(val) => self.build_println_str(val),
            Statement::Assign(id, expr) => self.build_assign(id, expr),
            _ => Err(CompileError::not_yet_impl(format!("statement: {:?}", statement))),
        }
    }

    fn build_func(&mut self, func: Func) -> Result<(), CompileError> {
        let fn_type = self.ctx.void_type().fn_type(&[], false);
        let fp = self.module.add_function(&func.name, fn_type, None);
        self.sym.add_func(func.name, fp);

        self.ctx.append_basic_block(fp, "entry");
        self.current_fp = Some(fp);
        
        for statement in func.body {
            self.build_statement(statement)?;
        }
        self.builder.build_return(None);

        Ok(())
    }

    /// Prints the compiled LLVM IR to a file.
    fn print_to_file<P: AsRef<Path>>(&self, dest: P) -> Result<(), CompileError> {
        self.module.verify()?;
        self.module.print_to_file(dest)?;
        Ok(())
    }

    /// Executes the compiled code. Fails if there is no `main` function defined.
    fn exec(&self) -> Result<(), CompileError> {
        if self.sym.func("main").is_some() {
            let exec_engine = self.module.create_jit_execution_engine(OptimizationLevel::Aggressive)?;
            let exec: JitFunction<unsafe extern "C" fn()> = unsafe { exec_engine.get_function("main")? };
            unsafe { exec.call() };
            Ok(())
        } else {
            Err(CompileError::NoMain)
        }
    }
}

pub fn run<P: AsRef<Path>>(dest: P, func: Func) -> Result<(), CompileError> {
    let ctx = Context::create();
    let mut gen = Compiler::new(&ctx);
    gen.build_func(func)?;
    gen.print_to_file(dest)?;
    eprintln!("Executing program...\n---");
    gen.exec()
}
