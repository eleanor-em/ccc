use std::{collections::HashMap, path::Path, rc::Rc};

use inkwell::{FloatPredicate, OptimizationLevel, basic_block::BasicBlock, builder::Builder, context::Context, execution_engine::JitFunction, module::Module, values::{FunctionValue, InstructionOpcode}};

use crate::{analyse::{Complex, ComplexPointer, ComplexValue, Located, Location, Type, Typed}, builtins::Builtins, error::{LocatedCompileError, InternalError}, parse::{BinOp, Expr, UnOp, Func, Statement}, util::ComplexNum};

struct SymbolTable<'ctx> {
    // TODO: function types
    func_map: HashMap<String, FunctionValue<'ctx>>,
    var_map: HashMap<String, Located<Typed<ComplexPointer<'ctx>>>>,
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

    fn add_var(&mut self, name: Located<String>, ptr: ComplexPointer<'ctx>, ty: Type) {
        let pos = name.pos();
        self.var_map.insert(name.val(), Located::new(Typed::new(ptr, ty), pos));
    }

    fn var(&self, name: &str) -> Option<&Located<Typed<ComplexPointer<'ctx>>>> {
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

    fn get_fp(&self) -> Result<FunctionValue<'ctx>, LocatedCompileError> {
        self.current_fp.ok_or_else(|| InternalError::invalid_state("no active function"))
    }

    fn get_entry_block(&self) -> Result<BasicBlock<'ctx>, LocatedCompileError> {
        self.get_fp()?.get_first_basic_block()
            .ok_or_else(|| InternalError::invalid_state("no blocks in active function"))
    }

    fn move_to_entry(&mut self) -> Result<(), LocatedCompileError> {
        let entry = self.get_entry_block()?;
        match entry.get_first_instruction() {
            Some(ins) => self.builder.position_before(&ins),
            None => self.builder.position_at_end(entry),
        }
        Ok(())
    }

    fn move_to_end(&mut self) -> Result<(), LocatedCompileError> {
        self.builder.position_at_end(self.get_entry_block()?);
        Ok(())
    }

    fn complex_mul(&self, lval: ComplexValue<'ctx>, rval: ComplexValue<'ctx>) -> ComplexValue<'ctx> {
        let re1 = self.builder.build_float_mul(lval.re, rval.re, "tmp_mul_re1");
        let re2 = self.builder.build_float_mul(lval.im, rval.im, "tmp_mul_re2");
        let re = self.builder.build_float_sub(re1, re2, "tmp_mul_re");
        let im1 = self.builder.build_float_mul(lval.re, rval.im, "tmp_mul_im1");
        let im2 = self.builder.build_float_mul(lval.im, rval.re, "tmp_mul_im2");
        let im = self.builder.build_float_add(im1, im2, "tmp_mul_im");
        ComplexValue { re, im }
    }

    fn complex_cmp(&mut self, pos: Location, op: FloatPredicate, lval: ComplexValue<'ctx>, rval: ComplexValue<'ctx>)
            -> Result<ComplexValue<'ctx>, LocatedCompileError> {
        let cmp1 = self.builder.build_float_compare(op, lval.re, rval.re, "tmp_cmp1");
        let cmp2 = self.builder.build_float_compare(op, lval.im, rval.im, "tmp_cmp2");
        let res = match op {
            FloatPredicate::OEQ => self.builder.build_and(cmp1, cmp2, "tmp_res"),
            FloatPredicate::ONE => self.builder.build_or(cmp1, cmp2, "tmp_res"),
            _                   => return Err(LocatedCompileError::unsupported(pos, format!("{:?}", op)))
        };
        let res = self.builder.build_int_z_extend(res, self.ctx.i64_type(), "tmp_cast");
        let res = self.builder.build_cast(InstructionOpcode::SIToFP, res, self.ctx.f64_type(), "tmp_castf")
            .into_float_value();
        Ok(ComplexValue {
            re: res,
            im: self.ctx.f64_type().const_zero(),
        })
    }

    #[inline]
    fn complex_conjugate(&self, val: ComplexValue<'ctx>) -> ComplexValue<'ctx> {
        (val.re, self.builder.build_float_neg(val.im, "tmp_neg")).into()
    }

    fn complex_modulus(&mut self, val: ComplexValue<'ctx>) -> Result<ComplexValue<'ctx>, LocatedCompileError> {
        let conj = self.complex_conjugate(val);
        let modsq = self.complex_mul(val, conj).re;
        let sqrt = self.builtins.sqrt();
        self.move_to_end()?;
        let res = self.builder.build_call(sqrt, &[modsq.into()], "tmp_sqrt")
            .try_as_basic_value().left()
                .ok_or_else(|| InternalError::invalid_state("failed to interpret return value of sqrt"))?
            .into_float_value();
        Ok(ComplexValue {
            re: res,
            im: self.ctx.f64_type().const_zero(),
        })
    }

    fn build_expr(&mut self, expr: Located<Expr>) -> Result<ComplexValue<'ctx>, LocatedCompileError> {
        let (expr, pos) = expr.unwrap();
        match expr {
            Expr::Value(ComplexNum(re, im)) => {
                let re = self.ctx.f64_type().const_float(re);
                let im = self.ctx.f64_type().const_float(im);
                Ok(ComplexValue { re, im })
            },
            Expr::Id(id) => {
                if let Some(var) = self.sym.var(id.borrow_val()) {
                    let re = self.builder.build_load(var.re(), &name_re(id.borrow_val()))
                        .into_float_value();
                    let im = self.builder.build_load(var.im(), &name_im(id.borrow_val()))
                        .into_float_value();
                    Ok(ComplexValue { re, im })
                } else {
                    Err(LocatedCompileError::unknown_symbol(id))
                }
            },
            Expr::BinOp(op, boxed) => {
                let (lhs, rhs) = *boxed;
                let lval = self.build_expr(lhs)?;
                let rval = self.build_expr(rhs)?;
                
                match op {
                    BinOp::Plus      => Ok((self.builder.build_float_add(lval.re, rval.re, "tmp_add_re"),
                                            self.builder.build_float_add(lval.im, rval.im, "tmp_add_im")).into()),
                    BinOp::Minus     => Ok((self.builder.build_float_sub(lval.re, rval.re, "tmp_sub_re"),
                                            self.builder.build_float_sub(lval.im, rval.im, "tmp_sub_im")).into()),
                    BinOp::Times     => Ok(self.complex_mul(lval, rval)),
                    BinOp::Equals    => self.complex_cmp(pos, FloatPredicate::OEQ, lval, rval),
                    BinOp::NotEquals => self.complex_cmp(pos, FloatPredicate::ONE, lval, rval),
                    BinOp::Divide    => Err(LocatedCompileError::not_yet_impl(pos, "`/`")),
                    BinOp::Remainder => Err(LocatedCompileError::not_yet_impl(pos, "`%`")),
                    BinOp::Power     => Err(LocatedCompileError::not_yet_impl(pos, "`**`")),
                }
            },
            Expr::UnOp(op, expr) => {
                let val = self.build_expr(*expr)?;
                match op {
                    UnOp::Negate    => Ok((self.builder.build_float_neg(val.re, "tmp_neg_re"),
                                           self.builder.build_float_neg(val.im, "tmp_neg_im")).into()),
                    UnOp::Conjugate => Ok(self.complex_conjugate(val)),
                    UnOp::Modulus   => self.complex_modulus(val),
                }
            },
            _ => Err(LocatedCompileError::not_yet_impl(pos, format!("expression: {:?}", expr))),
        }
    }

    fn build_let_general(&mut self, pos: Location, id: Located<String>, expr: Located<Expr>, ty: Type) -> Result<(), LocatedCompileError> {
        // allocate variable memory
        self.move_to_entry()?;
        let re = self.builder.build_alloca(self.ctx.f64_type(), &name_re(id.borrow_val()));
        let im = self.builder.build_alloca(self.ctx.f64_type(), &name_im(id.borrow_val()));

        // assign value
        let value = self.build_expr(expr)?;
        self.builder.build_store(re, value.re);
        self.builder.build_store(im, value.im);

        // update symbol table
        self.sym.add_var(Located::new(id.val(), pos), ComplexPointer { re, im }, ty);
        Ok(())

    }

    fn build_let(&mut self, pos: Location, id: Located<String>, expr: Located<Expr>) -> Result<(), LocatedCompileError> {
        self.build_let_general(pos, id, expr, Type::Scalar)
    }

    fn build_let_mut(&mut self, pos: Location, id: Located<String>, expr: Located<Expr>) -> Result<(), LocatedCompileError> {
        self.build_let_general(pos, id, expr, Type::MutScalar)
    }

    fn build_print(&mut self, expr: Located<Expr>) -> Result<(), LocatedCompileError> {
        let value = self.build_expr(expr)?;
        self.move_to_end()?;
        let f = self.builtins.print_float();
        self.move_to_end()?;
        self.builder.build_call(f, &[value.re.into(), value.im.into()], "call");
        Ok(())
    }

    fn build_println(&mut self, expr: Located<Expr>) -> Result<(), LocatedCompileError> {
        let value = self.build_expr(expr)?;
        self.move_to_end()?;
        let f = self.builtins.println_float();
        self.move_to_end()?;
        self.builder.build_call(f, &[value.re.into(), value.im.into()], "call");
        Ok(())
    }

    fn build_print_str(&mut self, value: String) -> Result<(), LocatedCompileError> {
        self.move_to_end()?;
        let f = self.builtins.print_str();
        self.move_to_end()?;
        let ptr = self.builder.build_global_string_ptr(&value, ".str_arg").as_pointer_value();
        self.builder.build_call(f, &[ptr.into()], "call");
        Ok(())
    }

    fn build_println_str(&mut self, value: String) -> Result<(), LocatedCompileError> {
        self.move_to_end()?;
        let f = self.builtins.println_str();
        self.move_to_end()?;
        let ptr = self.builder.build_global_string_ptr(&value, ".str_arg").as_pointer_value();
        self.builder.build_call(f, &[ptr.into()], "call");
        Ok(())
    }

    fn build_assign(&mut self, statement_pos: Location, id: Located<String>, expr: Located<Expr>)
            -> Result<(), LocatedCompileError> {
        let val = self.build_expr(expr)?;

        if let Some(var) = self.sym.var(id.borrow_val()) {
            if var.borrow_val().is_mutable() {
                self.builder.build_store(var.re(), val.re);
                self.builder.build_store(var.im(), val.im);
                Ok(())
            } else {
                Err(LocatedCompileError::immutable(statement_pos, id.val(), var.pos()))
            }
        } else {
            Err(LocatedCompileError::unknown_symbol(id))
        }
    }

    fn build_statement(&mut self, statement: Located<Statement>) -> Result<(), LocatedCompileError> {
        let (statement, pos) = statement.unwrap();
        match statement {
            Statement::Let(name, expr) => self.build_let(pos, name, expr),
            Statement::LetMut(name, expr) => self.build_let_mut(pos, name, expr),
            Statement::Print(expr) => self.build_print(expr),
            Statement::PrintLn(expr) => self.build_println(expr),
            Statement::PrintLit(val) => self.build_print_str(val),
            Statement::PrintLitLn(val) => self.build_println_str(val),
            Statement::Assign(id, expr) => self.build_assign(pos, id, expr),
            _ => Err(LocatedCompileError::not_yet_impl(pos, format!("statement: {:?}", statement))),
        }
    }

    fn build_func(&mut self, func: Func) -> Result<(), LocatedCompileError> {
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
    fn print_to_file<P: AsRef<Path>>(&self, dest: P) -> Result<(), LocatedCompileError> {
        self.module.verify()?;
        self.module.print_to_file(dest)?;
        Ok(())
    }

    /// Executes the compiled code. Fails if there is no `main` function defined.
    fn exec(&self) -> Result<(), LocatedCompileError> {
        if self.sym.func("main").is_some() {
            let exec_engine = self.module.create_jit_execution_engine(OptimizationLevel::Aggressive)?;
            // Safety: ¯\_(ツ)_/¯
            unsafe {
                let exec: JitFunction<unsafe extern "C" fn()> = exec_engine.get_function("main")?;
                exec.call();
            }
            Ok(())
        } else {
            Err(LocatedCompileError::no_main())
        }
    }
}

pub fn run<P: AsRef<Path>>(dest: P, func: Func) -> Result<(), LocatedCompileError> {
    let ctx = Context::create();
    let mut gen = Compiler::new(&ctx);
    gen.build_func(func)?;
    gen.print_to_file(dest)?;
    eprintln!("Executing program...\n---");
    gen.exec()
}
