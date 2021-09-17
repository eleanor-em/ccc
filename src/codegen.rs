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

pub struct Config {
    accurate_div: bool,
    newton_rhapson_passes: usize,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            accurate_div: false,
            newton_rhapson_passes: 10
        }
    }
}

pub struct Compiler<'ctx> {
    config: Config,
    ctx: &'ctx Context,
    module: Rc<Module<'ctx>>,
    builder: Rc<Builder<'ctx>>,
    builtins: Builtins<'ctx>,
    sym: SymbolTable<'ctx>,
    inside_loop: bool,
    current_fp: Option<FunctionValue<'ctx>>,
    current_block: Option<BasicBlock<'ctx>>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(config: Config, ctx: &'ctx Context) -> Self {
        let module = Rc::new(ctx.create_module("primary"));
        let builder = Rc::new(ctx.create_builder());
        let builtins = Builtins::new(ctx, module.clone(), builder.clone());

        Self {
            config, ctx, module, builder, builtins,
            sym: SymbolTable::new(),
            inside_loop: false,
            current_fp: None,
            current_block: None,
        }
    }

    fn get_fp(&self) -> Result<FunctionValue<'ctx>, LocatedCompileError> {
        self.current_fp.ok_or_else(|| InternalError::invalid_state("no active function"))
    }

    fn get_block(&self) -> Result<BasicBlock<'ctx>, LocatedCompileError> {
        self.current_block.ok_or_else(|| InternalError::invalid_state("no blocks in active function"))
    }

    fn move_to_end(&mut self) -> Result<(), LocatedCompileError> {
        self.builder.position_at_end(self.get_block()?);
        Ok(())
    }

    fn set_and_move_block(&mut self, block: BasicBlock<'ctx>) -> Result<(), LocatedCompileError> {
        self.current_block = Some(block);
        self.move_to_end()
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

    fn complex_div(&self, lval: ComplexValue<'ctx>, rval: ComplexValue<'ctx>) -> ComplexValue<'ctx> {
        let re1 = self.builder.build_float_mul(lval.re, rval.re, "tmp_div_re1");
        let re2 = self.builder.build_float_mul(lval.im, rval.im, "tmp_div_re2");
        let re = self.builder.build_float_add(re1, re2, "tmp_div_re");

        let im1 = self.builder.build_float_mul(lval.im, rval.re, "tmp_div_im1");
        let im2 = self.builder.build_float_mul(lval.re, rval.im, "tmp_div_im2");
        let im = self.builder.build_float_sub(im1, im2, "tmp_div_im");

        let c2 = self.builder.build_float_mul(rval.re, rval.re, "tmp_div_c2");
        let d2 = self.builder.build_float_mul(rval.im, rval.im, "tmp_div_d2");
        let denom = self.builder.build_float_add(c2, d2, "tmp_div_denom");
        
        let re = self.builder.build_float_div(re, denom, "tmp_div_re_final");
        let im = self.builder.build_float_div(im, denom, "tmp_div_im_final");
        ComplexValue { re, im }
    }

    // This _may_ turn out to be more accurate for certain inputs.
    #[allow(clippy::many_single_char_names)]
    fn complex_div_accurate(&mut self, lval: ComplexValue<'ctx>, rval: ComplexValue<'ctx>) -> Result<ComplexValue<'ctx>, LocatedCompileError> {
        let fabs = self.builtins.abs();
        let fmin = self.builtins.min();
        let fmax = self.builtins.max();
        self.move_to_end()?;

        let a = lval.re();
        let b = lval.im();
        let c = rval.re();
        let d = rval.im();

        let one = self.ctx.f64_type().const_float(1.);

        let c_abs = self.builder.build_call(fabs, &[c.into()], "tmp_cabs")
            .try_as_basic_value().left()
                .ok_or_else(|| InternalError::invalid_state("failed to interpret return value of abs"))?
            .into_float_value();

        let d_abs = self.builder.build_call(fabs, &[d.into()], "tmp_dabs")
            .try_as_basic_value().left()
                .ok_or_else(|| InternalError::invalid_state("failed to interpret return value of abs"))?
            .into_float_value();

        let min = self.builder.build_call(fmin, &[c_abs.into(), d_abs.into()], "tmp_min")
            .try_as_basic_value().left()
                .ok_or_else(|| InternalError::invalid_state("failed to interpret return value of min"))?
            .into_float_value();

        let max = self.builder.build_call(fmax, &[c_abs.into(), d_abs.into()], "tmp_max")
            .try_as_basic_value().left()
                .ok_or_else(|| InternalError::invalid_state("failed to interpret return value of max"))?
            .into_float_value();
        
        /*
          Where |c| < |d|:
          1/hypot(c, d) = 1/(|d| hypot(1, |c/d|)) = 1/|d| * fastInvSqrt(1+|c/d|^2)
         */
        let x = self.builder.build_float_div(min, max, "tmp_opp");
        let x = self.builder.build_float_mul(x, x, "tmp_sqr");
        let x = self.builder.build_float_add(one, x, "tmp_x");

        // https://stackoverflow.com/questions/11644441/fast-inverse-square-root-on-x64/11644533
        let x2 = self.builder.build_float_mul(x, self.ctx.f64_type().const_float(0.5), "tmp_x2");
        let i = self.builder.build_bitcast(x, self.ctx.i64_type(), "tmp_ftoi")
            .into_int_value();
        let i = self.builder.build_int_signed_div(i, self.ctx.i64_type().const_int(2, true), "tmp_shr");
        let i = self.builder.build_int_sub(self.ctx.i64_type().const_int(0x5fe6eb50c7b537a9, true), i, "tmp_wtf");
        let x = self.builder.build_bitcast(i, self.ctx.f64_type(), "tmp_itof")
            .into_float_value();
        // Newton-Rhapson iteration
        let mut y = x;
        let mut x = self.builder.build_float_mul(y, y, "tmp_nr1");
        x = self.builder.build_float_mul(x2, x, "tmp_nr2");
        x = self.builder.build_float_sub(self.ctx.f64_type().const_float(1.5), x, "tmp_nr3");
        x = self.builder.build_float_mul(y, x, "tmp_nr4");

        for _ in 1..self.config.newton_rhapson_passes {
            y = x;
            x = self.builder.build_float_mul(y, y, "tmp_nr1");
            x = self.builder.build_float_mul(x2, x, "tmp_nr2");
            x = self.builder.build_float_sub(self.ctx.f64_type().const_float(1.5), x, "tmp_nr3");
            x = self.builder.build_float_mul(y, x, "tmp_nr4");
        }

        let denom = self.builder.build_float_div(x, max, "tmp_hypot");

        /*
            denom = 1. / hypot(c, d)
            a *= denom
            b *= denom
            c *= denom
            d *= denom
            a * c + b * d  + i ( b * c - a * d )
         */

        let a = self.builder.build_float_mul(a, denom, "tmp_ad");
        let b = self.builder.build_float_mul(b, denom, "tmp_bd");
        let c = self.builder.build_float_mul(c, denom, "tmp_cd");
        let d = self.builder.build_float_mul(d, denom, "tmp_dd");
        
        let x = self.builder.build_float_mul(a, c, "tmp_f1_re");
        let y = self.builder.build_float_mul(b, d, "tmp_f2_re");
        let re = self.builder.build_float_add(x, y, "tmp_f3_re");
        
        let xi = self.builder.build_float_mul(b, c, "tmp_f1_im");
        let yi = self.builder.build_float_mul(a, d, "tmp_f2_im");
        let im = self.builder.build_float_sub(xi, yi, "tmp_f3_im");

        Ok(ComplexValue { re, im })
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
                    BinOp::Divide    => {
                        if self.config.accurate_div {
                            self.complex_div_accurate(lval, rval)
                        } else {
                            Ok(self.complex_div(lval, rval))
                        }
                    },
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
            Expr::IfElse(boxed) => {
                let (cond, value_if, value_else) = *boxed;
                let cond = self.build_expr(cond)?;

                let then_bb = self.ctx.append_basic_block(self.get_fp()?, "then");
                let else_bb = self.ctx.append_basic_block(self.get_fp()?, "else");
                let cont_bb = self.ctx.append_basic_block(self.get_fp()?, "count");
                
                let re =  self.builder.build_float_compare(FloatPredicate::ONE, cond.re(), self.ctx.f64_type().const_zero(), "test_re");
                let im =  self.builder.build_float_compare(FloatPredicate::ONE, cond.im(), self.ctx.f64_type().const_zero(), "test_im");
                let cond = self.builder.build_or(re, im, "test");
                self.builder.build_conditional_branch(cond, then_bb, else_bb);

                self.set_and_move_block(then_bb)?;
                let value_if = self.build_expr(value_if)?;
                self.builder.build_unconditional_branch(cont_bb);

                self.set_and_move_block(else_bb)?;
                let value_else = self.build_expr(value_else)?;
                self.builder.build_unconditional_branch(cont_bb);

                self.set_and_move_block(cont_bb)?;
                let phi_re = self.builder.build_phi(self.ctx.f64_type(), "iftmp_re");
                phi_re.add_incoming(&[(&value_if.re(), then_bb), (&value_else.re(), else_bb)]);
                let phi_im = self.builder.build_phi(self.ctx.f64_type(), "iftmp_im");
                phi_im.add_incoming(&[(&value_if.im(), then_bb), (&value_else.im(), else_bb)]);

                let re = phi_re.as_basic_value().into_float_value();
                let im = phi_im.as_basic_value().into_float_value();
                Ok(ComplexValue { re, im })
            }
        }
    }

    fn build_let_general(&mut self, pos: Location, id: Located<String>, expr: Located<Expr>, ty: Type) -> Result<(), LocatedCompileError> {
        // allocate variable memory
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
            Statement::AddAssign(id, rhs) =>
                self.build_assign(pos, id.clone(), 
                    Located::new(Expr::BinOp(
                        BinOp::Plus,
                        Box::new((Located::new(Expr::Id(id), pos), rhs))), pos)),
            Statement::SubAssign(id, rhs) =>
                self.build_assign(pos, id.clone(), 
                    Located::new(Expr::BinOp(
                        BinOp::Minus,
                        Box::new((Located::new(Expr::Id(id), pos), rhs))), pos)),
            Statement::MulAssign(id, rhs) =>
                self.build_assign(pos, id.clone(), 
                    Located::new(Expr::BinOp(
                        BinOp::Times,
                        Box::new((Located::new(Expr::Id(id), pos), rhs))), pos)),
            Statement::DivAssign(id, rhs) =>
                self.build_assign(pos, id.clone(), 
                    Located::new(Expr::BinOp(
                        BinOp::Divide,
                        Box::new((Located::new(Expr::Id(id), pos), rhs))), pos)),
            Statement::ModAssign(id, rhs) =>
                self.build_assign(pos, id.clone(), 
                    Located::new(Expr::BinOp(
                        BinOp::Remainder,
                        Box::new((Located::new(Expr::Id(id), pos), rhs))), pos)),
            // Statement::If(_, _) => todo!(),
            // Statement::IfElse(_, _, _) => todo!(),
            // Statement::While(_, _) => todo!(),
            Statement::Break => {
                if !self.inside_loop {
                    Err(LocatedCompileError::not_inside_loop(Located::new("break".to_owned(), pos)))
                } else {
                    Err(LocatedCompileError::not_yet_impl(pos, format!("statement: {:?}", statement)))
                }
            },
            Statement::Continue => {
                if !self.inside_loop {
                    Err(LocatedCompileError::not_inside_loop(Located::new("continue".to_owned(), pos)))
                } else {
                    Err(LocatedCompileError::not_yet_impl(pos, format!("statement: {:?}", statement)))
                }
            },
            _ => Err(LocatedCompileError::not_yet_impl(pos, format!("statement: {:?}", statement))),
        }
    }

    fn build_func(&mut self, func: Func) -> Result<(), LocatedCompileError> {
        let fn_type = self.ctx.void_type().fn_type(&[], false);
        let fp = self.module.add_function(&func.name, fn_type, None);
        self.sym.add_func(func.name, fp);

        let block = self.ctx.append_basic_block(fp, "entry");
        self.current_fp = Some(fp);
        self.set_and_move_block(block)?;
        
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
    let mut gen = Compiler::new(Config::default(), &ctx);
    gen.build_func(func)?;
    gen.print_to_file(dest)?;
    eprintln!("Executing program...\n---");
    gen.exec()
}
