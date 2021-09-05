use std::rc::Rc;

use inkwell::{builder::Builder, context::Context, module::{Linkage, Module}, values::{FunctionValue, InstructionOpcode}};

use crate::error::CompileError;

pub struct Builtins<'ctx> {
    ctx: &'ctx Context,
    module: Rc<Module<'ctx>>,
    builder: Rc<Builder<'ctx>>,
    _printf: Option<FunctionValue<'ctx>>,
    print_int: Option<FunctionValue<'ctx>>,
    println_int: Option<FunctionValue<'ctx>>,
    print_str: Option<FunctionValue<'ctx>>,
    println_str: Option<FunctionValue<'ctx>>,
    sqrt: Option<FunctionValue<'ctx>>,
    isqrt: Option<FunctionValue<'ctx>>,
}

impl<'ctx> Builtins<'ctx> {
    fn printf(&mut self) -> FunctionValue<'ctx> {
        let f = self._printf.unwrap_or_else(|| {
            let i8p_type = self.ctx.i8_type().ptr_type(inkwell::AddressSpace::Generic);
            let printf_type = self.ctx.i32_type().fn_type(&[i8p_type.into()], true);
            self.module.add_function("printf", printf_type, Some(Linkage::External))
        });
        *self._printf.get_or_insert(f)
    }

    pub fn print_int(&mut self) -> FunctionValue<'ctx> {
        let printf = self.printf();
        let f = self.print_int.unwrap_or_else(|| {
            let t_i64 = self.ctx.i64_type();
            let fn_type = self.ctx.void_type().fn_type(&[t_i64.into(), t_i64.into()], false);
            let f = self.module.add_function(".print_int", fn_type, None);
            let block = self.ctx.append_basic_block(f, "entry");
            self.builder.position_at_end(block);
            let printf_str = self.builder.build_global_string_ptr("%lli + %llii", ".int_format");
            let re = f.get_nth_param(0).unwrap().into_int_value();
            let im = f.get_nth_param(1).unwrap().into_int_value();
            self.builder.build_call(printf, &[printf_str.as_pointer_value().into(), re.into(), im.into()], "call");
            self.builder.build_return(None);
            f
        });
        *self.print_int.get_or_insert(f)
    }

    pub fn println_int(&mut self) -> FunctionValue<'ctx> {
        let printf = self.printf();
        let f = self.println_int.unwrap_or_else(|| {
            let t_i64 = self.ctx.i64_type();
            let fn_type = self.ctx.void_type().fn_type(&[t_i64.into(), t_i64.into()], false);
            let f = self.module.add_function(".println_int", fn_type, None);
            let block = self.ctx.append_basic_block(f, "entry");
            self.builder.position_at_end(block);
            let printf_str = self.builder.build_global_string_ptr("%lli + %llii\n", ".ln_int_format");
            let re = f.get_nth_param(0).unwrap().into_int_value();
            let im = f.get_nth_param(1).unwrap().into_int_value();
            self.builder.build_call(printf, &[printf_str.as_pointer_value().into(), re.into(), im.into()], "call");
            self.builder.build_return(None);
            f
        });
        *self.println_int.get_or_insert(f)
    }

    pub fn print_str(&mut self) -> FunctionValue<'ctx> {
        let printf = self.printf();
        let f = self.print_str.unwrap_or_else(|| {
            let i8p_type = self.ctx.i8_type().ptr_type(inkwell::AddressSpace::Generic);
            let fn_type = self.ctx.void_type().fn_type(&[i8p_type.into()], false);
            let f = self.module.add_function(".print_str", fn_type, None);
            let block = self.ctx.append_basic_block(f, "entry");
            self.builder.position_at_end(block);
            let printf_str = self.builder.build_global_string_ptr("%s", ".str_format");
            let ptr = f.get_nth_param(0).unwrap().into_pointer_value();
            self.builder.build_call(printf, &[printf_str.as_pointer_value().into(), ptr.into()], "call");
            self.builder.build_return(None);
            f
        });
        *self.print_str.get_or_insert(f)
    }

    pub fn println_str(&mut self) -> FunctionValue<'ctx> {
        let printf = self.printf();
        let f = self.println_str.unwrap_or_else(|| {
            let i8p_type = self.ctx.i8_type().ptr_type(inkwell::AddressSpace::Generic);
            let fn_type = self.ctx.void_type().fn_type(&[i8p_type.into()], false);
            let f = self.module.add_function(".println_str", fn_type, None);
            let block = self.ctx.append_basic_block(f, "entry");
            self.builder.position_at_end(block);
            let printf_str = self.builder.build_global_string_ptr("%s\n", ".ln_str_format");
            let ptr = f.get_nth_param(0).unwrap().into_pointer_value();
            self.builder.build_call(printf, &[printf_str.as_pointer_value().into(), ptr.into()], "call");
            self.builder.build_return(None);
            f
        });
        *self.println_str.get_or_insert(f)
    }

    pub fn sqrt(&mut self) -> FunctionValue<'ctx> {
        let f = self.sqrt.unwrap_or_else(|| {
            let t_f64 = self.ctx.f64_type();
            let fn_type = t_f64.fn_type(&[t_f64.into()], false);
            self.module.add_function("sqrt", fn_type, Some(Linkage::External))
        });
        *self.sqrt.get_or_insert(f)
    }

    pub fn isqrt(&mut self) -> Result<FunctionValue<'ctx>, CompileError> {
        let sqrt = self.sqrt();
        let f: Result<_, CompileError> = self.isqrt
            .map(|val| Ok(val))
            .unwrap_or_else(|| {
                let t_f64 = self.ctx.f64_type();
                let t_i64 = self.ctx.i64_type();
                let fn_type = t_i64.fn_type(&[t_i64.into()], false);

                let f = self.module.add_function(".isqrt", fn_type, None);
                let block = self.ctx.append_basic_block(f, "entry");
                self.builder.position_at_end(block);
                let x = f.get_nth_param(0).unwrap().into_int_value();
                let xf = self.builder.build_cast(InstructionOpcode::SIToFP, x, t_f64, "tmp_cast")
                    .into_float_value();
                let res = self.builder.build_call(sqrt, &[xf.into()], "call")
                    .try_as_basic_value().left()
                    .ok_or(CompileError::InvalidState("failed to interpret return value of sqrt"))?
                    .into_float_value();
                let res = self.builder.build_cast(InstructionOpcode::FPToSI, res, t_i64, "tmp_cast")
                    .into_int_value();
                self.builder.build_return(Some(&res));
                Ok(f)
            });
        Ok(*self.isqrt.get_or_insert(f?))
    }

    pub fn new(ctx: &'ctx Context, module: Rc<Module<'ctx>>, builder: Rc<Builder<'ctx>>) -> Self {
        Self {
            ctx, module, builder,
            _printf: None, print_int: None, println_int: None, print_str: None, println_str: None,
            sqrt: None, isqrt: None,
        }
    }
}
