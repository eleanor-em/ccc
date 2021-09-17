use std::rc::Rc;

use inkwell::{builder::Builder, context::Context, module::{Linkage, Module}, values::FunctionValue};

pub struct Builtins<'ctx> {
    ctx: &'ctx Context,
    module: Rc<Module<'ctx>>,
    builder: Rc<Builder<'ctx>>,
    _printf: Option<FunctionValue<'ctx>>,
    print_float: Option<FunctionValue<'ctx>>,
    println_float: Option<FunctionValue<'ctx>>,
    print_str: Option<FunctionValue<'ctx>>,
    println_str: Option<FunctionValue<'ctx>>,
    sqrt: Option<FunctionValue<'ctx>>,
    min: Option<FunctionValue<'ctx>>,
    max: Option<FunctionValue<'ctx>>,
    abs: Option<FunctionValue<'ctx>>,
    log: Option<FunctionValue<'ctx>>,
    cos: Option<FunctionValue<'ctx>>,
    sin: Option<FunctionValue<'ctx>>,
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

    pub fn print_float(&mut self) -> FunctionValue<'ctx> {
        let printf = self.printf();
        let f = self.print_float.unwrap_or_else(|| {
            let t_f64 = self.ctx.f64_type();
            let fn_type = self.ctx.void_type().fn_type(&[t_f64.into(), t_f64.into()], false);
            let f = self.module.add_function(".print_float", fn_type, None);
            let block = self.ctx.append_basic_block(f, "entry");
            self.builder.position_at_end(block);
            let printf_str = self.builder.build_global_string_ptr("%.12f + %.12fi", ".int_format");
            let re = f.get_nth_param(0).unwrap().into_float_value();
            let im = f.get_nth_param(1).unwrap().into_float_value();
            self.builder.build_call(printf, &[printf_str.as_pointer_value().into(), re.into(), im.into()], "call");
            self.builder.build_return(None);
            f
        });
        *self.print_float.get_or_insert(f)
    }

    pub fn println_float(&mut self) -> FunctionValue<'ctx> {
        let printf = self.printf();
        let f = self.println_float.unwrap_or_else(|| {
            let t_f64 = self.ctx.f64_type();
            let fn_type = self.ctx.void_type().fn_type(&[t_f64.into(), t_f64.into()], false);
            let f = self.module.add_function(".println_float", fn_type, None);
            let block = self.ctx.append_basic_block(f, "entry");
            self.builder.position_at_end(block);
            let printf_str = self.builder.build_global_string_ptr("%.20f + %.20fi\n", ".ln_float_format");
            let re = f.get_nth_param(0).unwrap().into_float_value();
            let im = f.get_nth_param(1).unwrap().into_float_value();
            self.builder.build_call(printf, &[printf_str.as_pointer_value().into(), re.into(), im.into()], "call");
            self.builder.build_return(None);
            f
        });
        *self.println_float.get_or_insert(f)
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
            self.module.add_function("llvm.sqrt.f64", fn_type, Some(Linkage::External))
        });
        *self.sqrt.get_or_insert(f)
    }

    pub fn min(&mut self) -> FunctionValue<'ctx> {
        let f = self.min.unwrap_or_else(|| {
            let t_f64 = self.ctx.f64_type();
            let fn_type = t_f64.fn_type(&[t_f64.into(), t_f64.into()], false);
            self.module.add_function("llvm.minnum.f64", fn_type, Some(Linkage::External))
        });
        *self.min.get_or_insert(f)
    }

    pub fn max(&mut self) -> FunctionValue<'ctx> {
        let f = self.max.unwrap_or_else(|| {
            let t_f64 = self.ctx.f64_type();
            let fn_type = t_f64.fn_type(&[t_f64.into(), t_f64.into()], false);
            self.module.add_function("llvm.maxnum.f64", fn_type, Some(Linkage::External))
        });
        *self.max.get_or_insert(f)
    }

    pub fn abs(&mut self) -> FunctionValue<'ctx> {
        let f = self.abs.unwrap_or_else(|| {
            let t_f64 = self.ctx.f64_type();
            let fn_type = t_f64.fn_type(&[t_f64.into()], false);
            self.module.add_function("llvm.fabs.f64", fn_type, Some(Linkage::External))
        });
        *self.abs.get_or_insert(f)
    }

    pub fn log(&mut self) -> FunctionValue<'ctx> {
        let f = self.log.unwrap_or_else(|| {
            let t_f64 = self.ctx.f64_type();
            let fn_type = t_f64.fn_type(&[t_f64.into()], false);
            self.module.add_function("llvm.log.f64", fn_type, Some(Linkage::External))
        });
        *self.log.get_or_insert(f)
    }

    pub fn cos(&mut self) -> FunctionValue<'ctx> {
        let f = self.cos.unwrap_or_else(|| {
            let t_f64 = self.ctx.f64_type();
            let fn_type = t_f64.fn_type(&[t_f64.into()], false);
            self.module.add_function("llvm.cos.f64", fn_type, Some(Linkage::External))
        });
        *self.cos.get_or_insert(f)
    }

    pub fn sin(&mut self) -> FunctionValue<'ctx> {
        let f = self.sin.unwrap_or_else(|| {
            let t_f64 = self.ctx.f64_type();
            let fn_type = t_f64.fn_type(&[t_f64.into()], false);
            self.module.add_function("llvm.sin.f64", fn_type, Some(Linkage::External))
        });
        *self.sin.get_or_insert(f)
    }

    pub fn new(ctx: &'ctx Context, module: Rc<Module<'ctx>>, builder: Rc<Builder<'ctx>>) -> Self {
        Self {
            ctx, module, builder,
            _printf: None, print_float: None, println_float: None, print_str: None, println_str: None,
            sqrt: None, min: None, max: None, abs: None, log: None, cos: None, sin: None,
        }
    }
}
