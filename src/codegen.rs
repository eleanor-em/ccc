use std::{collections::HashMap, path::Path};

use inkwell::{OptimizationLevel, builder::Builder, context::Context, execution_engine::JitFunction, module::{Linkage, Module}, values::FunctionValue};

use crate::error::CompileError;

struct SymbolTable<'ctx> {
    func_map: HashMap<String, FunctionValue<'ctx>>,
}

impl<'ctx> SymbolTable<'ctx> {
    fn new() -> Self {
        Self { func_map: HashMap::new(), }
    }
    
    fn func(&self, name: &str) -> Option<&FunctionValue<'ctx>> {
        self.func_map.get(name)
    }

    fn has_func(&self, name: &str) -> bool {
        self.func_map.contains_key(name)
    }
}

pub struct Compiler<'ctx> {
    ctx: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    iprint_fn: FunctionValue<'ctx>,
    sym: SymbolTable<'ctx>
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(ctx: &'ctx Context) -> Self {
        let module = ctx.create_module("primary");
        let builder = ctx.create_builder();

        // Create general print function
        
        // First link to printf
        let i32_type = ctx.i32_type();
        let i8p_type = ctx.i8_type().ptr_type(inkwell::AddressSpace::Generic);
        let printf_type = i32_type.fn_type(&[i8p_type.into()], true);
        let printf = module.add_function("printf", printf_type, Some(Linkage::External));

        // Now create the function
        let void_type = ctx.void_type();
        let i64_type = ctx.i64_type();
        let fn_type = void_type.fn_type(&[i64_type.into(), i64_type.into()], false);
        let iprint_fn = module.add_function(".iprint", fn_type, None);
        let block = ctx.append_basic_block(iprint_fn, "entry");
        builder.position_at_end(block);

        // The function just calls printf with the right arguments
        let printf_str = builder.build_global_string_ptr("%lli + %llii", ".format");
        let re = iprint_fn.get_nth_param(0).unwrap().into_int_value();
        let im = iprint_fn.get_nth_param(1).unwrap().into_int_value();
        builder.build_call(printf, &[printf_str.as_pointer_value().into(), re.into(), im.into()], "call");
        builder.build_return(None);

        Self {
            ctx, module, builder, iprint_fn,
            sym: SymbolTable::new(),
        }
    }

    fn add_test(&mut self) {
        let void_type = self.ctx.void_type();
        let fn_type = void_type.fn_type(&[], false);
        let func = self.module.add_function("main", fn_type, None);
        let block = self.ctx.append_basic_block(func, "entry");

        let i64_type = self.ctx.i64_type();
        self.builder.position_at_end(block);
        self.builder.build_call(self.iprint_fn, 
            &[i64_type.const_int(1, true).into(), i64_type.const_int(2, true).into()], 
            "call");
        self.builder.build_return(None);

        self.sym.func_map.insert("main".to_string(), func);
    }

    /// Prints the compiled LLVM IR to a file.
    fn print_to_file<P: AsRef<Path>>(&self, dest: P) -> Result<(), CompileError> {
        self.module.verify().unwrap();
        self.module.print_to_file(dest)?;
        Ok(())
    }

    /// Executes the compiled code. Fails if there is no `main` function defined.
    fn exec(&self) -> Result<(), CompileError> {
        if self.sym.has_func("main") {
            let exec_engine = self.module.create_jit_execution_engine(OptimizationLevel::Aggressive)?;
            let exec: JitFunction<unsafe extern "C" fn()> = unsafe { exec_engine.get_function("main")? };
            unsafe { exec.call() };
            Ok(())
        } else {
            Err(CompileError::NoMain)
        }
    }
}

pub fn run(dest: &str) -> Result<(), CompileError> {
    let ctx = Context::create();
    let mut gen = Compiler::new(&ctx);
    gen.add_test();
    gen.print_to_file(dest)?;
    gen.exec()
}
