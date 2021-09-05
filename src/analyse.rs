use inkwell::values::IntValue;

// Value types
#[derive(Debug, Clone, Copy)]
pub enum Type {
    MutScalar,
    Scalar,
}

// A value with its type
pub struct Typed<T>(T, Type);

impl<T> Typed<T> {
    pub fn new(val: T, ty: Type) -> Self { Self(val, ty) }
    pub fn val(&self) -> &T { &self.0 }
    pub fn ty(&self) -> Type { self.1 }
}

// Collects two LLVM IntValues into one object
pub struct ComplexValue<'ctx> {
    pub re: IntValue<'ctx>,
    pub im: IntValue<'ctx>,
}

impl<'ctx> From<(IntValue<'ctx>, IntValue<'ctx>)> for ComplexValue<'ctx> {
    fn from((re, im): (IntValue<'ctx>, IntValue<'ctx>)) -> Self {
        Self { re, im }
    }
}
