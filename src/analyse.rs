use inkwell::values::{IntValue, PointerValue};

use crate::Span;

#[derive(Debug, Clone, Copy)]
pub struct Location {
    pub line: usize,
    pub col: usize,
}

impl From<&Span<'_>> for Location {
    fn from(span: &Span) -> Self {
        Self {
            line: span.location_line() as usize,
            col: span.get_column(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Located<T: Clone>(T, Location);

impl<T: Clone> Located<T> {
    pub fn new(val: T, at: Location) -> Self {
        Self(val, at)
    }

    pub fn val_ref(&self) -> &T { &self.0 }
    pub fn val(self) -> T { self.0 }
    pub fn at(&self) -> Location { self.1 }

    pub fn unwrap(self) -> (T, Location) { (self.0, self.1) }
}

// Value types
#[derive(Debug, Clone, Copy)]
pub enum Type {
    MutIntScalar,
    IntScalar,
    MutScalar,
    Scalar,
}

// A value with its type
pub struct Typed<T>(T, Type);

impl<T> Typed<T> {
    pub fn new(val: T, ty: Type) -> Self { Self(val, ty) }
    pub fn val(&self) -> &T { &self.0 }
    pub fn ty(&self) -> Type { self.1 }

    pub fn mutable(&self) -> bool {
        matches!(self.1, Type::MutIntScalar | Type::MutScalar)
    }
}

// Collects two LLVM IntValues into one object
#[derive(Debug, Clone, Copy)]
pub struct ComplexValue<'ctx> {
    pub re: IntValue<'ctx>,
    pub im: IntValue<'ctx>,
}

pub struct ComplexPointer<'ctx> {
    pub re: PointerValue<'ctx>,
    pub im: PointerValue<'ctx>,
}

impl<'ctx> From<(IntValue<'ctx>, IntValue<'ctx>)> for ComplexValue<'ctx> {
    fn from((re, im): (IntValue<'ctx>, IntValue<'ctx>)) -> Self {
        Self { re, im }
    }
}
