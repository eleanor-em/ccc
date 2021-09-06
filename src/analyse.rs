use std::fmt;

use inkwell::values::{IntValue, PointerValue};

use crate::Span;

pub trait Complex<T> {
    fn re(&self) -> T;
    fn im(&self) -> T;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpanLength {
    Size(usize),
    ToEnd,
    None,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Location {
    pub line: usize,
    pub col: usize,
    pub len: SpanLength,
}

impl Location {
    // default Eq/Ord impl's would do weird stuff, so write our own solution
    pub fn span_to(self, rhs: Self) -> Self {
        match self.line.cmp(&rhs.line) {
            std::cmp::Ordering::Less    => {
                Self {
                    line: self.line,
                    col: self.col,
                    len: SpanLength::ToEnd,
                }
            },
            std::cmp::Ordering::Greater => {
                Self {
                    line: rhs.line,
                    col: rhs.col,
                    len: SpanLength::ToEnd,
                }
            }
            std::cmp::Ordering::Equal => {
                let min = if self.col <= rhs.col { self } else { rhs };
                let max = if self.col == min.col { rhs } else { self };
                Self {
                    line: self.line,
                    col: min.col,
                    len: SpanLength::Size(max.col - min.col),
                }
            },
        }
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.len {
            SpanLength::Size(len) => write!(f, "line {}, column {}-{}", self.line, self.col, self.col + len),
            _ => write!(f, "line {}, column {}", self.line, self.col),
        }
    }
}

impl From<&Span<'_>> for Location {
    fn from(span: &Span) -> Self {
        Self {
            line: span.location_line() as usize,
            col: span.get_column(),
            len: SpanLength::None,
        }
    }
}

#[derive(Debug)]
pub struct Located<T>(T, Location);

impl<T> Located<T> {
    pub fn new(val: T, at: Location) -> Self {
        Self(val, at)
    }

    pub fn borrow_val(&self) -> &T { &self.0 }
    pub fn val(self) -> T { self.0 }
    pub fn unwrap(self) -> (T, Location) { (self.0, self.1) }
    
    pub fn pos(&self) -> Location { self.1 }
}

impl<T: Clone> Clone for Located<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1)
    }
}

impl<V, T: Complex<V>> Complex<V> for Located<Typed<T>> {
    fn re(&self) -> V {
        self.borrow_val().val().re()
    }

    fn im(&self) -> V {
        self.borrow_val().val().im()
    }
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

impl<'a> Complex<IntValue<'a>> for ComplexValue<'a> {
    fn re(&self) -> IntValue<'a> {
        self.re
    }

    fn im(&self) -> IntValue<'a> {
        self.im
    }
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

impl<'a> Complex<PointerValue<'a>> for ComplexPointer<'a> {
    fn re(&self) -> PointerValue<'a> {
        self.re
    }

    fn im(&self) -> PointerValue<'a> {
        self.im
    }
}
