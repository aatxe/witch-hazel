use bumpalo::boxed::Box;
use bumpalo::collections::{String, Vec};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOperator {
    /// `+`
    Add,
    /// `-`
    Subtract,
    /// `*`
    Multiply,
    /// `/`
    Divide,
    /// `//`
    FloorDivide,
    /// `%`,
    Modulo,
    /// `^`
    Exponentiation,
    /// `..`
    Concat,

    /// `and`
    And,
    /// `or`
    Or,

    /// `==`
    Equals,
    /// `~=`
    NotEquals,
    /// `<`
    LessThan,
    /// `>`
    GreaterThan,
    /// `<=`
    LessThanOrEqual,
    /// `>=`
    GreaterThanOrEqual,
}

#[derive(Debug, PartialEq)]
pub enum Type<'alloc> {
    Reference { name: String<'alloc> },
}

#[derive(Debug, PartialEq)]
pub struct Binding<'alloc> {
    name: String<'alloc>,
    ty: Type<'alloc>,
}

pub type Bindings<'alloc> = Vec<'alloc, Binding<'alloc>>;

#[derive(Debug, PartialEq)]
pub struct Block<'alloc> {
    expressions: Vec<'alloc, Expression<'alloc>>,
}

#[derive(Debug, PartialEq)]
pub enum Expression<'alloc> {
    /// A variable reference.
    Identifier { value: String<'alloc> },

    /// A `nil` literal value.
    NilLiteral,

    /// A boolean literal value.
    BooleanLiteral { value: bool },

    /// A number literal value.
    NumberLiteral { value: f64 },

    /// A block expression
    DoBlock { body: Block<'alloc> },

    /// A function expression.
    Function {
        parameters: Bindings<'alloc>,
        body: Block<'alloc>,
    },

    /// A binary operation.
    BinaryOperation {
        operator: BinaryOperator,
        left: Box<'alloc, Expression<'alloc>>,
        right: Box<'alloc, Expression<'alloc>>,
    },

    /// An erroneous expression.
    Error,
}
