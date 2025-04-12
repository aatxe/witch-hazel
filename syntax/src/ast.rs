use bumpalo::boxed::Box;
use bumpalo::collections::{String, Vec};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IndexOperator {
    /// `.`
    Dot,

    /// `:`
    Colon,
}

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
    pub name: String<'alloc>,
    pub ty: Option<Type<'alloc>>,
}

pub type Bindings<'alloc> = Vec<'alloc, Binding<'alloc>>;
pub type Expressions<'alloc> = Vec<'alloc, Expression<'alloc>>;
pub type BoxedExpression<'alloc> = Box<'alloc, Expression<'alloc>>;

#[derive(Debug, PartialEq)]
pub struct Block<'alloc> {
    pub expressions: Expressions<'alloc>,
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

    /// A function call expression.
    Call {
        function: Box<'alloc, Expression<'alloc>>,
        arguments: Expressions<'alloc>,
    },

    /// An expression indexed by a name.
    IndexName {
        operator: IndexOperator,
        expression: BoxedExpression<'alloc>,
        name: String<'alloc>,
    },

    /// An expression indexed by an expression.
    IndexExpression {
        expression: BoxedExpression<'alloc>,
        index: BoxedExpression<'alloc>,
    },

    /// A block expression.
    DoBlock { body: Block<'alloc> },

    /// A function expression.
    Function {
        name: Option<String<'alloc>>,
        parameters: Bindings<'alloc>,
        body: Block<'alloc>,
    },

    /// A binary operation.
    BinaryOperation {
        operator: BinaryOperator,
        left: BoxedExpression<'alloc>,
        right: BoxedExpression<'alloc>,
    },

    /// An erroneous expression.
    Error,
}
