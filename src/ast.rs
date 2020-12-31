use std::fmt::Debug;

#[derive(Debug)]
pub struct CompilerError {
    message: String
}

#[derive(Debug)]
pub enum Expression {
    /// Integer literal
    Integer(i32),
    /// Float literal
    Float(f32),
    /// Boolean literal
    Boolean(bool),
    /// Char literal
    Char(char),
    /// String literal
    String(String),
    /// Binary Operation
    BinOperation(Box<Expression>, BinOpcode, Box<Expression>),
    /// Unary Operation
    UnOperation(UnOpcode, Box<Expression>),
    /// An error occured
    Error,
}

/// Binary Operator
#[derive(Debug, Copy, Clone)]
pub enum BinOpcode {
    /// Multiplication
    Mul,
    /// Division
    Div,
    /// Addition
    Add,
    /// Substraction
    Sub,
    /// Equals
    Equ,
    /// Does not equal
    Neq,
    /// Less than
    Lst,
    /// Greater than
    Grt,
    /// Less or equal to
    Leq,
    /// Greater or equal to
    Geq,
}

/// Unary Operator
///
/// ### Structure
///
/// ``OP expression``
///
/// Example: `-5`
#[derive(Debug, Copy, Clone)]
pub enum UnOpcode {
    /// Not
    Not,
    /// Negation
    Neg,
}
