/// Expression
#[derive(Debug)]
pub enum Expr {
    // Literals
    Integer(i32),
    Float(f32),
    Bool(bool),
    Char(char),
    String(String),
    Identifier(String),
    Array(Vec<Box<Expr>>),
    // Binary operation
    BinOp(Box<Expr>, BinOpcode, Box<Expr>),
    // Unary operation
    PrefixOp(PrefixOpcode, Box<Expr>),
    PostfixOp(Box<Expr>, PostfixOpcode),
}

#[derive(Debug, Copy, Clone)]
pub enum PrefixOpcode {
    Neg, // -
    Not, // "not"
    Increment, // ++
    Decrement, // --
    Typeof, // "typeof"
}

#[derive(Debug)]
pub enum PostfixOpcode {
    Increment, // ++
    Decrement, // --
    Call(Vec<Box<Expr>>), // (a)
    Indexing(Vec<Box<Expr>>) // [a]
}

/// Operator
#[derive(Debug, Copy, Clone)]
pub enum BinOpcode {
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Pow, // **
    Mod, // %
    Lst, // <
    Grt, // >
    Leq, // <=
    Geq, // >=
    And, // "and"
    Or,  // "or"
    Eq,  // ==
    Neq, // !=
    Nav, // .
}

/// Statement
#[derive(Debug)]
pub enum Statement {
    LeftHandSide(Box<LeftHandSide>),
    Assignment(Box<LeftHandSide>, Box<Expr>),
    Block(Vec<Box<Statement>>),
    /// condition, if statement, else statement
    Branch(Box<Expr>, Box<Statement>, Option<Box<Statement>>),
    /// init, until, step, body
    For(Box<Statement>, Box<Expr>, Box<Statement>, Box<Statement>),
    ForEach(Box<LeftHandSide>, Box<Expr>, Box<Statement>),
}
/// Statement
#[derive(Debug)]
pub enum LeftHandSide {
    /// "var a : type". The type is optional. The declaration can be mutable or not
    Declaration(Box<Expr>, Option<Box<Expr>>, bool),
    Expression(Box<Expr>),
}