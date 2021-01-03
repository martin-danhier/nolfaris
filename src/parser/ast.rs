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
    FunctionCall(Box<Expr>, Vec<Box<Expr>>),
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

#[derive(Debug, Copy, Clone)]
pub enum PostfixOpcode {
    Increment, // ++
    Decrement, // --
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