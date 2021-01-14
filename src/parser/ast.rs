// =========================
// == STRUCTURAL ELEMENTS ==
// =========================

/// # Structural Element
/// Structural elements are the things that are placed at a root of a module, but not inside functions.
/// They are used to structure the code in different parts.
#[derive(Debug)]
pub enum StructuralElement {
    Module(Module),
    Function(Function),
    Import(Box<Expr>),
    Struct(Struct),
    Enum(Enumeration),
    Statement(Box<Statement>),
    Error,
}

/// # Struct
///
/// A struct is a bunch of variables (called fields) grouped together
/// in a single one. Each variable can be accessed via its name using the nav operator.
///
/// A struct can have type parameters and a visibility.
#[derive(Debug)]
pub struct Struct {
    pub name: Box<Expr>,
    pub fields: Vec<Box<Statement>>,
    pub type_params: Vec<Box<Expr>>,
    pub public: bool,
}

/// # Enumeration
///
/// An enum is a set of named symbolic values, called variants.
///
/// An enum is private by default, but can be defined as public using the "pub" keyword.
#[derive(Debug)]
pub struct Enumeration {
    pub name: Box<Expr>,
    pub variants: Vec<Box<Expr>>,
    pub public: bool,
}

/// # Function
///
/// A function is a reusable block of code that can be
/// called with certain parameters.
///
/// A function also has
/// a return type that specifies the type of the returned value.
#[derive(Debug)]
pub struct Function {
    pub name: Box<Expr>,
    pub params: Vec<Box<Statement>>,
    pub body: Box<Statement>,
    pub return_type: Option<Box<Expr>>,
    pub type_params: Vec<Box<Expr>>,
    pub public: bool,
}

/// # Module
///
/// A module is a structural element that contains other structural elements.
///
/// For example, a module can contain functions or other modules.
///
/// A file is an implicit module.
#[derive(Debug)]
pub struct Module {
    pub name: Box<Expr>,
    pub body: Vec<Box<StructuralElement>>
}

// =================
// == EXPRESSIONS ==
// =================

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
    Call(FunctionCall), // (a)
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

/// # Function call
///
/// A function call is an operation that consist on calling a function to execute the contained code
/// using the given parameters. The parameters are values given between the parentheses, but there can be
/// type parameters given between the angle brackets.
///
/// ## Example
///
/// ```noke
/// display("hello");
/// foo<Bar>(42);
/// ```
#[derive(Debug)]
pub struct FunctionCall {
    pub type_params: Vec<Box<Expr>>,
    pub params: Vec<Box<Expr>>,
}

// ================
// == STATEMENTS ==
// ================

/// # Statement
///
/// A statement is a element that executes a specific action.
/// Each statement of a block is executed in from to to bottom.
///
/// ## Example
///
/// ```noke
/// let a = 3 + 2;
/// log("hello world");
/// if (true) {
///     return false;
/// }
/// ```
#[derive(Debug)]
pub enum Statement {
    LeftHandSide(Box<LeftHandSide>),
    Affectation(Affectation),
    Return(Box<Expr>),
    Block(Vec<Box<Statement>>),
    Branch(Branch),
    /// init, until, step, body
    For(ForLoop),
    ForEach(ForEachLoop),
    While(WhileLoop),
    // Avoid errors when empty
    Empty,
    Error,
}

/// # Affectation Opcode
///
/// Usable operator in an affectation.
///
/// See [Affectation] for more details.
#[derive(Debug, Copy, Clone)]
pub enum AffectationOpcode {
    Affect, // =
    AffSum, // +=
    AffSub, // -=
    AffMul, // *=
    AffDiv, // /=
    AffMod, // %=
}

/// # Affectation
///
/// An affectation is a statement that affects a value to a variable.
///
/// The operator defines a secondary action.
///
/// ```noke
/// // Affectation
/// let a = 0;
/// // Addition affectation
/// a += 5;
/// // This is equivalent to:
/// a = a + 5;
/// ```
#[derive(Debug)]
pub struct Affectation {
    pub left_hand_side: Box<LeftHandSide>,
    pub op: AffectationOpcode,
    pub right_hand_side: Box<Expr>,
}

/// # Branch
///
/// A branch is a statement that can test a condition and execute a specific
/// block of code depending on the result.
///
/// ## Example
///
/// ```noke
/// if (a == 2) {
///     a = 8;
///     log("first condition");
/// }
/// else log("nope");
/// ```
#[derive(Debug)]
pub struct Branch {
    pub condition: Box<Expr>,
    pub if_statement: Box<Statement>,
    pub else_statement: Option<Box<Statement>>,
}

/// # For loop
///
/// A for loop is a statement that can iterate over a block of code for a specific
/// amount of iterations. The counter variable is initialised with the init statement,
/// it is then incremented between each step using the step statement. The loop interates
/// while the until condition is true.
///
/// ## Example
///
/// ```noke
/// for (let i = 0; i < 10; i++) {
///     log("i = " + i);
/// }
/// ```
#[derive(Debug)]
pub struct ForLoop {
    pub init: Box<Statement>,
    pub until: Box<Expr>,
    pub step: Box<Statement>,
    pub body: Box<Statement>,
}

/// # ForEach loop
///
/// A foreach loop is a statement that can iterate over a block of code for each value in an iterable.
///
/// ## Example
///
/// ```noke
/// for (let elem in [0, 4, 7]) {
///     log("elem = " + elem);
/// }
/// ```
#[derive(Debug)]
pub struct ForEachLoop {
    pub init: Box<LeftHandSide>,
    pub iterable: Box<Expr>,
    pub body: Box<Statement>,
}

/// # While loop
///
/// A while loop is a statement that can iterate over a block of code while a condition is true.
///
/// ## Example
///
/// ```noke
/// let i = 0;
/// while (i < 10) {
///     i += 1;
/// }
/// ```
#[derive(Debug)]
pub struct WhileLoop {
    pub condition: Box<Expr>,
    pub body: Box<Statement>,
}

/// Statement
#[derive(Debug)]
pub enum LeftHandSide {
    /// "var a : type". The type is optional. The declaration can be mutable or not
    Declaration(Box<Expr>, Option<Box<Expr>>, bool),
    Expression(Box<Expr>),
}