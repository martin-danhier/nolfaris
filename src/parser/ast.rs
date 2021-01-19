use pest::Span;

use crate::utils::locations::InFileLocation;
use std::fmt::Debug;

// ================
// == MAIN TYPES ==
// ================

/// # Abstract Syntax Tree
///
/// An **Abstract Syntax Tree** (AST) is a tree-like structure representing
/// the contents of a file. It is a essential part of the parsing process.
///
/// When a NoKe file is parsed, the parser will build an AST containing its contents.
///
/// Once the parsing is finished, the AST will then be analysed to detect more errors.
/// This step is called the **semantic analysis**. During this step, the tree will also be
/// annotated to include additionnal informations about its contents, like the variables
/// in scope.
///
/// If the analysis terminates with errors, the compiler stops and return a list of all the
/// encountered errors.
///
/// If the analysis terminates without any errors, then the parsed file is valid and the
/// compilation process can continue. The tree will then be **serialized** into the desired
/// output format.
///
/// This format can be one of the following:
/// - annotated JSON (useful for IDE support)
/// - 3AC (low-level assembly-like structure useful for optimization)
#[derive(Debug)]
pub struct Ast {
    pub file_name: String,
    pub root_node: AstNode<File>,
}

/// # AST Node
///
/// An AST node is a singular node in the abstract syntax tree.
/// In contains data about the element it represents, including its location,
/// what was parsed, or whether it contains any error.
///
/// For more informations about the AST itself, see [Ast].
#[derive(Debug)]
pub struct AstNode<T> {
    /// Location of the node in the file.
    ///
    /// It can used for several purposes:
    /// - error reporting
    /// - autocompletion
    /// - documentation
    /// - debugging
    pub location: InFileLocation,
    /// Contains the eventual error.
    ///
    /// See the documentation of [SyntaxError].
    pub error: SyntaxError,
    /// The parsed data.
    ///
    /// Contains informations about the parsed data represented by the node.
    ///
    /// For example, it can be a variable, an operation, ...
    pub data: T,
}

impl<T> AstNode<T> {
    /// Create a new ``AstNode`` from a ``pest_consume::Node``, when there is no error.
    pub fn from_span(span: &Span, data: T) -> AstNode<T> {
        AstNode {
            data,
            error: SyntaxError::None,
            location: InFileLocation::from_span(&span),
        }
    }

    /// Create a new ``AstNode`` from a ``pest_consume::Node``, when there is an error.
    pub fn from_span_err(span: &Span, data: T, err: SyntaxError) -> AstNode<T> {
        AstNode {
            data,
            error: err,
            location: InFileLocation::from_span(&span),
        }
    }

    /// Creates a new `AstNode`, without error, with the given location.
    pub fn with_location(location: InFileLocation, data: T) -> AstNode<T> {
        AstNode {
            data,
            error: SyntaxError::None,
            location,
        }
    }

    /// Creates a new `AstNode`, without error, with the given location.
    pub fn with_location_err(location: InFileLocation, data: T, err: SyntaxError) -> AstNode<T> {
        AstNode {
            data,
            error: err,
            location,
        }
    }

    /// Adds the given error to the `AstNode`
    pub fn add_error(mut self, error: SyntaxError) -> Self {
        self.error = error;
        self
    }
}

// Define some aliases to make the code more readable

/// Equivalent to `Box<AstNode<T>>`
pub type BoxedNode<T> = Box<AstNode<T>>;

/// Equivalent to `Vec<Box<AstNode<T>>>`
pub type NodeVec<T> = Vec<Box<AstNode<T>>>;

/// Equivalent to ``Vec<AstNode<StructuralElement>>``
pub type File = Vec<AstNode<StructuralElement>>;

// ====================
// == ERROR RECOVERY ==
// ====================

/// # Syntax Error
///
/// The parser is voluntarily too permissive and will parse invalid elements.
/// The reason is that when the parser fails, it returns a single error and doesn't
/// return the succesfully parsed nodes. An overpemissive parser allows us to
/// create error recovery during the semantic analysis. The errors will be parsed and noted
/// in the AST using the [SyntaxError] enum, but for the parser, the input string will be
/// considered as perfectly correct. This allows us to report several syntax errors at once,
/// and even some semantic errors for the successfully-parsed sections.
///
/// The `None` field indicates the absence of any error. It is present to avoid an unnecessary
/// wrap of the enum in an [Option].
#[derive(Debug)]
pub enum SyntaxError {
    None,
    UnexpectedChar(char),
    UnclosedBlock,
    MissingSemicolon,
    ParseIntError(std::num::ParseIntError),
    ParseFloatError(std::num::ParseFloatError),
    ParseCharError {
        c: String,
        message: String,
    },
    /// This error sould not occur.
    /// If it does, then the parser or the grammar has a problem
    /// Example, parsing something else than "true" or "false" as boolean
    ///
    /// The parameter is a string containing a message.
    ParserError(String),
    SeveralErrors(Vec<SyntaxError>),
    EmptyListAccess,
    InvalidOperator(String),
}

// ================
// == PARSE DATA ==
// ================

/// # Expressions
///
/// An **expression** is a node that evaluates to a value. It is one of the most common structure
/// in programming languages.
///
/// **Examples of expressions:**
///
/// - `foo(4)`
/// - `45 + bar`
/// - `counter++`
/// - `"string"`
/// - `[1, 2, 3]`
/// - `list[i]`
#[derive(Debug)]
pub enum Expression {
    Array(NodeVec<Expression>),
    // Since a literal isn't recursive, it doesn't need to be wrapped in a Box
    Literal(AstNode<Literal>),
    PrefixOperation {
        operator: AstNode<PrefixOperator>,
        term: BoxedNode<Expression>,
    },
    PostfixOperation {
        operator: AstNode<PostfixOperator>,
        term: BoxedNode<Expression>,
    },
    BinaryOperation {
        operator: AstNode<BinaryOperator>,
        left_term: BoxedNode<Expression>,
        right_term: BoxedNode<Expression>,
    },
    // When there is an error
    None,
}

impl Expression {
    pub fn bin_operation(
        l: AstNode<Expression>,
        op_span: &Span,
        op: BinaryOperator,
        r: AstNode<Expression>,
    ) -> AstNode<Expression> {
        // Get the location of the operation. It goes from the beginning of the left term to the end of the right term
        let location =
            InFileLocation::Span(l.location.get_start().clone(), r.location.get_end().clone());

        AstNode::with_location(
            location,
            Expression::BinaryOperation {
                left_term: Box::new(l),
                right_term: Box::new(r),
                operator: AstNode::from_span(&op_span, op),
            },
        )
    }
}

/// # Literal
///
/// A **literal** is an expression containing a predefined, hardcoded value.
/// They are the "leaves" of the Abstract Syntax Tree.
///
/// **Examples:**
/// - ``4`` : decimal int literal
/// - ``0xf`` : hexadecimal int literal
/// - ``0b110`` : binary int literal
/// - ``42.5`` : float literal
/// - ``true`` : bool literal
/// - ``'c'`` : char literal
/// - ``"string"`` : string literal
/// - ``foo`` : identifier
///
/// The identifier is a particular case, because it will not be hardcoded in the destination code.
/// However, it is still a primary node that can be used as a leaf.
#[derive(Debug)]
pub enum Literal {
    Integer(i32),
    Float(f32),
    Bool(bool),
    Char(char),
    String(String),
    Identifier(String),
    /// Used when a parsing error occured.
    None,
}

/// # Prefix operator
///
/// A **prefix operator** is a unary operator that is placed to the left of its argument.
///
/// **Examples:**
/// - ``-5``: negation
/// - ``not true``: logical not
/// - ``++a``: increment
/// - ``--a``: decrement
/// - ``typeof foo``: get the type of an expression
#[derive(Debug)]
pub enum PrefixOperator {
    Neg,
    Not,
    Increment,
    Decrement,
    Typeof,
    /// Used when an invalid prefix is found
    None,
}

/// # Postfix operator
///
/// A **postfix operator** is a unary operator that is placed to the right of its argument.
///
/// **Examples:**
/// - ``a++``: increment
/// - ``a--``: decrement
/// - ``foo(bar)``: function call
/// - ``foo[1]``: list access
#[derive(Debug)]
pub enum PostfixOperator {
    Increment,
    Decrement,
    Call {
        type_params: NodeVec<Expression>,
        params: NodeVec<Expression>,
    },
    ListAccess(BoxedNode<Expression>),
}

/// # Binary operator
///
/// A **binary operator** is an operator with two arguments, one on each side of the operator.
///
/// **Examples**:
/// - ``a + b``: addition
/// - ``a - b``: substraction
/// - ``a * b``: product
/// - ``a / b``: division
/// - ``a ** b``: exponent
/// - ``a % b``: Modulo
/// - ``a < b``: Less than
/// - ``a > b``: Greater than
/// - ``a <= b``: Less than or equal to
/// - ``a >= b``: Greater than or equal to
/// - ``a and b``: Logical and
/// - ``a or b``: Logical or
/// - ``a == b``: Equality
/// - ``a != b``: Non-equality
/// - ``a . b``: Navigation
#[derive(Debug, Copy, Clone)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Mod,
    Lst,
    Grt,
    Leq,
    Geq,
    And,
    Or,
    Eq,
    Neq,
    Nav,
}

/// # Statement
///
/// A statement is a element that executes a specific action.
/// Each statement of a block is executed in order from to to bottom.
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
    Empty,
    Return(AstNode<Expression>),
    /// # Block
    ///
    /// A **block** is a set of statement in a specific order,
    /// than can be executed in order from top to bottom.
    ///
    /// A block is also **scoped** which mean that a local variable declared inside the block
    /// will not be accessible from outside of the block.
    Block(NodeVec<Statement>),
    /// See [LeftHandSide].
    LeftHandSide(AstNode<LeftHandSide>),
    /// # Affectation
    ///
    /// An **affectation** is a statement that affects a value to a variable.
    ///
    /// The operator can define a secondary action. See [AffectationOperator].
    ///
    /// ```noke
    /// // Affectation
    /// let a = 0;
    /// // Addition affectation
    /// a += 5;
    /// // This is equivalent to:
    /// a = a + 5;
    /// ```
    Affectation {
        left_hand_side: AstNode<LeftHandSide>,
        operator: AstNode<AffectationOperator>,
        right_hand_side: AstNode<Expression>,
    },
    /// # Branch
    ///
    /// A **branch** is a statement that can test a condition and execute a specific
    /// statement depending on the result.
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
    Branch {
        condition: AstNode<Expression>,
        if_statement: BoxedNode<Statement>,
        else_statement: Option<BoxedNode<Statement>>,
    },
    /// # For loop
    ///
    /// A **for loop** is a statement that can iterate over a block of code for a specific
    /// amount of iterations. The counter variable is initialised with the init statement,
    /// it is then incremented between each step using the step statement. The loop iterates
    /// while the until condition is true.
    ///
    /// ## Example
    ///
    /// ```noke
    /// for (let i = 0; i < 10; i++) {
    ///     log("i = " + i);
    /// }
    /// ```
    ForLoop {
        init: BoxedNode<Statement>,
        until: AstNode<Expression>,
        step: BoxedNode<Statement>,
        body: BoxedNode<Statement>,
    },
    /// # ForEach loop
    ///
    /// A **foreach loop** is a statement that can iterate over a block of code
    /// for each value in an iterable.
    ///
    /// ## Example
    ///
    /// ```noke
    /// for (let elem in [0, 4, 7]) {
    ///     log("elem = " + elem);
    /// }
    /// ```
    ForEachLoop {
        init: AstNode<LeftHandSide>,
        iterable: AstNode<Expression>,
        body: BoxedNode<Statement>,
    },
    /// # While loop
    ///
    /// A while loop is a statement that can iterate over a block of code while a
    /// condition is true.
    ///
    /// ## Example
    ///
    /// ```noke
    /// let i = 0;
    /// while (i < 10) {
    ///     i += 1;
    /// }
    /// ```
    WhileLoop {
        condition: AstNode<Expression>,
        body: BoxedNode<Statement>,
    },
}

/// # Left hand side
///
/// A **left hand side** (LHS) is a particular kind of statement, most commonly found in assignments.
/// Assignements are divided by the assignement operator (for example an `=`). The part to the left
/// is then called the "left hand side" (LHS), while the part to the right is called the "right hand side" (RHS).
///
/// However, left hand sides can be used outside of assignements. For example, a statement that only consists of
/// a declaration or an expression evaluation is only a left hand side.
///
/// ## Examples
///
/// ```noke
/// foo(); // "foo()" is a LHS
/// let a = 4; // The "let a" is a LHS
/// let a: int; // "let a: int" is a LHS
/// ```
#[derive(Debug)]
pub enum LeftHandSide {
    Expression(AstNode<Expression>),
    Declaration {
        name: AstNode<Literal>,
        var_type: Option<AstNode<Expression>>,
        mutable: bool,
    },
}
/// # Affectation Operator
///
/// An **affectation operator** is a operator used in an affectation.
/// It typically *affects* the value of the expression on the right (called *right-hand-side*)
/// to the variable/struct member/... on the left (called *left-hand-side*), hence its name.
///
/// Some operators can apply an additional operations on the sides before affecting the value.
/// Typically, it is used to shorten the common affectations.
///
/// **Examples**:
/// - ``foo = 4``: store 4 in the variable `foo`
/// - ``foo += 4``: equivalent to ``foo = foo + 4``
/// - ``foo -= 4``: equivalent to ``foo = foo - 4``
/// - ``foo *= 4``: equivalent to ``foo = foo * 4``
/// - ``foo /= 4``: equivalent to ``foo = foo / 4``
/// - ``foo %= 4``: equivalent to ``foo = foo % 4``
#[derive(Debug, Copy, Clone)]
pub enum AffectationOperator {
    Affect, // =
    AffSum, // +=
    AffSub, // -=
    AffMul, // *=
    AffDiv, // /=
    AffMod, // %=
    /// Used when an invalid prefix is found
    None,
}

/// # Structural Element
///
/// A **structural element** is a structure placed at the root of a file (or any module).
/// They are mainly used to structure the code into different parts, hence their name.
///
/// Structural element include statements, because they can be placed at the root of the file
/// as well.
///
/// **Examples:**
/// ```noke
/// fun foo() {}
///
/// enum Mode {
///    On,
///    Off
/// }
/// ```
#[derive(Debug)]
pub enum StructuralElement {
    // When there is an error
    None,
    Statement(AstNode<Statement>),
    /// # Module
    ///
    /// A module is a structural element that contains other structural elements.
    ///
    /// For example, a module can contain functions or other modules.
    ///
    /// A file is an implicit module.
    Module {
        name: AstNode<Literal>,
        body: NodeVec<StructuralElement>,
    },
    /// # Function
    ///
    /// A function is a reusable block of code that can be
    /// called with certain parameters.
    ///
    /// A function also has
    /// a return type that specifies the type of the returned value.
    Function {
        name: AstNode<Literal>,
        params: Vec<AstNode<Statement>>,
        body: AstNode<Statement>,
        return_type: Option<AstNode<Expression>>,
        type_params: NodeVec<Expression>,
        public: bool,
    },
    Import(AstNode<Expression>),
    /// # Struct
    ///
    /// A struct is a bunch of variables (called fields) grouped together
    /// in a single one. Each variable can be accessed via its name using the nav operator.
    ///
    /// A struct can have type parameters and a visibility.
    Struct {
        name: AstNode<Literal>,
        fields: Vec<AstNode<Statement>>,
        type_params: NodeVec<Expression>,
        public: bool,
    },
    Enum {
        name: AstNode<Literal>,
        variants: Vec<AstNode<Literal>>,
        public: bool,
    }
}
