use super::ast::*;
use crate::error::{Error, ErrorVariant, Severity};
use crate::utils::locations::{InFileLocation, InFilePosition, NodeLocation};
use pest::prec_climber as pcl;
use pest_consume::{match_nodes, Parser};
use std::{str::FromStr, vec::IntoIter};

// Declare type aliases to make the code more readable

#[allow(dead_code)] // Rust incorrectly thinks that the typedef is not used
type Result<T> = std::result::Result<T, pest_consume::Error<Rule>>;
pub type Node<'i> = pest_consume::Node<'i, Rule, ()>;

// Derive main parser struct

/// # NokeParser
///
/// The parser is the core struct of the parser module.
/// Its role is to parse a given string into an **Abstract Syntax Tree** (AST).
/// To learn more about ASTs, see [Ast].
///
/// Using the `pest` parser generator, the contents of the struct are generated
/// automatically based on the grammar file.
///
/// However, since ``pest`` uses proc macros, it has really bad IDE support
/// (the output types are `unknown`, which disable every editor linting and autocompletion).
/// To prevent that, we use the `pest_consume` crate that converts every rule into
/// a custom AST. The impl block of this struct contains one function per grammar rule and returns
/// the AST built from its children.
#[derive(Parser)]
#[grammar = "./parser/grammar.pest"]
struct NokeParser;

// Create prec climber lazily
// Prec climber is used to define operator precedence
lazy_static::lazy_static! {
    static ref PRECCLIMBER: pcl::PrecClimber<Rule> = pcl::PrecClimber::new (
        // Define operator precedence
        // The furthest in the list, the highest the precedence
        // A high precedence means that the operator will be matched first
        // A | operator is used to define operators on the same precedence level
        vec![
            // Or
            pcl::Operator::new(Rule::or, pcl::Assoc::Left),
            // And
            pcl::Operator::new(Rule::and, pcl::Assoc::Left),
            // == and !=
            pcl::Operator::new(Rule::equals, pcl::Assoc::Left)
                | pcl::Operator::new(Rule::different, pcl::Assoc::Left),
            // comparisons
            pcl::Operator::new(Rule::less_or_equal_to, pcl::Assoc::Left)
                | pcl::Operator::new(Rule::less_than, pcl::Assoc::Left)
                | pcl::Operator::new(Rule::greater_or_equal_to, pcl::Assoc::Left)
                | pcl::Operator::new(Rule::greater_than, pcl::Assoc::Left),
            // + and -
            pcl::Operator::new(Rule::minus, pcl::Assoc::Left)
                | pcl::Operator::new(Rule::plus, pcl::Assoc::Left),
            // *, / and %
            pcl::Operator::new(Rule::modulo, pcl::Assoc::Left)
                | pcl::Operator::new(Rule::div, pcl::Assoc::Left)
                | pcl::Operator::new(Rule::times, pcl::Assoc::Left),
            // exponent
            pcl::Operator::new(Rule::exponent, pcl::Assoc::Left),
            // misc
            pcl::Operator::new(Rule::nav, pcl::Assoc::Left),
        ]
    );
}

trait VecUtils<T> {
    /// Wraps each element of a Vec into a Box.
    ///
    /// Example:
    ///
    /// ```rust
    /// box_vec_elems(vec![1, 2]) == vec![Box::new(1), Box::new(2)]
    /// ```
    fn collect_and_box_elems(self) -> Vec<Box<T>>;
}

impl<T> VecUtils<T> for IntoIter<T> {
    fn collect_and_box_elems(self) -> Vec<Box<T>> {
        self.map(|e| Box::new(e)).collect()
    }
}

// Define the converting functions for each rule.
// Each function represents a grammar rule
// It returns the corresponding AST, built from its children
// For more details, see the documentation of `pest_consume`.
#[pest_consume::parser]
impl NokeParser {
    // ===================
    // == Keyword rules ==
    // ===================

    // These rule don't return anything, but we check their presence

    /// Checks if a semicolon is present
    pub fn semicolon(input: Node) -> Result<()> {
        Ok(())
    }

    /// Checks if the `let` keyword is present
    pub fn mutable(input: Node) -> Result<()> {
        Ok(())
    }

    /// Checks if the `const` keyword is present
    pub fn immutable(input: Node) -> Result<()> {
        Ok(())
    }

    /// Checks if the end of input is reached
    pub fn EOI(_input: Node) -> Result<()> {
        Ok(())
    }

    /// Checks if an opening curly bracket is present
    pub fn op_curly_bracket(input: Node) -> Result<()> {
        Ok(())
    }

    /// Checks if a closing curly bracket is present
    pub fn cl_curly_bracket(input: Node) -> Result<()> {
        Ok(())
    }

    /// Checks if the `pub` keyword is present
    pub fn public(input: Node) -> Result<()> {
        Ok(())
    }

    // ====================
    // == Error handling ==
    // ====================

    pub fn err_unsupported_char(input: Node) -> Result<SyntaxError> {
        Ok(SyntaxError::UnexpectedChar(
            input.as_str().chars().next().unwrap(),
        ))
    }

    // ==============
    // == Literals ==
    // ==============

    // === Integers ===

    /// Parses a decimal integer.
    ///
    /// Example: `10`
    pub fn dec_integer(input: Node) -> Result<std::result::Result<i32, std::num::ParseIntError>> {
        Ok(i32::from_str(input.as_str()))
    }

    /// Parses a hexadecimal integer.
    ///
    /// Example: `0x4f`
    pub fn hex_integer(input: Node) -> Result<std::result::Result<i32, std::num::ParseIntError>> {
        Ok(i32::from_str_radix(input.as_str(), 16))
    }

    /// Parses a binary integer
    ///
    /// Example: `0b1001`
    pub fn bin_integer(input: Node) -> Result<std::result::Result<i32, std::num::ParseIntError>> {
        Ok(i32::from_str_radix(input.as_str(), 2))
    }

    /// Parses an integer
    pub fn integer(input: Node) -> Result<AstNode<Literal>> {
        let sp = input.as_span();

        // Get the parsed int
        let parse_result = match_nodes![
            input.into_children();
            [dec_integer(nb)] => nb,
            [hex_integer(nb)] => nb,
            [bin_integer(nb)] => nb
        ];

        // Return an AstNode with it
        match parse_result {
            // Ok => Literal with the value
            Ok(n) => Ok(AstNode::from_span(&sp, Literal::Integer(n))),
            // Error => None literal with the error
            Err(e) => Ok(AstNode::from_span_err(
                &sp,
                Literal::None,
                SyntaxError::ParseIntError(e),
            )),
        }
    }

    // === Floats ===

    /// Parses a float.
    ///
    /// Example: `4.15`
    pub fn float(input: Node) -> Result<AstNode<Literal>> {
        let sp = input.as_span();

        match f32::from_str(input.as_str()) {
            // Ok => Literal with the value
            Ok(n) => Ok(AstNode::from_span(&sp, Literal::Float(n))),
            // Error => None literal with the error
            Err(e) => Ok(AstNode::from_span_err(
                &sp,
                Literal::None,
                SyntaxError::ParseFloatError(e),
            )),
        }
    }

    // === Booleans ===

    /// Parses a boolean.
    ///
    /// Examples: `true`, `false`.
    pub fn bool(input: Node) -> Result<AstNode<Literal>> {
        let sp = input.as_span();

        match input.as_str() {
            "true" => Ok(AstNode::from_span(&sp, Literal::Bool(true))),
            "false" => Ok(AstNode::from_span(&sp, Literal::Bool(false))),
            // If it is not one of the above possibilities, then the parser or the grammar has a problem
            // Note it as a parser error
            // As always, still tell pest_consume that there are no error, so the parsing continues and can potentially spot other errors.
            l => Ok(AstNode::from_span_err(
                &sp,
                Literal::None,
                SyntaxError::ParserError(format!("Invalid bool literal: \"{:?}\".", l)),
            )),
        }
    }

    // === Char ===

    /// Parses a character. Used in char and string literals. Handles escape sequences.
    ///
    /// Example: `a`, `\n`
    pub fn character(input: Node) -> Result<std::result::Result<char, SyntaxError>> {
        // Get the char from the input
        let mut chars = input.as_str().chars();

        Ok(match chars.next() {
            Some('\\') => {
                // Handle escape sequences
                match chars.next() {
                    Some('u') => {
                        // Get the digits
                        // They will always be 4 hexadecimal numbers because the grammar requires so
                        let value = chars
                            .by_ref()
                            .take(4)
                            .fold(0, |acc, c| acc * 16 + c.to_digit(16).unwrap());
                        // Try to convert them
                        match std::char::from_u32(value) {
                            Some(c) => Ok(c),
                            None => {
                                // Ok(Err(...)) = the parsing worked, but there was an error with the parsed elements
                                Err(SyntaxError::ParseCharError {
                                    message: format!("Invalid character: \"{}\".", input.as_str()),
                                    c: String::from(input.as_str()),
                                })
                            }
                        }
                    }
                    Some('b') => Ok('\x08'),
                    Some('f') => Ok('\x0c'),
                    Some('n') => Ok('\n'),
                    Some('r') => Ok('\r'),
                    Some('t') => Ok('\t'),
                    Some(c) => Ok(c),
                    _ => Err(SyntaxError::ParseCharError {
                        message: format!("Malformed escape: \"{}\"", input.as_str()),
                        c: String::from(input.as_str()),
                    }),
                }
            }
            // Normal chars
            Some(c) => Ok(c),
            None => Err(SyntaxError::ParserError(String::from(
                "Character rule matched an empty character.",
            ))),
        })
    }

    /// Parses a char literal
    ///
    /// Example: `'a'`, `'\n'`
    pub fn char(input: Node) -> Result<AstNode<Literal>> {
        let sp = input.as_span();

        // Get the value of the char
        let res = match_nodes![
            input.into_children();
            [character(c)] => c,
        ];

        // Wrap it in an AstNode
        Ok(match res {
            Ok(c) => AstNode::from_span(&sp, Literal::Char(c)),
            Err(e) => AstNode::from_span_err(&sp, Literal::None, e),
        })
    }

    // === Strings ===

    /// Parses the content of a String
    pub fn inner_string(input: Node) -> Result<AstNode<Literal>> {
        let sp = input.as_span();

        // Get the list of results from the characters
        let results: Vec<std::result::Result<char, SyntaxError>> = match_nodes![
            input.into_children();
            [character(c)..] => c.collect()
        ];

        // Declare two lists : one for the string that will be built
        let mut s = String::with_capacity(results.len());
        // And one for the errors
        let mut errors = vec![];

        // Then build the list based on each result
        for res in results {
            match res {
                // No error : append char to string
                Ok(c) => s.push(c),
                // Error
                Err(e) => {
                    // In case of char parsing error, add the parsed text to the string
                    // For example, an invalid escape sequence will be added to the string
                    if let SyntaxError::ParseCharError { c: ch, message: _ } = &e {
                        s.push_str(&ch);
                    };

                    // Add the error to the list
                    errors.push(e);
                }
            }
        }

        // Wrap the string list in a literal
        let data = Literal::String(s);

        Ok(match errors.len() {
            // No error
            0 => AstNode::from_span(&sp, data),
            // Exactly one error
            1 => AstNode::from_span_err(&sp, data, errors.remove(1)),
            // More than one error
            _ => AstNode::from_span_err(&sp, data, SyntaxError::SeveralErrors(errors)),
        })
    }

    /// Parses a string
    ///
    /// Example: `"hello world"`
    pub fn string(input: Node) -> Result<AstNode<Literal>> {
        Ok(match_nodes![
            input.into_children();
            [inner_string(s)] => s
        ])
    }

    /// Parses the content of a raw string. Used in... raw strings.
    ///
    /// Example: `Hello \n`
    pub fn inner_raw_string(input: Node) -> Result<String> {
        // Since we don't have to handle escape sequences, the parsing is much easier
        Ok(String::from(input.as_str()))
    }

    /// Parses a raw string.
    ///
    /// Example `#"Hello \n"#`
    pub fn raw_string(input: Node) -> Result<AstNode<Literal>> {
        let sp = input.as_span();

        Ok(match_nodes![
            input.into_children();
            [inner_raw_string(s)] => AstNode::from_span(&sp, Literal::String(s)),
        ])
    }

    // === Identifier ===

    /// Parses an identifier.
    ///
    /// Example: `my_variable`
    pub fn identifier(input: Node) -> Result<AstNode<Literal>> {
        Ok(AstNode::from_span(
            &input.as_span(),
            Literal::Identifier(String::from(input.as_str())),
        ))
    }

    // ===============
    // == Operators ==
    // ===============

    /// Parses a function call
    ///
    /// Example: `(4, true)`, `()`
    pub fn call(input: Node) -> Result<AstNode<PostfixOperator>> {
        let sp = input.as_span();
        Ok(AstNode::from_span(
            &sp,
            match_nodes!(
                input.into_children();
                // Params and type params given
                [type_expression_list(type_params), expression_list(params)] => PostfixOperator::Call{params, type_params},
                // Only params given
                [expression_list(params)] => PostfixOperator::Call{params, type_params: vec![]},
                // Only type params given
                [type_expression_list(type_params)] => PostfixOperator::Call{params: vec![], type_params},
                // Nothing given
                [] => PostfixOperator::Call{params:  vec![], type_params: vec![]}
            ),
        ))
    }

    /// Parses type parameters
    ///
    /// Examples: `hello, foo<bar>`
    pub fn type_expression_params(input: Node) -> Result<AstNode<PostfixOperator>> {
        Ok(AstNode::from_span(
            &input.as_span(),
            match_nodes!(
                input.into_children();
                [type_expression_list(type_params)] => PostfixOperator::Call{params: vec![], type_params},
            ),
        ))
    }

    /// Parses a list access
    ///
    /// Examples: `[4]`, `[i, j]`
    pub fn list_access(input: Node) -> Result<AstNode<Expression>> {
        let sp = input.as_span();

        Ok(match_nodes!(
            input.into_children();
            // Params given
            [expression_list(params)] => AstNode::from_span(&sp,Expression::Array(params)),
            // No params given (invalid)
            [] => AstNode::from_span_err(&sp, Expression::Array(vec![]), SyntaxError::EmptyListAccess),
        ))
    }

    /// Parses an affectation operator
    ///
    /// Examples: `=`, `+=`
    pub fn affectation_operator(input: Node) -> Result<AstNode<AffectationOperator>> {
        let sp = input.as_span();

        Ok(match input.as_str() {
            "=" => AstNode::from_span(&sp, AffectationOperator::Affect),
            "+=" => AstNode::from_span(&sp, AffectationOperator::AffSum),
            "-=" => AstNode::from_span(&sp, AffectationOperator::AffSub),
            "*=" => AstNode::from_span(&sp, AffectationOperator::AffMul),
            "/=" => AstNode::from_span(&sp, AffectationOperator::AffDiv),
            "%=" => AstNode::from_span(&sp, AffectationOperator::AffMod),
            o => AstNode::from_span_err(
                &sp,
                AffectationOperator::None,
                SyntaxError::InvalidOperator(String::from(o)),
            ),
        })
    }

    // ================
    // == Operations ==
    // ================

    /// Parses a prefix operator
    ///
    /// Examples: `++`, `not`
    pub fn prefix_operator(input: Node) -> Result<AstNode<PrefixOperator>> {
        let sp = input.as_span();

        match input.as_str() {
            // Negation
            "-" => Ok(AstNode::from_span(&sp, PrefixOperator::Neg)),
            // Logical not
            "not" => Ok(AstNode::from_span(&sp, PrefixOperator::Not)),
            // Increment/Decrement
            "++" => Ok(AstNode::from_span(&sp, PrefixOperator::Increment)),
            "--" => Ok(AstNode::from_span(&sp, PrefixOperator::Decrement)),
            // Type of
            "typeof" => Ok(AstNode::from_span(&sp, PrefixOperator::Typeof)),
            // Other: unsupported operator (grammar updated but not parser ?)
            o => Ok(AstNode::from_span_err(
                &sp,
                PrefixOperator::None,
                SyntaxError::InvalidOperator(String::from(o)),
            )),
        }
    }

    /// Parses a postfix operator
    ///
    /// Examples: `++`, `(4, true)`
    pub fn postfix_operator(input: Node) -> Result<AstNode<PostfixOperator>> {
        let sp = input.as_span();

        match input.as_str() {
            // Increment
            "++" => Ok(AstNode::from_span(&sp, PostfixOperator::Increment)),
            // Decrement
            "--" => Ok(AstNode::from_span(&sp, PostfixOperator::Decrement)),
            // Otherwise, there is a rule to parse it. Use it.
            _ => Ok(match_nodes!(input.into_children();
                [call(op)] => op,
                [list_access(op)] => AstNode::from_span(&sp, PostfixOperator::ListAccess(Box::new(op))),
            )),
        }
    }

    /// Parses a term
    ///
    /// Examples: `4`, `"hello"`
    pub fn term(input: Node) -> Result<AstNode<Expression>> {
        let sp = input.as_span();

        Ok(match_nodes!(
            input.into_children();
            // Literals
            [float(nb)] => AstNode::from_span(&sp, Expression::Literal(nb)),
            [integer(nb)] => AstNode::from_span(&sp, Expression::Literal(nb)),
            [bool(value)] => AstNode::from_span(&sp, Expression::Literal(value)),
            [char(value)] => AstNode::from_span(&sp, Expression::Literal(value)),
            [string(value)] =>  AstNode::from_span(&sp, Expression::Literal(value)),
            [raw_string(value)] => AstNode::from_span(&sp, Expression::Literal(value)),
            [identifier(value)] => AstNode::from_span(&sp, Expression::Literal(value)),
            // List
            [list_access(expr)] => expr,
            // Other expression between parentheses
            [expression(expr)] => expr,
        ))
    }

    /// Parses a unary operation
    ///
    /// Examples: ` 4+ 8`, `foo.bar`, `5 * (4 + 2)`
    pub fn unary_operation(input: Node) -> Result<AstNode<Expression>> {
        let sp = input.as_span();

        Ok(match_nodes!(
            input.into_children();
            // Prefix operation
            [prefix_operator(op), term(t)] => AstNode::from_span(&sp, Expression::PrefixOperation{
                operator: op,
                term: Box::new(t),
            }),
            // Postfix operation
            [term(t), postfix_operator(op)] => AstNode::from_span(&sp, Expression::PostfixOperation{
                operator: op,
                term: Box::new(t),
            }),
            // Both
            [prefix_operator(l), term(t), postfix_operator(r)] => AstNode::from_span(&sp, Expression::PrefixOperation{
                operator: l,
                term: Box::new(AstNode::from_span(&sp, Expression::PostfixOperation {
                    operator: r,
                    term: Box::new(t),
                })),
            }),
            // Only a term
            [term(t)] => t,
        ))
    }

    /// Parses a type unary operation, a subset of unary operation, used in types.
    ///
    /// Examples: `hello`, `foo<bar>`
    pub fn type_unary_operation(input: Node) -> Result<AstNode<Expression>> {
        let sp = input.as_span();

        Ok(match_nodes!(
            input.into_children();
            // Identifier with type parameters
            [identifier(id), type_expression_params(type_params)] => AstNode::from_span(&sp, Expression::PostfixOperation {
                operator: type_params,
                term: Box::new(AstNode::with_location(id.location, Expression::Literal(id))),
            }),
            // Identifier only
            [identifier(id)] => AstNode::from_span(&sp, Expression::Literal(id)),
        ))
    }

    /// Parses a binary operation
    ///
    /// Examples: ` 4+ 8`, `foo.bar`, `5 * (4 + 2)`
    #[prec_climb(unary_operation, PRECCLIMBER)]
    pub fn binary_operation(
        l: AstNode<Expression>,
        op: Node,
        r: AstNode<Expression>,
    ) -> Result<AstNode<Expression>> {
        let op_span = op.as_span();

        Ok(match op.as_rule() {
            // Nav
            Rule::nav => Expression::bin_operation(l, &op_span, BinaryOperator::Nav, r),
            // Math
            Rule::plus => Expression::bin_operation(l, &op_span, BinaryOperator::Add, r),
            Rule::minus => Expression::bin_operation(l, &op_span, BinaryOperator::Sub, r),
            Rule::times => Expression::bin_operation(l, &op_span, BinaryOperator::Mul, r),
            Rule::modulo => Expression::bin_operation(l, &op_span, BinaryOperator::Mod, r),
            Rule::div => Expression::bin_operation(l, &op_span, BinaryOperator::Div, r),
            Rule::exponent => Expression::bin_operation(l, &op_span, BinaryOperator::Pow, r),
            // Logical
            Rule::and => Expression::bin_operation(l, &op_span, BinaryOperator::And, r),
            Rule::or => Expression::bin_operation(l, &op_span, BinaryOperator::Or, r),
            Rule::less_than => Expression::bin_operation(l, &op_span, BinaryOperator::Lst, r),
            Rule::greater_than => Expression::bin_operation(l, &op_span, BinaryOperator::Grt, r),
            Rule::less_or_equal_to => {
                Expression::bin_operation(l, &op_span, BinaryOperator::Leq, r)
            }
            Rule::greater_or_equal_to => {
                Expression::bin_operation(l, &op_span, BinaryOperator::Geq, r)
            }
            Rule::equals => Expression::bin_operation(l, &op_span, BinaryOperator::Eq, r),
            Rule::different => Expression::bin_operation(l, &op_span, BinaryOperator::Neq, r),
            // Fallback
            _ => AstNode::with_location_err(
                InFileLocation::Span(l.location.get_start().clone(), r.location.get_end().clone()),
                Expression::None,
                SyntaxError::InvalidOperator(String::from(op.as_str())),
            ),
        })
    }

    // =================
    // == Expressions ==
    // =================

    /// Parses an expression list
    ///
    /// Exemple `4, true, foo`
    pub fn expression_list(input: Node) -> Result<NodeVec<Expression>> {
        Ok(match_nodes!(
            input.into_children();
            [expression(exp)..] => exp.collect_and_box_elems()
        ))
    }

    /// Parses a type expression list
    ///
    /// Example `String, List<i32>`
    pub fn type_expression_list(input: Node) -> Result<NodeVec<Expression>> {
        Ok(match_nodes!(
            input.into_children();
            [type_expression(exp)..] => exp.collect_and_box_elems()
        ))
    }

    /// Parses an expression.
    ///
    /// Example: `4 + hello(42, foo[0])`
    pub fn expression(input: Node) -> Result<AstNode<Expression>> {
        Ok(match_nodes!(
            input.into_children();
            [binary_operation(expr)] => expr,
        ))
    }

    /// Parses a type expression (a subset of expression that is used with types)
    ///
    /// Example: `String`
    #[prec_climb(type_unary_operation, PRECCLIMBER)]
    pub fn type_expression(
        l: AstNode<Expression>,
        op: Node,
        r: AstNode<Expression>,
    ) -> Result<AstNode<Expression>> {
        Ok(match op.as_rule() {
            // Nav
            Rule::nav => Expression::bin_operation(l, &op.as_span(), BinaryOperator::Nav, r),
            // Fallback
            _ => AstNode::with_location_err(
                InFileLocation::Span(l.location.get_start().clone(), r.location.get_end().clone()),
                Expression::None,
                SyntaxError::InvalidOperator(String::from(op.as_str())),
            ),
        })
    }

    // =====================
    // == Left Hand Sides ==
    // =====================

    /// Parses a declaration
    ///
    /// Examples: `let bar: int`, `const thing`
    pub fn declaration(input: Node) -> Result<AstNode<LeftHandSide>> {
        let sp = input.as_span();

        Ok(match_nodes!(
            input.into_children();
            // const identifier: type
            [immutable(_), identifier(name), type_expression(typ)] => AstNode::from_span(&sp, LeftHandSide::Declaration {
                name,
                var_type: Some(typ),
                mutable: false,
            }),
            // const identifier
            [immutable(_), identifier(name)] => AstNode::from_span(&sp, LeftHandSide::Declaration {
                name,
                var_type: None,
                mutable: false,
            }),
            // let identifier: type
            [mutable(_), identifier(name), type_expression(typ)] => AstNode::from_span(&sp, LeftHandSide::Declaration {
                name,
                var_type: Some(typ),
                mutable: true,
            }),
            // let identifier
            [mutable(_), identifier(name)] => AstNode::from_span(&sp, LeftHandSide::Declaration {
                name,
                var_type: None,
                mutable: true,
            }),
        ))
    }

    /// Parses a left hand side
    ///
    /// Examples: `foo`, `let bar: int`
    pub fn left_hand_side(input: Node) -> Result<AstNode<LeftHandSide>> {
        let sp = input.as_span();

        Ok(match_nodes!(
            input.into_children();
            [declaration(decl)] => decl,
            [expression(expr)] => AstNode::from_span(&sp, LeftHandSide::Expression(expr)),
        ))
    }

    // ================
    // == Parameters ==
    // ================

    /// Parses a param declaration
    ///
    /// Examples: `hello: String`
    pub fn param_declaration(input: Node) -> Result<AstNode<LeftHandSide>> {
        let sp = input.as_span();

        Ok(match_nodes!(
            input.into_children();
            // identifier: type
            [identifier(name), type_expression(typ)] => AstNode::from_span(&sp, LeftHandSide::Declaration{
                name,
                var_type: Some(typ),
                mutable: true
            }),
        ))
    }

    /// Parses a param with default value
    ///
    /// Example: `a = 4`
    pub fn param_default_value(input: Node) -> Result<AstNode<Statement>> {
        let sp = input.as_span();

        Ok(match_nodes!(
            input.into_children();
            // identifier: Type = expression
            [identifier(name), type_expression(typ), affectation_operator(op), expression(default)] => AstNode::from_span(&sp, Statement::Affectation{
                left_hand_side: AstNode::with_location(name.location, LeftHandSide::Declaration{
                    name,
                    var_type: Some(typ),
                    mutable: true
                }),
                right_hand_side: default,
                operator: op,
            }),
            // identifier = expression
            [identifier(name), affectation_operator(op), expression(default)] => AstNode::from_span(&sp, Statement::Affectation{
                left_hand_side: AstNode::with_location(name.location, LeftHandSide::Declaration{
                    name,
                    var_type: None,
                    mutable: true
                }),
                right_hand_side: default,
                operator: op,
            }),
        ))
    }

    /// Parses a parameter
    ///
    /// Examples: `a = 4`, `17`
    pub fn param(input: Node) -> Result<AstNode<Statement>> {
        let sp = input.as_span();

        Ok(match_nodes!(
            input.into_children();
            [param_declaration(decl)] => AstNode::from_span(&sp, Statement::LeftHandSide(decl)),
            [param_default_value(def)] => def,
        ))
    }

    // ==================
    // == Affectations ==
    // ==================

    /// Parses an affectation
    ///
    /// Example: `let a = 4`
    pub fn affectation(input: Node) -> Result<AstNode<Statement>> {
        let sp = input.as_span();

        Ok(match_nodes!(
            input.into_children();
            [left_hand_side(lhs), affectation_operator(op), expression(rhs)] => AstNode::from_span(&sp, Statement::Affectation{
                left_hand_side: lhs,
                operator: op,
                right_hand_side: rhs
            }),
        ))
    }

    // =============
    // == Returns ==
    // =============

    /// Parses a return statement
    ///
    /// Example: `return value`
    pub fn return_statement(input: Node) -> Result<AstNode<Statement>> {
        let sp = input.as_span();

        Ok(match_nodes!(
            input.into_children();
            [expression(expr)] => AstNode::from_span(&sp, Statement::Return(expr)),
        ))
    }

    // ===========
    // == Block ==
    // ===========

    /// Parses a block.
    ///
    /// Example: `{ let b = 3; foo(b); }`
    pub fn block(input: Node) -> Result<AstNode<Statement>> {
        let sp = input.as_span();

        Ok(match_nodes!(
            input.into_children();
            // { stmts.. } = valid input
            [op_curly_bracket(_), checked_statement(stmts).., cl_curly_bracket(_) ] => AstNode::from_span(&sp, Statement::Block(stmts.collect_and_box_elems())),
            // {} = valid input
            [op_curly_bracket(_), cl_curly_bracket(_) ] => AstNode::from_span(&sp, Statement::Block(vec![])),
            // { stmts.. = invalid input
            [op_curly_bracket(_), checked_statement(stmts)..] => AstNode::from_span_err(&sp, Statement::Block(stmts.collect_and_box_elems()), SyntaxError::UnclosedBlock),
            // { = invalid input
            [op_curly_bracket(_)] => AstNode::from_span_err(&sp, Statement::Block(vec![]), SyntaxError::UnclosedBlock),
        ))
    }

    // ============
    // == Branch ==
    // ============

    pub fn branch(input: Node) -> Result<AstNode<Statement>> {
        let sp = input.as_span();

        Ok(match_nodes!(
            input.into_children();
            // if (condition) if_stmt
            [expression(condition), statement(if_stmt)] => AstNode::from_span(&sp, Statement::Branch{
                condition: condition,
                if_statement: Box::new(if_stmt),
                else_statement: None,
            }),
            // if (condition) if_statement else else_stmt
            [expression(condition), statement(if_stmt), statement(else_stmt)] => AstNode::from_span(&sp, Statement::Branch{
                condition: condition,
                if_statement: Box::new(if_stmt),
                else_statement: Some(Box::new(else_stmt)),
            }),
        ))
    }

    // ===========
    // == Loops ==
    // ===========

    /// Parses a for loop
    ///
    /// Example: `for (let i = 0; i < 10; i++) body`
    pub fn for_loop(input: Node) -> Result<AstNode<Statement>> {
        let sp = input.as_span();

        Ok(match_nodes!(
            input.into_children();
            // for (let i = 4; i < 10; i = i + 4) body
            [affectation(init), _, expression(until), _, affectation(step), statement(body)] => AstNode::from_span(&sp, Statement::ForLoop {
                init: Box::new(init),
                until,
                step: Box::new(step),
                body: Box::new(body)
            }),
            //for (let i = 0; i < 10; i++) body
            [affectation(init), _, expression(until), _, expression(step), statement(body)] => AstNode::from_span(&sp, Statement::ForLoop {
                init: Box::new(init),
                until,
                step: Box::new(AstNode::with_location(step.location,
                    Statement::LeftHandSide(AstNode::with_location(step.location, LeftHandSide::Expression(step))))),
                body: Box::new(body)
            }),
            // for (let i in iterable) body
            [declaration(decl), expression(iterable), statement(body)] => AstNode::from_span(&sp, Statement::ForEachLoop {
                init: decl,
                iterable,
                body: Box::new(body),
            }),
        ))
    }

    /// Parses a while loop
    ///
    /// Example: `while (thing == true) body`
    pub fn while_loop(input: Node) -> Result<AstNode<Statement>> {
        let sp = input.as_span();

        Ok(match_nodes!(
            input.into_children();
            [expression(condition), statement(body)] => AstNode::from_span(&sp, Statement::WhileLoop {
                condition,
                body: Box::new(body)
            }),
        ))
    }

    // ===============
    // == Statement ==
    // ===============

    /// Parses a statement.
    ///
    /// Examples: `let a = 5;`, `while (true) { foo(4); }`
    pub fn statement(input: Node) -> Result<AstNode<Statement>> {
        let sp = input.as_span();

        Ok(match_nodes!(
            input.into_children();
            // Affectation
            [affectation(stmt), semicolon(_)] => stmt,
            // Affectation without semicolon
            [affectation(stmt)] => stmt.add_error(SyntaxError::MissingSemicolon),
            // Left Hand Side only
            [left_hand_side(lhs), semicolon(_)] => AstNode::from_span(&sp, Statement::LeftHandSide(lhs)),
            // Left Hand Side only without semicolon
            [left_hand_side(lhs)] => AstNode::from_span_err(&sp, Statement::LeftHandSide(lhs), SyntaxError::MissingSemicolon),
            // Return statement
            [return_statement(stmt), semicolon(_)] => stmt,
            // Return statement without semicolon
            [return_statement(stmt)] => stmt.add_error(SyntaxError::MissingSemicolon),
            // Block
            [block(b)] => b,
            // Branch
            [branch(b)] => b,
            // For loop
            [for_loop(stmt)] => stmt,
            // While loop
            [while_loop(wl)] => wl,
            // Only a semicolon (empty statement)
            [semicolon(_)] => AstNode::from_span(&sp, Statement::Empty),
        ))
    }

    /// Parses a checked statement. A checked statement is a statement, but also checks if there is any unsupported character.
    ///
    /// Examples: `let a = 5;`, `$`
    pub fn checked_statement(input: Node) -> Result<AstNode<Statement>> {
        let sp = input.as_span();

        Ok(match_nodes!(
            input.into_children();
            [statement(stmt)] => stmt,
            [err_unsupported_char(err)] => AstNode::from_span_err(&sp, Statement::Empty, err),
        ))
    }

    // =========================
    // == Structural elements ==
    // =========================

    // === Module ===

    /// Parses a module
    ///
    /// Example: `module tests { }`
    pub fn module(input: Node) -> Result<AstNode<StructuralElement>> {
        let sp = input.as_span();

        Ok(match_nodes!(
            input.into_children();
            [identifier(id), structural_element(m)..] => AstNode::from_span(&sp, StructuralElement::Module{
                name: id,
                body: m.collect_and_box_elems(),
            }),
        ))
    }

    // === Function ===

    /// Parses function parameters
    ///
    /// Example: `param: int, param2: void = 4`
    pub fn function_def_parameters(input: Node) -> Result<Vec<AstNode<Statement>>> {
        Ok(match_nodes!(
            input.into_children();
            [param(others)..] => {
                others.collect()
            },
        ))
    }

    /// Parses a function
    ///
    /// Example: `fun foo(bar: int) {  }`
    pub fn function(input: Node) -> Result<AstNode<StructuralElement>> {
        /// Struct that can build a Function in a more convienient way
        pub struct FunctionBuilder {
            name: AstNode<Literal>,
            params: Vec<AstNode<Statement>>,
            body: AstNode<Statement>,
            return_type: Option<AstNode<Expression>>,
            type_params: NodeVec<Expression>,
            public: bool,
        }

        impl FunctionBuilder {
            /// Create a new function
            pub fn new(name: AstNode<Literal>, body: AstNode<Statement>) -> FunctionBuilder {
                FunctionBuilder {
                    name,
                    params: vec![],
                    body,
                    return_type: None,
                    type_params: vec![],
                    public: false,
                }
            }

            /// Add type params to the given function builder
            pub fn params(mut self, params: Vec<AstNode<Statement>>) -> Self {
                self.params = params;
                self
            }

            /// Add type params to the given function builder
            pub fn type_params(mut self, type_params: NodeVec<Expression>) -> Self {
                self.type_params = type_params;
                self
            }

            /// Add return type to the given function builder
            pub fn returns(mut self, return_type: AstNode<Expression>) -> Self {
                self.return_type = Some(return_type);
                self
            }

            /// Add return type to the given function builder
            pub fn public(mut self) -> Self {
                self.public = true;
                self
            }

            /// Finalize the function and return the structural element
            pub fn build(self) -> StructuralElement {
                StructuralElement::Function {
                    name: self.name,
                    params: self.params,
                    body: self.body,
                    return_type: self.return_type,
                    type_params: self.type_params,
                    public: self.public,
                }
            }
        }

        Ok(AstNode::from_span(
            &input.as_span(),
            match_nodes!(
                input.into_children();

                // fun id(p) b
                [identifier(id), function_def_parameters(p), block(b)] => FunctionBuilder::new(id, b).params(p).build(),
                // fun id() b
                [identifier(id), block(b)] => FunctionBuilder::new(id, b).build(),
                // fun id(p) -> r b
                [identifier(id), function_def_parameters(p), expression(r), block(b)] => FunctionBuilder::new(id, b).params(p).returns(r).build(),
                // fun id() -> r b
                [identifier(id), expression(r),  block(b)] => FunctionBuilder::new(id, b).returns(r).build(),

                // fun id<t>(p) b
                [identifier(id), type_expression_list(t), function_def_parameters(p), block(b)] => FunctionBuilder::new(id, b).type_params(t).params(p).build(),
                // fun id<t>() b
                [identifier(id), type_expression_list(t), block(b)] => FunctionBuilder::new(id, b).type_params(t).build(),
                // fun id<t>(p) -> r b
                [identifier(id), type_expression_list(t), function_def_parameters(p), expression(r), block(b)] => FunctionBuilder::new(id, b).type_params(t).params(p).returns(r).build(),
                // fun id<t>() -> r b
                [identifier(id), type_expression_list(t), expression(r),  block(b)] => FunctionBuilder::new(id, b).type_params(t).returns(r).build(),

                // pub fun id(p) b
                [public(_), identifier(id), function_def_parameters(p), block(b)] => FunctionBuilder::new(id, b).params(p).public().build(),
                // pub fun id() b
                [public(_), identifier(id), block(b)] => FunctionBuilder::new(id, b).public().build(),
                // pub fun id(p) -> r b
                [public(_), identifier(id), function_def_parameters(p), expression(r), block(b)] => FunctionBuilder::new(id, b).params(p).returns(r).public().build(),
                // pub fun id() -> r b
                [public(_), identifier(id), expression(r),  block(b)] => FunctionBuilder::new(id, b).returns(r).public().build(),

                // pub fun id<t>(p) b
                [public(_), identifier(id), type_expression_list(t), function_def_parameters(p), block(b)] => FunctionBuilder::new(id, b).type_params(t).params(p).public().build(),
                // pub fun id<t>() b
                [public(_), identifier(id), type_expression_list(t), block(b)] => FunctionBuilder::new(id, b).type_params(t).public().build(),
                // pub fun id<t>(p) -> r b
                [public(_), identifier(id), type_expression_list(t), function_def_parameters(p), expression(r), block(b)] => FunctionBuilder::new(id, b).type_params(t).params(p).returns(r).public().build(),
                // pub fun id<t>() -> r b
                [public(_), identifier(id), type_expression_list(t), expression(r),  block(b)] => FunctionBuilder::new(id, b).type_params(t).returns(r).public().build(),
            ),
        ))
    }

    // === Import ===

    /// Parses an import
    ///
    /// Example: `import library;`
    pub fn import(input: Node) -> Result<AstNode<StructuralElement>> {
        let sp = input.as_span();

        Ok(match_nodes!(
            input.into_children();
            [expression(expr), _] => AstNode::from_span(&sp, StructuralElement::Import(expr)),
        ))
    }

    // === Struct ===

    /// Parses a struct
    ///
    /// Example: `struct Thing { a: int }`
    pub fn struct_def(input: Node) -> Result<AstNode<StructuralElement>> {
        /// Struct that can build a Struct in a more convienient way
        pub struct StructBuilder {
            name: AstNode<Literal>,
            fields: Vec<AstNode<Statement>>,
            type_params: NodeVec<Expression>,
            public: bool,
        }

        impl StructBuilder {
            pub fn new(name: AstNode<Literal>) -> StructBuilder {
                StructBuilder {
                    name,
                    fields: vec![],
                    type_params: vec![],
                    public: false,
                }
            }

            pub fn fields(mut self, fields: Vec<AstNode<Statement>>) -> StructBuilder {
                self.fields = fields;
                self
            }

            pub fn type_params(mut self, type_params: NodeVec<Expression>) -> StructBuilder {
                self.type_params = type_params;
                self
            }

            pub fn public(mut self) -> StructBuilder {
                self.public = true;
                self
            }

            pub fn build(self) -> StructuralElement {
                StructuralElement::Struct {
                    name: self.name,
                    fields: self.fields,
                    type_params: self.type_params,
                    public: self.public,
                }
            }
        }

        Ok(AstNode::from_span(
            &input.as_span(),
            match_nodes!(
                input.into_children();
                // struct id {}
                [identifier(id)] => StructBuilder::new(id).build(),
                // struct id { f1: type }
                [identifier(id), function_def_parameters(f)] => StructBuilder::new(id).fields(f).build(),
                // pub struct id {}
                [public(_), identifier(id)] => StructBuilder::new(id).public().build(),
                // pub struct id { f1: type }
                [public(_), identifier(id), function_def_parameters(f)] => StructBuilder::new(id).fields(f).public().build(),
                // struct id<t> {}
                [identifier(id), type_expression_list(t)] => StructBuilder::new(id).type_params(t).build(),
                // struct id<t> { f1: type }
                [identifier(id), type_expression_list(t), function_def_parameters(f)] => StructBuilder::new(id).type_params(t).fields(f).build(),
                // pub struct id<t> {}
                [public(_), identifier(id), type_expression_list(t)] => StructBuilder::new(id).type_params(t).public().build(),
                // pub struct id<t> { f1: type }
                [public(_), identifier(id), type_expression_list(t), function_def_parameters(f)] => StructBuilder::new(id).type_params(t).fields(f).public().build(),
            ),
        ))
    }

    // === Enums ===

    /// Parses a list of enumeration variants
    ///
    /// Example: `On, Off`
    pub fn enumeration_variants(input: Node) -> Result<Vec<AstNode<Literal>>> {
        Ok(match_nodes!(
            input.into_children();
            [identifier(variants)..] => variants.collect(),
        ))
    }

    /// Parses an Enum
    ///
    /// Example: `enum State { On, Off }`
    pub fn enumeration(input: Node) -> Result<AstNode<StructuralElement>> {
        Ok(AstNode::from_span(
            &input.as_span(),
            match_nodes!(
                input.into_children();
                // Public enum
                [public(_), identifier(name), enumeration_variants(variants)] => StructuralElement::Enum {
                    name,
                    variants,
                    public: true,
                },
                // Private enum
                [identifier(name), enumeration_variants(variants)] => StructuralElement::Enum {
                    name,
                    variants,
                    public: false,
                },
            ),
        ))
    }

    // === Structural element ===

    /// Parses a structural element
    ///
    /// Example: `struct a { f: int }` `fun foo() {}`
    pub fn structural_element(input: Node) -> Result<AstNode<StructuralElement>> {
        let sp = input.as_span();

        Ok(match_nodes!(
            input.into_children();
            // Module
            [module(m)] => m,
            // Function
            [function(f)] => f,
            // Import
            [import(i)] => i,
            // Struct
            [struct_def(s)] => s,
            // Enum
            [enumeration(e)] => e,
            // Just a statement
            [statement(s)] => AstNode::from_span(&sp, StructuralElement::Statement(s)),
            // Unsupported char
            [err_unsupported_char(err)] => AstNode::from_span_err(&sp, StructuralElement::None, err),
        ))
    }

    // =================
    // == Global file ==
    // =================

    /// Parses an entire file
    pub fn file(input: Node) -> Result<AstNode<File>> {
        let sp = input.as_span();

        Ok(match_nodes!(
            input.into_children();
            // Wrap the file in a AstNode as well so we can store the beginning and end of it
            [structural_element(structural).., _] => AstNode::from_span(&sp, structural.collect()),
        ))
    }
}

/// # Parse file
///
/// Main function of the parser. Given an input string, it returns an Abstract Syntax Tree (AST)
/// representing its content.
///
/// The file is parsed using Pest. When it fails, it stops and pest returns an Error.
///
/// To avoid stopping everything on error, most errors are parsed successfully without telling pest.
/// The errors are then annotated in the AST. They must be handled before anything is done with the Ast.
///
/// For more details, see [Ast].
pub fn parse_file(input_str: &str, file_path: String) -> std::result::Result<Ast, Error> {
    let result = parse_with_pest(input_str);

    match result {
        // Parsing sucessful : return the AST
        Ok(root) => Ok(Ast {
            file_name: file_path,
            root_node: root,
        }),
        // Error found: return an Error
        Err(err) => Err(Error::from_pest_error(err, file_path, input_str)),
    }
}

/// Parses a given string with the pest parser
fn parse_with_pest(input_str: &str) -> Result<AstNode<File>> {
    // Parse the input into `Nodes`
    let inputs = NokeParser::parse(Rule::file, input_str)?;
    // There should be a single root node in the parsed tree
    let input = inputs.single()?;
    // Consume the `Node` recursively into the final value
    NokeParser::file(input)
}

pub fn rule_to_str(rule: Rule) -> Option<String> {
    match rule {
        Rule::statement | Rule::checked_statement => Some(String::from("Statement")),
        Rule::expression
        | Rule::binary_operation
        | Rule::unary_operation
        | Rule::term
        | Rule::expression_list => Some(String::from("Expression")),
        Rule::file | Rule::structural_element => Some(String::from(
            "Structural Element (import, module, function, struct, enum or statement)",
        )),
        Rule::type_expression | Rule::type_unary_operation | Rule::type_expression_list => {
            Some(String::from("Type"))
        }
        Rule::type_expression_params => Some(String::from("Type parameters")),
        Rule::param => Some(String::from("Parameter")),
        Rule::identifier => Some(String::from("Identifier")),

        // Literals
        Rule::dec_integer | Rule::hex_integer | Rule::bin_integer | Rule::integer => {
            Some(String::from("Int literal"))
        }
        Rule::float => Some(String::from("Float literal")),
        Rule::bool => Some(String::from("Bool literal")),
        Rule::character | Rule::char => Some(String::from("Char literal")),
        Rule::inner_string | Rule::string | Rule::inner_raw_string | Rule::raw_string => {
            Some(String::from("String literal"))
        }

        // Operators
        Rule::plus
        | Rule::minus
        | Rule::times
        | Rule::div
        | Rule::exponent
        | Rule::modulo
        | Rule::less_or_equal_to
        | Rule::greater_or_equal_to
        | Rule::less_than
        | Rule::greater_than
        | Rule::equals
        | Rule::different
        | Rule::and
        | Rule::or
        | Rule::nav
        | Rule::binary_operator => Some(String::from("Binary operator")),
        Rule::type_binary_operator => Some(String::from("Navigation operator \".\"")),
        Rule::affectation_operator => Some(String::from("Affectation operator")),
        Rule::prefix_operator => Some(String::from("Prefix operator")),
        Rule::postfix_operator => Some(String::from(
            "Postfix operator (including function calls and list indexes)",
        )),
        // Miscellaneous
        Rule::semicolon => Some(String::from(";")),
        Rule::cl_curly_bracket => Some(String::from("}")),
        Rule::op_curly_bracket => Some(String::from("{")),
        Rule::block => Some(String::from("Block")),
        Rule::EOI => Some(String::from("End of file")),
        // Others, just use the enum name
        _ => Some(format!("{:?}", rule)),
    }
}

impl Error {
    fn from_pest_error(err: pest::error::Error<Rule>, file_path: String, input_str: &str) -> Error {
        let mut hint = None;

        let message = match err.variant {
            pest::error::ErrorVariant::CustomError { message } => message,
            pest::error::ErrorVariant::ParsingError {
                positives,
                negatives: _,
            } => {
                // Filter the matching rules
                let mut shown_rules = vec![];
                for rule in positives {
                    if let Some(rule_str) = rule_to_str(rule) {
                        // Set the hint in common situations
                        if rule == Rule::semicolon {
                            hint = Some(String::from("Did you forget a semicolon ?"));
                        } else if rule == Rule::op_curly_bracket {
                            hint = Some(String::from("Did you forget an opening bracket ?"));
                        } else if rule == Rule::cl_curly_bracket {
                            hint = Some(String::from("Did you forget a closing bracket ?"));
                        } else if rule == Rule::EOI {
                            hint = Some(String::from(
                                "Did you forget an opening bracket or parenthese ?",
                            ));
                        } else if rule == Rule::statement {
                            hint = Some(String::from("Did you forget a loop or if condition ?"));
                        } else if rule == Rule::param {
                            hint = Some(String::from("Did you forget a closing parenthese ?"));
                        } else if rule == Rule::block {
                            hint = Some(String::from("Did you forget an opening parenthese ?"));
                        }

                        // Push the rule
                        if !shown_rules.contains(&rule_str) {
                            shown_rules.push(rule_str);
                        }
                    }
                }

                // Sort the rules
                shown_rules.sort();

                // Create the message
                let mut msg = String::from("Expected one of the following:");
                for rule in shown_rules {
                    msg += &format!("\n\t-> {}", rule);
                }
                msg
            }
        };

        // Get the location of the error

        let location = NodeLocation {
            file_path,
            // Location in the file
            location: match err.line_col {
                pest::error::LineColLocation::Pos((l, c)) => {
                    // Ponctual: both location and line col should be Pos
                    if let pest::error::InputLocation::Pos(p) = err.location {
                        InFileLocation::Ponctual(InFilePosition {
                            line: l,
                            col: c,
                            pos: p,
                        })
                    } else {
                        panic!("line_col and location should have the same variant. Is there an error with Pest ?")
                    }
                }
                pest::error::LineColLocation::Span((sl, sc), (el, ec)) => {
                    // Span: both location and line col should be Span
                    if let pest::error::InputLocation::Span((sp, ep)) = err.location {
                        InFileLocation::Span(
                            InFilePosition {
                                line: sl,
                                col: sc,
                                pos: sp,
                            },
                            InFilePosition {
                                line: el,
                                col: ec,
                                pos: ep,
                            },
                        )
                    } else {
                        panic!("line_col and location should have the same variant. Is there an error with Pest ?")
                    }
                }
            },
        };

        // Get the content of the line
        let line_content = match input_str.lines().nth(location.get_start().line - 1) {
            Some(l) => Some(String::from(l)),
            None => None,
        };

        // Return the error
        Error {
            variant: ErrorVariant::Syntax,
            location,
            line_content,
            message,
            hint,
            severity: Severity::Error,
        }
    }
}
