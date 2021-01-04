use pest::prec_climber as pcl;
use pest_consume::{match_nodes, Error, Parser};
use std::str::FromStr;

// Import ast
pub mod ast;
use ast::*;

// Custom types

#[allow(dead_code)] // Rust incorrectly thinks that the typedef is not used
type Result<T> = std::result::Result<T, Error<Rule>>;
#[allow(dead_code)]
type Node<'i> = pest_consume::Node<'i, Rule, ()>;

// Derive main parser struct
#[derive(Parser)]
#[grammar = "./parser/grammar.pest"]
struct NokeParser;

// Create prec climber lazily
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

#[pest_consume::parser]
impl NokeParser {
    /// End of input
    pub fn EOI(_input: Node) -> Result<()> {
        Ok(())
    }

    // =================
    // == Expressions ==
    // =================

    // Integers

    pub fn dec_integer(input: Node) -> Result<i32> {
        i32::from_str(input.as_str()).map_err(|e| input.error(e))
    }

    pub fn hex_integer(input: Node) -> Result<i32> {
        i32::from_str_radix(input.as_str(), 16).map_err(|e| input.error(e))
    }

    pub fn bin_integer(input: Node) -> Result<i32> {
        i32::from_str_radix(input.as_str(), 2).map_err(|e| input.error(e))
    }

    pub fn integer(input: Node) -> Result<i32> {
        Ok(match_nodes![
            input.into_children();
            [dec_integer(nb)] => nb,
            [hex_integer(nb)] => nb,
            [bin_integer(nb)] => nb
        ])
    }

    // Floats

    pub fn float(input: Node) -> Result<f32> {
        f32::from_str(input.as_str()).map_err(|e| input.error(e))
    }

    // Booleans

    pub fn bool(input: Node) -> Result<bool> {
        match input.as_str() {
            "true" => Ok(true),
            "false" => Ok(false),
            l => Err(input.error(format!("Invalid bool literal: \"{:?}\".", l))),
        }
    }

    // Char

    pub fn character(input: Node) -> Result<char> {
        let mut chars = input.as_str().chars();

        match chars.next() {
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
                                Err(input
                                    .error(format!("Invalid character: \"{}\".", input.as_str())))
                            }
                        }
                    }
                    Some('b') => Ok('\x08'),
                    Some('f') => Ok('\x0c'),
                    Some('n') => Ok('\n'),
                    Some('r') => Ok('\r'),
                    Some('t') => Ok('\t'),
                    Some(c) => Ok(c),
                    _ => Err(input.error(format!("Malformed escape: \"{}\"", input.as_str()))),
                }
            }
            // Normal chars
            Some(c) => Ok(c),
            None => Err(input.error("Character rule matched an empty character.")),
        }
    }

    pub fn char(input: Node) -> Result<char> {
        Ok(match_nodes![
            input.into_children();
            [character(c)] => c,
        ])
    }

    // Strings

    pub fn inner_string(input: Node) -> Result<String> {
        Ok(match_nodes![
            input.into_children();
            [character(c)..] => c.collect()
        ])
    }

    pub fn string(input: Node) -> Result<String> {
        Ok(match_nodes![
            input.into_children();
            [inner_string(str)] => str
        ])
    }

    pub fn inner_raw_string(input: Node) -> Result<String> {
        Ok(String::from(input.as_str()))
    }

    pub fn raw_string(input: Node) -> Result<String> {
        Ok(match_nodes![
            input.into_children();
            [inner_raw_string(str)] => str
        ])
    }

    // Identifier

    pub fn identifier(input: Node) -> Result<Box<Expr>> {
        Ok(Box::new(Expr::Identifier(String::from(input.as_str()))))
    }

    //  Function call and indexing

    pub fn call(input: Node) -> Result<Vec<Box<Expr>>> {
        Ok(match_nodes!(
            input.into_children();
            [expression_list(params)] => params,
            [] => vec![],
        ))
    }

    pub fn indexing(input: Node) -> Result<Vec<Box<Expr>>> {
        Ok(match_nodes!(
            input.into_children();
            [expression_list(params)] => params,
            [] => vec![]
        ))
    }

    pub fn expression_list(input: Node) -> Result<Vec<Box<Expr>>> {
        Ok(match_nodes!(
            input.into_children();
            [expression(exp)..] => exp.collect()
        ))
    }




    // Expressions

    pub fn prefix_operator(input: Node) -> Result<PrefixOpcode> {
        match input.as_str() {
            "-" => Ok(PrefixOpcode::Neg),
            "not" => Ok(PrefixOpcode::Not),
            "++" => Ok(PrefixOpcode::Increment),
            "--" => Ok(PrefixOpcode::Decrement),
            "typeof" => Ok(PrefixOpcode::Typeof),
            o => Err(input.error(format!("\"{:?}\" isn't an operator.", o)))?,
        }
    }

    pub fn postfix_operator(input: Node) -> Result<PostfixOpcode> {
        match input.as_str() {
            "++" => Ok(PostfixOpcode::Increment),
            "--" => Ok(PostfixOpcode::Decrement),
            _ => {
                Ok(match_nodes!(input.into_children();
                    [call(op)] => PostfixOpcode::Call(op),
                    [indexing(op)] => PostfixOpcode::Indexing(op),
                ))
            }
        }
    }

    pub fn term(input: Node) -> Result<Box<Expr>> {
        Ok(match_nodes!(
            input.into_children();
            [float(nb)] => Box::new(Expr::Float(nb)),
            [integer(nb)] => Box::new(Expr::Integer(nb)),
            [expression(expr)] => expr,
            [bool(value)] => Box::new(Expr::Bool(value)),
            [char(value)] => Box::new(Expr::Char(value)),
            [string(value)] => Box::new(Expr::String(value)),
            [raw_string(value)] => Box::new(Expr::String(value)),
            [identifier(value)] => value,
            [indexing(expr)] => Box::new(Expr::Array(expr)),
        ))
    }

    #[prec_climb(unary_operation, PRECCLIMBER)]
    pub fn binary_operation(l: Box<Expr>, op: Node, r: Box<Expr>) -> Result<Box<Expr>> {
        match op.as_rule() {
            Rule::nav => Ok(Box::new(Expr::BinOp(l, BinOpcode::Nav, r))),
            // Math
            Rule::plus => Ok(Box::new(Expr::BinOp(l, BinOpcode::Add, r))),
            Rule::minus => Ok(Box::new(Expr::BinOp(l, BinOpcode::Sub, r))),
            Rule::times => Ok(Box::new(Expr::BinOp(l, BinOpcode::Mul, r))),
            Rule::modulo => Ok(Box::new(Expr::BinOp(l, BinOpcode::Mod, r))),
            Rule::div => Ok(Box::new(Expr::BinOp(l, BinOpcode::Div, r))),
            Rule::exponent => Ok(Box::new(Expr::BinOp(l, BinOpcode::Pow, r))),
            // Logical
            Rule::and => Ok(Box::new(Expr::BinOp(l, BinOpcode::And, r))),
            Rule::or => Ok(Box::new(Expr::BinOp(l, BinOpcode::Or, r))),
            Rule::less_than => Ok(Box::new(Expr::BinOp(l, BinOpcode::Lst, r))),
            Rule::greater_than => Ok(Box::new(Expr::BinOp(l, BinOpcode::Grt, r))),
            Rule::less_or_equal_to => Ok(Box::new(Expr::BinOp(l, BinOpcode::Leq, r))),
            Rule::greater_or_equal_to => Ok(Box::new(Expr::BinOp(l, BinOpcode::Geq, r))),
            Rule::equals => Ok(Box::new(Expr::BinOp(l, BinOpcode::Eq, r))),
            Rule::different => Ok(Box::new(Expr::BinOp(l, BinOpcode::Neq, r))),
            // Fallback
            r => Err(op.error(format!("Rule {:?} isn't an operator.", r)))?,
        }
    }

    pub fn unary_operation(input: Node) -> Result<Box<Expr>> {
        Ok(match_nodes!(
            input.into_children();
            // term
            [prefix_operator(op), term(t)] => Box::new(Expr::PrefixOp(op, t)),
            [term(t), postfix_operator(op)] => Box::new(Expr::PostfixOp(t, op)),
            [prefix_operator(l), term(t), postfix_operator(r)] => Box::new(
                Expr::PrefixOp(l, Box::new(Expr::PostfixOp(t, r)))
            ),
            [term(t)] => t,
        ))
    }

    pub fn expression(input: Node) -> Result<Box<Expr>> {
        Ok(match_nodes!(
            input.into_children();
            // [unary_operation(expr)] => expr,
            [binary_operation(expr)] => expr,
        ))
    }

    // ================
    // == Statements ==
    // ================

    pub fn semicolon(input: Node) -> Result<()> {
        Ok(())
    }


    pub fn statement(input: Node) -> Result<Box<Statement>> {
        Ok(match_nodes!(
            input.into_children();
            [assignment(stmt), _] => stmt,
            [left_hand_side(lhs), _] => Box::new(Statement::LeftHandSide(lhs)),
            [block(b)] => b,
            [branch(b)] => b,
            [for_loop(stmt)] => stmt,
        ))
    }

    // assignment

    pub fn mutable(input: Node) -> Result<()> {
        Ok(())
    }

    pub fn immutable(input: Node) -> Result<()> {
        Ok(())
    }

    pub fn declaration(input: Node) -> Result<Box<LeftHandSide>> {
        Ok(match_nodes!(
            input.into_children();
            [immutable(_), identifier(id), expression(typ)] => Box::new(LeftHandSide::Declaration(id, Some(typ), false)),
            [immutable(_), identifier(id)] => Box::new(LeftHandSide::Declaration(id, None, false)),
            [mutable(_), identifier(id), expression(typ)] => Box::new(LeftHandSide::Declaration(id, Some(typ), true)),
            [mutable(_), identifier(id)] => Box::new(LeftHandSide::Declaration(id, None, true)),
        ))
    }

    pub fn left_hand_side(input: Node) -> Result<Box<LeftHandSide>> {
        Ok(match_nodes!(
            input.into_children();
            [declaration(decl)] => decl,
            [expression(expr)] => Box::new(LeftHandSide::Expression(expr)),
        ))
    }

    pub fn assignment(input: Node) -> Result<Box<Statement>> {
        Ok(match_nodes!(
            input.into_children();
            [left_hand_side(lhs), expression(expr)] => Box::new(Statement::Assignment(lhs, expr)),
        ))
    }

    // Block

    pub fn block(input: Node) -> Result<Box<Statement>> {
        Ok(match_nodes!(
            input.into_children();
            [statement(stmts)..] => Box::new(Statement::Block(stmts.collect())),
            [] => Box::new(Statement::Block(vec![])),
        ))
    }

    // Branch

    pub fn branch(input: Node) -> Result<Box<Statement>> {
        Ok(match_nodes!(
            input.into_children();
            [expression(condition), statement(if_stmt)] => Box::new(Statement::Branch(condition, if_stmt, None)),
            [expression(condition), statement(if_stmt), statement(else_stmt)] => Box::new(Statement::Branch(condition, if_stmt, Some(else_stmt))),
        ))
    }

    // For loop

    pub fn for_loop(input: Node) -> Result<Box<Statement>> {
        Ok(match_nodes!(
            input.into_children();
            [assignment(init), _, expression(until), _, assignment(step), statement(body)] => Box::new(Statement::For(init, until, step, body)),
            [assignment(init), _, expression(until), _, expression(step), statement(body)] => Box::new(Statement::For(
                init,
                until,
                Box::new(Statement::LeftHandSide(
                    Box::new(LeftHandSide::Expression(step))
                )),
                body
            )),
            [declaration(decl), expression(iterable), statement(body)] => Box::new(Statement::ForEach(decl, iterable, body)),
        ))
    }

    // =================
    // == Global file ==
    // =================

    pub fn file(input: Node) -> Result<Vec<Box<Statement>>> {
        Ok(match_nodes!(
            input.into_children();
            [statement(stmt).., _] => stmt.collect(),
        ))
    }
}

pub fn parse_file(input_str: &str) -> Result<Vec<Box<Statement>>> {
    // Parse the input into `Nodes`
    let inputs = NokeParser::parse(Rule::file, input_str)?;
    // There should be a single root node in the parsed tree
    let input = inputs.single()?;
    // Consume the `Node` recursively into the final value
    NokeParser::file(input)
}
