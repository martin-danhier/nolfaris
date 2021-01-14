use pest::prec_climber as pcl;
use pest_consume::{match_nodes, Parser};
use std::str::FromStr;

// Import ast
pub mod ast;
use ast::*;

use crate::error::{Error, ErrorLocation, ErrorVariant, InFileLocation, InFilePosition, Severity};

// Custom types

#[allow(dead_code)] // Rust incorrectly thinks that the typedef is not used
type Result<T> = std::result::Result<T, pest_consume::Error<Rule>>;
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

    pub fn call(input: Node) -> Result<PostfixOpcode> {
        Ok(match_nodes!(
            input.into_children();
            [type_expression_list(type_params), expression_list(params)] => PostfixOpcode::Call(FunctionCall {
                params, type_params,
            }),
            [expression_list(params)] => PostfixOpcode::Call(FunctionCall {
                params,
                type_params: vec![],
            }),
            [type_expression_list(type_params)] => PostfixOpcode::Call(FunctionCall {
                params: vec![],
                type_params: type_params,
            }),
            [] => PostfixOpcode::Call(FunctionCall {
                params: vec![],
                type_params: vec![],
            })
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
            _ => Ok(match_nodes!(input.into_children();
                [call(op)] => op,
                [indexing(op)] => PostfixOpcode::Indexing(op),
            )),
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

    // Type expressions (used in types)

    pub fn type_expression_list(input: Node) -> Result<Vec<Box<Expr>>> {
        Ok(match_nodes!(
            input.into_children();
            [type_expression(exprs)..] => exprs.collect(),
        ))
    }

    pub fn type_expression_params(input: Node) -> Result<Vec<Box<Expr>>> {
        Ok(match_nodes!(
            input.into_children();
            [type_expression_list(type_params)] => type_params,
        ))
    }

    pub fn type_unary_operation(input: Node) -> Result<Box<Expr>> {
        Ok(match_nodes!(
            input.into_children();
            [identifier(id), type_expression_params(type_params)] => Box::new(Expr::PostfixOp(
                id, PostfixOpcode::Call(FunctionCall {
                    type_params,
                    params: vec![],
                })
            )),
            [identifier(id)] => id,
        ))
    }

    #[prec_climb(type_unary_operation, PRECCLIMBER)]
    pub fn type_expression(l: Box<Expr>, op: Node, r: Box<Expr>) -> Result<Box<Expr>> {
        match op.as_rule() {
            Rule::nav => Ok(Box::new(Expr::BinOp(l, BinOpcode::Nav, r))),
            // Fallback
            r => Err(op.error(format!("Rule {:?} isn't an operator.", r)))?,
        }
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
            [affectation(stmt), _] => stmt,
            [left_hand_side(lhs), _] => Box::new(Statement::LeftHandSide(lhs)),
            [return_statement(stmt), _] => stmt,
            [block(b)] => b,
            [branch(b)] => b,
            [for_loop(stmt)] => stmt,
            [while_loop(wl)] => wl,
            [semicolon(_)] => Box::new(Statement::Empty),
        ))
    }

    // assignment

    pub fn mutable(input: Node) -> Result<()> {
        Ok(())
    }

    pub fn immutable(input: Node) -> Result<()> {
        Ok(())
    }

    pub fn param_declaration(input: Node) -> Result<Box<LeftHandSide>> {
        Ok(match_nodes!(
            input.into_children();
            [identifier(id), type_expression(typ)] => Box::new(LeftHandSide::Declaration(id, Some(typ), true)),
        ))
    }

    pub fn param_default_value(input: Node) -> Result<Box<Statement>> {
        Ok(match_nodes!(
            input.into_children();
            [identifier(id), type_expression(typ), expression(default)] => Box::new(Statement::Affectation(Affectation {
                left_hand_side: Box::new(LeftHandSide::Declaration(id, Some(typ), true)),
                right_hand_side: default,
                op: AffectationOpcode::Affect,
            })),
            [identifier(id), expression(default)] => Box::new(Statement::Affectation(Affectation {
                left_hand_side: Box::new(LeftHandSide::Declaration(id, None, true)),
                right_hand_side: default,
                op: AffectationOpcode::Affect,
            })),
        ))
    }

    pub fn param(input: Node) -> Result<Box<Statement>> {
        Ok(match_nodes!(
            input.into_children();
            [param_declaration(decl)] => Box::new(Statement::LeftHandSide(decl)),
            [param_default_value(def)] => def,
        ))
    }

    pub fn declaration(input: Node) -> Result<Box<LeftHandSide>> {
        Ok(match_nodes!(
            input.into_children();
            [immutable(_), identifier(id), type_expression(typ)] => Box::new(LeftHandSide::Declaration(id, Some(typ), false)),
            [immutable(_), identifier(id)] => Box::new(LeftHandSide::Declaration(id, None, false)),
            [mutable(_), identifier(id), type_expression(typ)] => Box::new(LeftHandSide::Declaration(id, Some(typ), true)),
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

    pub fn affectation_operator(input: Node) -> Result<AffectationOpcode> {
        match input.as_str() {
            "=" => Ok(AffectationOpcode::Affect),
            "+=" => Ok(AffectationOpcode::AffSum),
            "-=" => Ok(AffectationOpcode::AffSub),
            "*=" => Ok(AffectationOpcode::AffMul),
            "/=" => Ok(AffectationOpcode::AffDiv),
            "%=" => Ok(AffectationOpcode::AffMod),
            o => Err(input.error(format!("\"{:?}\" isn't an assignment operator.", o)))?,
        }
    }

    pub fn affectation(input: Node) -> Result<Box<Statement>> {
        Ok(match_nodes!(
            input.into_children();
            [left_hand_side(lhs), affectation_operator(op), expression(expr)] => Box::new(Statement::Affectation(Affectation{
                left_hand_side: lhs,
                op,
                right_hand_side: expr
            })),
        ))
    }

    pub fn return_statement(input: Node) -> Result<Box<Statement>> {
        Ok(match_nodes!(
            input.into_children();
            [expression(expr)] => Box::new(Statement::Return(expr)),
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
            [expression(condition), statement(if_stmt)] => Box::new(Statement::Branch(Branch{
                condition: condition,
                if_statement: if_stmt,
                else_statement: None,
            })),
            [expression(condition), statement(if_stmt), statement(else_stmt)] => Box::new(Statement::Branch(Branch{
                condition: condition,
                if_statement: if_stmt,
                else_statement: Some(else_stmt),
            })),
        ))
    }

    // For loop

    pub fn for_loop(input: Node) -> Result<Box<Statement>> {
        Ok(match_nodes!(
            input.into_children();
            [affectation(init), _, expression(until), _, affectation(step), statement(body)] => Box::new(Statement::For(ForLoop {
                init, until, step, body
            })),
            [affectation(init), _, expression(until), _, expression(step), statement(body)] => Box::new(Statement::For(ForLoop {
                init,
                until,
                step: Box::new(Statement::LeftHandSide(
                    Box::new(LeftHandSide::Expression(step))
                )),
                body
             })),
            [declaration(decl), expression(iterable), statement(body)] => Box::new(Statement::ForEach(ForEachLoop{init: decl, iterable, body})),
        ))
    }

    // While loop

    pub fn while_loop(input: Node) -> Result<Box<Statement>> {
        Ok(match_nodes!(
            input.into_children();
            [expression(condition), statement(body)] => Box::new(Statement::While(WhileLoop{condition, body})),
        ))
    }

    // =========================
    // == Structural elements ==
    // =========================

    /// Module
    pub fn module(input: Node) -> Result<Box<StructuralElement>> {
        Ok(match_nodes!(
            input.into_children();
            [identifier(id), structural_element(m)..] => Box::new(StructuralElement::Module(Module {
                name: id,
                body: m.collect(),
            })),
        ))
    }

    /// Function
    pub fn function_def_parameters(input: Node) -> Result<Vec<Box<Statement>>> {
        Ok(match_nodes!(
            input.into_children();
            [param(others)..] => {
                others.collect()
            },
        ))
    }

    pub fn public(input: Node) -> Result<()> {
        Ok(())
    }

    pub fn function(input: Node) -> Result<Box<StructuralElement>> {
        Ok(match_nodes!(
            input.into_children();
            [identifier(id), function_def_parameters(params), block(body)] => Box::new(StructuralElement::Function(Function {
                name: id,
                params: params,
                return_type: None,
                body: body,
                type_params: vec![],
                public: false,
            })),
            [identifier(id), block(body)] => Box::new(StructuralElement::Function(Function {
                name: id,
                params: vec![],
                return_type: None,
                body: body,
                type_params: vec![],
                public: false,
            })),
            [identifier(id), function_def_parameters(params), expression(return_type), block(body)] => Box::new(StructuralElement::Function(Function {
                name: id,
                params: params,
                return_type: Some(return_type),
                body: body,
                type_params: vec![],
                public: false,
            })),
            [identifier(id), expression(return_type),  block(body)] => Box::new(StructuralElement::Function(Function {
                name: id,
                params: vec![],
                return_type: Some(return_type),
                body: body,
                type_params: vec![],
                public: false,
            })),
            [identifier(id), type_expression_list(type_params), function_def_parameters(params), block(body)] => Box::new(StructuralElement::Function(Function {
                name: id,
                params: params,
                return_type: None,
                body: body,
                type_params,
                public: false,
            })),
            [identifier(id), type_expression_list(type_params), block(body)] => Box::new(StructuralElement::Function(Function {
                name: id,
                params: vec![],
                return_type: None,
                body: body,
                type_params,
                public: false,
            })),
            [identifier(id), type_expression_list(type_params), function_def_parameters(params), expression(return_type), block(body)] => Box::new(StructuralElement::Function(Function {
                name: id,
                params: params,
                return_type: Some(return_type),
                body: body,
                type_params,
                public: false,
            })),
            [identifier(id), type_expression_list(type_params), expression(return_type),  block(body)] => Box::new(StructuralElement::Function(Function {
                name: id,
                params: vec![],
                return_type: Some(return_type),
                body: body,
                type_params,
                public: false,
            })),
            [public(_), identifier(id), function_def_parameters(params), block(body)] => Box::new(StructuralElement::Function(Function {
                name: id,
                params: params,
                return_type: None,
                body: body,
                type_params: vec![],
                public: true,
            })),
            [public(_), identifier(id), block(body)] => Box::new(StructuralElement::Function(Function {
                name: id,
                params: vec![],
                return_type: None,
                body: body,
                type_params: vec![],
                public: true,
            })),
            [public(_), identifier(id), function_def_parameters(params), expression(return_type), block(body)] => Box::new(StructuralElement::Function(Function {
                name: id,
                params: params,
                return_type: Some(return_type),
                body: body,
                type_params: vec![],
                public: true,
            })),
            [public(_), identifier(id), expression(return_type),  block(body)] => Box::new(StructuralElement::Function(Function {
                name: id,
                params: vec![],
                return_type: Some(return_type),
                body: body,
                type_params: vec![],
                public: true,
            })),
            [public(_), identifier(id), type_expression_list(type_params), function_def_parameters(params), block(body)] => Box::new(StructuralElement::Function(Function {
                name: id,
                params: params,
                return_type: None,
                body: body,
                type_params,
                public: true,
            })),
            [public(_), identifier(id), type_expression_list(type_params), block(body)] => Box::new(StructuralElement::Function(Function {
                name: id,
                params: vec![],
                return_type: None,
                body: body,
                type_params,
                public: true,
            })),
            [public(_), identifier(id), type_expression_list(type_params), function_def_parameters(params), expression(return_type), block(body)] => Box::new(StructuralElement::Function(Function {
                name: id,
                params: params,
                return_type: Some(return_type),
                body: body,
                type_params,
                public: true,
            })),
            [public(_), identifier(id), type_expression_list(type_params), expression(return_type),  block(body)] => Box::new(StructuralElement::Function(Function {
                name: id,
                params: vec![],
                return_type: Some(return_type),
                body: body,
                type_params,
                public: true,
            })),
        ))
    }

    // Import

    pub fn import(input: Node) -> Result<Box<StructuralElement>> {
        Ok(match_nodes!(
            input.into_children();
            [expression(expr), _] => Box::new(StructuralElement::Import(expr)),
        ))
    }

    // Struct

    pub fn struct_def(input: Node) -> Result<Box<StructuralElement>> {
        Ok(match_nodes!(
            input.into_children();
            [identifier(id)] => Box::new(StructuralElement::Struct(Struct{
                name: id,
                fields: vec![],
                public: false,
                type_params: vec![],
            })),
            [identifier(id), function_def_parameters(fields)] => Box::new(StructuralElement::Struct(Struct{
                name: id,
                fields,
                public: false,
                type_params: vec![],
            })),
            [public(_), identifier(id)] => Box::new(StructuralElement::Struct(Struct{
                name: id,
                fields: vec![],
                public: true,
                type_params: vec![],
            })),
            [public(_), identifier(id), function_def_parameters(fields)] => Box::new(StructuralElement::Struct(Struct{
                name: id,
                fields,
                public: true,
                type_params: vec![],
            })),
            [identifier(id), type_expression_list(type_params)] => Box::new(StructuralElement::Struct(Struct{
                name: id,
                fields: vec![],
                public: false,
                type_params,
            })),
            [identifier(id), type_expression_list(type_params), function_def_parameters(fields)] => Box::new(StructuralElement::Struct(Struct{
                name: id,
                fields,
                public: false,
                type_params,
            })),
            [public(_), identifier(id), type_expression_list(type_params)] => Box::new(StructuralElement::Struct(Struct{
                name: id,
                fields: vec![],
                public: true,
                type_params,
            })),
            [public(_), identifier(id), type_expression_list(type_params), function_def_parameters(fields)] => Box::new(StructuralElement::Struct(Struct{
                name: id,
                fields,
                public: true,
                type_params,
            })),
        ))
    }

    // Enumerations

    pub fn enumeration_variants(input: Node) -> Result<Vec<Box<Expr>>> {
        Ok(match_nodes!(
            input.into_children();
            [identifier(variants)..] => variants.collect(),
        ))
    }

    pub fn enumeration(input: Node) -> Result<Box<StructuralElement>> {
        Ok(match_nodes!(
            input.into_children();
            [public(_), identifier(name), enumeration_variants(variants)] => Box::new(StructuralElement::Enum(Enumeration {
                name,
                variants,
                public: true,
            })),
            [identifier(name), enumeration_variants(variants)] => Box::new(StructuralElement::Enum(Enumeration {
                name,
                variants,
                public: false,
            })),
        ))
    }

    pub fn structural_element(input: Node) -> Result<Box<StructuralElement>> {
        Ok(match_nodes!(
            input.into_children();
            [module(m)] => m,
            [function(f)] => f,
            [import(i)] => i,
            [struct_def(s)] => s,
            [enumeration(e)] => e,
            [statement(s)] => Box::new(StructuralElement::Statement(s)),
            [err_unsupported_char(c)] => c,
        ))
    }

    // ====================
    // == Error handling ==
    // ====================

    pub fn err_unsupported_char(input: Node) -> Result<Box<StructuralElement>> {
        Ok(Box::new(StructuralElement::Error))
    }


    // =================
    // == Global file ==
    // =================

    pub fn file(input: Node) -> Result<Vec<Box<StructuralElement>>> {
        Ok(match_nodes!(
            input.into_children();
            [structural_element(structural).., _] => structural.collect(),
        ))
    }
}

pub fn parse_file(input_str: &str) -> std::result::Result<Vec<Box<StructuralElement>>, Error> {
    let result = parse_with_pest(input_str);

    match result {
        Ok(res) => Ok(res),
        Err(err) => Err(Error::from_pest_error(
            err,
            String::from("src/numbers.idk"),
            input_str,
        )),
    }
}

fn parse_with_pest(input_str: &str) -> Result<Vec<Box<StructuralElement>>> {
    // Parse the input into `Nodes`
    let inputs = NokeParser::parse(Rule::file, input_str)?;
    // There should be a single root node in the parsed tree
    let input = inputs.single()?;
    // Consume the `Node` recursively into the final value
    NokeParser::file(input)
}

pub fn rule_to_str(rule: Rule) -> Option<String> {
    match rule {
        Rule::statement => Some(String::from("Statement")),
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
                        } else if rule == Rule::EOI {
                            hint = Some(String::from(
                                "Did you forget an opening bracket or parenthese ?",
                            ));
                        } else if rule == Rule::statement {
                            hint = Some(String::from("Did you forget a closing bracket ? Did you forget a loop or if condition ?"));
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

        let location = ErrorLocation {
            file_path,
            // Location in the file
            location: match err.line_col {
                pest::error::LineColLocation::Pos((l, c)) => {
                    InFileLocation::Ponctual(InFilePosition { line: l, col: c })
                }
                pest::error::LineColLocation::Span((sl, sc), (el, ec)) => InFileLocation::Span(
                    InFilePosition { line: sl, col: sc },
                    InFilePosition { line: el, col: ec },
                ),
            },
        };

        let line_content = match input_str.lines().nth(location.get_start().line - 1) {
            Some(l) => Some(String::from(l)),
            None => None,
        };

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
