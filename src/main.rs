pub mod ast;
pub mod utils;

// pub mod grammar;
#[macro_use]
extern crate lalrpop_util;
lalrpop_mod!(pub grammar);

fn main() {



    // Write type explicitly in order to provide useful compiler-time checking and autocompletion
    let mut errors: Vec<
        lalrpop_util::ErrorRecovery<usize, lalrpop_util::lexer::Token, &'static str>,
    > = Vec::new();
    let parsed_content: Result<
        Box<ast::Expression>,
        lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token, &'static str>,
    >;

    // Parse the text
    parsed_content = grammar::ExprParser::new().parse(&mut errors, "888888888888888888888");

    match parsed_content {
        Ok(v) => println!("{:?} {:?}", v, errors),
        Err(e) => print!("Unrecoverable error : {}", e),
    }
}
