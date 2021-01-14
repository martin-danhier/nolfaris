extern crate pest_consume;
extern crate colored;

pub mod parser;
use std::fs;
use parser::parse_file;

pub mod error;

fn main() {

    println!("\n\n\n\n\n\n");

    let unparsed_file = fs::read_to_string("./src/numbers.idk").expect("cannot read file");

    let a = parse_file(&unparsed_file);

    match a {
        Ok(ast) => println!("{:?}", ast),
        Err(error) => println!("{}", error),
    };

    println!();
}