extern crate pest_consume;
extern crate colored;

pub mod parser;
use std::fs;

use parser::parser::parse_file;

pub mod error;

pub mod utils;

fn main() {

    println!("\n\n");

    let unparsed_file = fs::read_to_string("./src/numbers.idk").expect("cannot read file");

    let a = parse_file(&unparsed_file, String::from("./src/numbers.idk"));

    match a {
        Ok(_) => (),
        Err(error) => println!("{}", error),
    };

    println!();
}