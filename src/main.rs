extern crate pest_consume;

pub mod parser;
use std::fs;
use parser::parse_file;

fn main() {
    let unparsed_file = fs::read_to_string("./src/numbers.idk").expect("cannot read file");

    let a = parse_file(&unparsed_file).expect("compilation failed");

    println!("{:?}", a);
}