#![feature(slice_patterns)]
#![feature(box_syntax, box_patterns)]

mod evalml3;
mod nameless_ml3;
mod evalml4;

use crate::evalml4::eval::Env;
use crate::evalml4::lexer;
use crate::evalml4::parser;

fn main() {
    let s = "
        let f = fun x -> match x with 
        [] -> 0 
        | a :: b -> a 
        in f (4::[]) + f [] + f (1 :: 2 :: 3 :: [])
    ";
    let tokens = lexer::str_to_tokens(s);
    println!("{:?}", tokens);
    let e = parser::parse(&tokens);
    println!("{:?}", e);
    let r = e.solve(&Env::None);
    println!("{}", r.string(0));
}
