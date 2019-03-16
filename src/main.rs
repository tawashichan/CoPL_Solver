#![feature(slice_patterns)]
#![feature(box_syntax, box_patterns)]

mod evalml3;
mod nameless_ml3;

use crate::evalml3::eval::Env;
use crate::evalml3::lexer;
use crate::evalml3::parser;

fn main() {
    //let s = "let max = fun x -> fun y -> if x < y then y else x in max 3 5";
    let s = "let twice = fun f -> fun x -> f (f x) in twice (fun x -> x * x) 2";
    let s = "let twice = fun f -> fun x -> f (f x) in twice twice (fun x -> x * x) 2";
    let s = "let compose = fun f -> fun g -> fun x -> f (g x) in 
    let p = fun x -> x * x in
    let q = fun x -> x + 4 in
    compose p q 4";
    let s = "let s = fun f -> fun g -> fun x -> f x (g x) in
    let k = fun x -> fun y -> x in
    s k k 7";
    let tokens = lexer::str_to_tokens(s);
    println!("{:?}", tokens);
    let e = parser::parse(&tokens);
    println!("{:?}", e);
    let r = e.solve(&Env::None);
    println!("{}", r.string(0));
}
