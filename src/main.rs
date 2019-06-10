#![feature(slice_patterns)]
#![feature(box_syntax, box_patterns)]

mod evalml3;
mod evalml4;
mod evalml5;
mod nameless_ml3;

use crate::evalml5::eval::Env;
use crate::evalml5::lexer;
use crate::evalml5::parser;

fn main() {
    let s = " let rec max = fun l -> match l with 
       x :: [] -> x 
     | x :: y :: z -> if x < y then max (y :: z) else max (x :: z) in
   max (9 :: 2 :: 3 :: [])";
    let s = " match [] with [] -> 1";
    //let s = "let f = fun x -> x + 1 in f 5";
    //let s = "let a = [] in match a with [] -> 5 | a :: b -> 6";
    let tokens = lexer::str_to_tokens(s);
    println!("{:?}", tokens);
    let e = parser::parse(&tokens);
    println!("{:?}", e);
    let r = e.solve(&Env::None);
    println!("{:?}", r);
    println!("{}", r.string(0));
}
