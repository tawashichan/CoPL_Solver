#![feature(slice_patterns)]
#![feature(box_syntax,box_patterns)]

mod evalml3;

use crate::evalml3::lexer;
use crate::evalml3::parser;
use crate::evalml3::eval::{Env};

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
   let s = "let rec x = fun y -> if y < 1 then 0 else x (y - 1) in x 1";
   let s = "let rec fact = fun n ->
   if n < 2 then 1 else n * fact (n - 1) in
   fact 3";
   let s = " let rec fib = fun n -> if n < 3 then 1 else fib (n - 1) + fib (n - 2) in
   fib 20";
   let s = "let rec sum = fun f -> fun n ->
     if n < 1 then 0 else f n + sum f (n - 1) in 
   sum (fun x -> x * x) 2";
   let s = "let fact = fun self -> fun n ->
     if n < 2 then 1 else n * self self (n - 1) in
   fact fact 3";
    //let s = "let a = fun x -> (x 1) + 1 in a fun x -> x";  
    //let s = "eb ec ed ee";
    //let s = "ea eb ec + ed ee";
    let tokens = lexer::str_to_tokens(s);
    println!("{:?}",tokens);
    let e = parser::parse(&tokens);
    println!("{:?}",e);
    let r = e.solve(&Env::None);
    println!("{}",r.string(0));

}   
