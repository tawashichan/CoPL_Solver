#![feature(slice_patterns)]
#![feature(box_syntax,box_patterns)]

mod ast;
mod lexer;
mod evalml3;
mod parser;

use crate::evalml3::*;
use crate::evalml3::Fun;


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
    //let s = "let a = fun x -> (x 1) + 1 in a fun x -> x";  
    //let s = "eb ec ed ee";
    //let s = "ea eb ec + ed ee";
    let tokens = lexer::str_to_tokens(s);
    println!("{:?}",tokens);
    let e = parser::parse(&tokens);
    println!("{:?}",e);
    let r = e.solve(&Env::None);
    println!("{}",r.string(0));
    //let e = Exp::Let(Let::new("y", Exp::Int(2),Exp::Fun(Fun::new("x", Exp::Op(Op::Plus,box Exp::Var("x"),box Exp::Var("y"))))));
    /*let exp = Exp::Let(
                Let::new("sq", 
                    Exp::Fun(
                        Fun::new(
                            "x", 
                            Exp::Op(
                                Op::Times,
                                box Exp::Var("x"),
                                box Exp::Var("x")
                            )
                        )
                    ),
                    Exp::Op(
                        Op::Plus,                           
                        box Exp::App(
                            box Exp::Var("sq"),
                            box Exp::Int(3)
                        ),
                        box Exp::App(
                            box Exp::Var("sq"),
                            box Exp::Int(4)
                        )
                    )        
                )
            );*/
    /*let exp = Exp::Let(
                Let::new("sm", 
                    Exp::Fun(
                        Fun::new(
                            "f", 
                            Exp::Op(
                                Op::Plus,
                                box Exp::App(box Exp::Var("f"),box Exp::Int(3)),
                                box Exp::App(box Exp::Var("f"),box Exp::Int(4)),
                            )
                        )
                    ),
                    Exp::App(
                        box Exp::Var("sm"),
                        box Exp::Fun(
                            Fun::new(
                                "x", 
                                Exp::Op(
                                    Op::Times,
                                    box Exp::Var("x"),
                                    box Exp::Var("x"),
                                )
                            )
                        ),
                    ),       
                )
            );*/
    /*let exp = Exp::Let(
        Let::new(
            "twice",
            Exp::Fun(
                Fun::new(
                    "f",
                    Exp::Fun(
                        Fun::new(
                            "x", 
                            Exp::App(
                                box Exp::Var("f"),
                                box Exp::App(
                                    box Exp::Var("f"),
                                    box Exp::Var("x")
                                )
                            )
                        )
                    )
                )
            ), 
            Exp::App(
                box Exp::App(
                    box Exp::Var("twice"),
                    box Exp::Fun(
                        Fun::new(
                            "x", 
                            Exp::Op(
                                Op::Times,
                                box Exp::Var("x"),
                                box Exp::Var("x")
                            )
                        )
                    )
                ),
                box Exp::Int(2)
            )
        )
    );
    let r = exp.solve(&Env::None);
    println!("{}",r.string(0));
    println!("{}",r.value().string());*/
}   
