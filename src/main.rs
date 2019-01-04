#![feature(slice_patterns)]
#![feature(box_syntax,box_patterns)]

mod ast;
mod lexer;
mod evalml3;

use crate::evalml3::*;
use crate::evalml3::Fun;

fn main() {
    //let exp = Exp::Let(Let::new("y", Exp::Int(2),Exp::Fun(Fun::new("x", Exp::Op(Op::Plus,box Exp::Var("x"),box Exp::Var("y"))))));
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
    let exp = Exp::Let(
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
            );
    let r = exp.solve(&Env::None);
    println!("{}",r.string());
}   
