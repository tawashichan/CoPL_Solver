#![feature(slice_patterns)]
#![feature(box_syntax,box_patterns)]

mod ast;
mod lexer;

use crate::ast::{Number,Expr,ApplyRule};

fn main() {

    let sample = ApplyRule::EConst(Number(1));
    let a = ApplyRule::EPLUS(
        Expr::Number(Number(1)),
        Expr::Number(Number(1)),
        box ApplyRule::EConst(Number(1)),
        box ApplyRule::EConst(Number(1)),
        box ApplyRule::PSucc(
            Number(1),
            Number(1),
            box ApplyRule::PZero(Number(1))
            )
        );

    //let exp = Expr::Plus(box Expr::Number(Number(1)),box Expr::Number(Number(1)));    
    //let exp = Expr::Plus(box Expr::Plus(box Expr::Number(Number(1)),box Expr::Number(Number(1))),box Expr::Number(Number(1)));    
    let exp = Expr::Mul(
        box Expr::Number(Number(0)),
        box Expr::Plus(
            box Expr::Number(Number(2)),
            box Expr::Number(Number(2))
        ),
    );

    /*let exp = Expr::Plus(
        box Expr::Plus(
            box Expr::Number(Number(1)),
            box Expr::Number(Number(1))
        ),
        box Expr::Number(Number(1)),
    );*/    
   /*let exp = Expr::Plus(
        box Expr::Number(Number(3)),
        box Expr::Mul(
            box Expr::Number(Number(2)),
            box Expr::Number(Number(1)),
        ),
    ); */
    let r = exp.get_rule();

    println!("{:?}",r.string());
}
