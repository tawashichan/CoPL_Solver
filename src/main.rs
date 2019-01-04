#![feature(slice_patterns)]
#![feature(box_syntax,box_patterns)]

mod ast;
mod lexer;
mod evalml3;

use crate::evalml3::*;
use crate::evalml3::Fun;

fn main() {
    let exp = Exp::Fun(Fun::new("x",Exp::Op(Op::Plus,box Exp::Var("x"),box Exp::Int(1))));
    println!("{}",exp.solve().string());
}
