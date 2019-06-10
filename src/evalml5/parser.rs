use crate::evalml5::eval::{Clauses, Exp, Fun, Let, LetRec, Op, Pattern, RecFun, Var};
use crate::evalml5::lexer::Token;

pub fn parse(tokens: &[Token]) -> Exp {
    let (exp, rest) = parse_exp(tokens);
    match rest {
        &[] => exp,
        _ => panic!("{:?} {:?}", exp, rest),
    }
}

fn parse_exp(tokens: &[Token]) -> (Exp, &[Token]) {
    let (exp, rest) = parse_op_exp(tokens);
    match rest {
        [Token::PLUS, res..] => {
            let (ex, re) = parse_exp(res);
            (Exp::Op(Op::Plus, box exp, box ex), re)
        }
        [Token::MINUS, res..] => {
            let (ex, re) = parse_exp(res);
            (Exp::Op(Op::Minus, box exp, box ex), re)
        }
        [Token::EQUAL, res..] => {
            let (ex, re) = parse_exp(res);
            (Exp::Op(Op::Equal, box exp, box ex), re)
        }
        [Token::JOINER, res..] => {
            let (ex, re) = parse_exp(res);
            (Exp::Cons(box exp, box ex), re)
        }
        [Token::LET, Token::VAR(s), Token::EQUAL, rest..] => parse_exp(rest),
        [Token::RPAR, res..] => (exp, rest),
        [Token::IN, res..] => (exp, rest),
        &[] => (exp, rest),
        _ => (exp, rest),
    }
}

fn is_exp(tokens: &[Token]) -> bool {
    match tokens {
        [Token::LPAR, rest..] => true,
        [Token::INT(i), rest..] => true,
        [Token::VAR(s), rest..] => true,
        [Token::TRUE, rest..] => true,
        [Token::FALSE, rest..] => true,
        [Token::LET, rest..] => true,
        [Token::FUNCTION, rest..] => true,
        [Token::MATCH, rest..] => true,
        [Token::LBRACKET, rest..] => true,
        _ => false,
    }
}

fn parse_app(exp: Exp, tokens: &[Token]) -> (Exp, &[Token]) {
    if is_exp(tokens) {
        let (ex, rest) = parse_term(tokens);
        parse_app(Exp::App(box exp, box ex), rest)
    } else {
        (exp, tokens)
    }
}

fn parse_match(tokens: &[Token]) -> (Exp, &[Token]) {
    match tokens {
        [Token::MATCH, rest..] => {
            let (exp, res) = parse_exp(rest);
            match res {
                [Token::WITH, re..] => {
                    let (c, r) = parse_clauses(re);
                    (Exp::Match(box exp, box c), r)
                }
                _ => panic!("{:?}", res),
            }
        }
        _ => panic!("{:?}", tokens),
    }
}

fn parse_clauses(tokens: &[Token]) -> (Clauses, &[Token]) {
    let (pattern, rest) = parse_pattern(tokens);
    match rest {
        [Token::RARROW, res..] => {
            let (exp, re) = parse_exp(res);
            match re {
                [Token::BAR, r..] => {
                    let (c, rr) = parse_clauses(r);
                    (Clauses::Complex(pattern, exp, box c), rr)
                }
                _ => (Clauses::Simple(pattern, exp), re),
            }
        }
        _ => panic!("{:?}", tokens),
    }
}

fn parse_pattern(tokens: &[Token]) -> (Pattern, &[Token]) {
    let (p1, rest) = parse_pattern_sub(tokens);
    match rest {
        [Token::JOINER, res..] => {
            let (p2, re) = parse_pattern(res);
            (Pattern::Cons(box p1, box p2), re)
        }
        _ => (p1, rest),
    }
}

fn parse_pattern_sub(tokens: &[Token]) -> (Pattern, &[Token]) {
    match tokens {
        [Token::ANY, rest..] => (Pattern::Any, rest),
        [Token::VAR(v), rest..] => (Pattern::Var(v.clone()), rest),
        [Token::LBRACKET, Token::RBRACKET, rest..] => (Pattern::Nil, rest),
        [Token::LPAR,rest..] => {
            let (p,res) = parse_pattern(rest);
            match res {
                [Token::RPAR,re..] => (p,re),
                _ => panic!("{:?}", res),
            }
        }
        _ => panic!("{:?}", tokens),
    }
}

fn parse_let(tokens: &[Token]) -> (Exp, &[Token]) {
    match tokens {
        [Token::LET, Token::VAR(s), Token::EQUAL, rest..] => {
            let (exp, res) = parse_exp(rest);
            match res {
                [Token::IN, re..] => {
                    let (ex, r) = parse_exp(re);
                    (Exp::Let(Let::new(s.clone(), exp, ex)), r)
                }
                _ => panic!("{:?}", res),
            }
        }
        _ => panic!(""),
    }
}

fn parse_let_rec(tokens: &[Token]) -> (Exp, &[Token]) {
    match tokens {
        [Token::LET, Token::REC, Token::VAR(s), Token::EQUAL, rest..] => {
            let (rec_fun, res) = parse_rec_fun(s.clone(), rest);
            match res {
                [Token::IN, re..] => {
                    let (ex, r) = parse_exp(re);
                    (Exp::LetRec(LetRec::new(s.clone(), rec_fun.clone(), ex)), r)
                }
                _ => panic!("{:?}", res),
            }
        }
        _ => panic!(""),
    }
}

fn parse_op_exp(tokens: &[Token]) -> (Exp, &[Token]) {
    let (exp, rest) = parse_term(tokens);
    match rest {
        [Token::MUL, res..] => {
            let (ex, re) = parse_op_exp(res);
            (Exp::Op(Op::Times, box exp, box ex), re)
        }
        [Token::LT, res..] => {
            let (ex, re) = parse_op_exp(res);
            (Exp::Op(Op::Lt, box exp, box ex), re)
        }
        _ => parse_app(exp, rest),
    }
}

fn parse_term(tokens: &[Token]) -> (Exp, &[Token]) {
    match tokens {
        [Token::LPAR, rest..] => {
            let (exp, res) = parse_exp(rest);
            match res {
                [Token::RPAR, re..] => (exp, re),
                _ => panic!("{:?}", res),
            }
        }
        [Token::INT(i), rest..] => (Exp::Int(*i), rest),
        [Token::VAR(s), rest..] => (Exp::Var(s.clone()), rest),
        [Token::TRUE, rest..] => (Exp::Bool(true), rest),
        [Token::FALSE, rest..] => (Exp::Bool(false), rest),
        [Token::LET, Token::REC, rest..] => parse_let_rec(tokens),
        [Token::LET, rest..] => parse_let(tokens),
        [Token::FUNCTION, rest..] => parse_fun(tokens),
        [Token::IF, rest..] => parse_if(tokens),
        [Token::LBRACKET, Token::RBRACKET, rest..] => (Exp::Nil, rest),
        [Token::MATCH, rest..] => parse_match(tokens),
        _ => panic!("{:?}", tokens),
    }
}

fn parse_if(tokens: &[Token]) -> (Exp, &[Token]) {
    match tokens {
        [Token::IF, rest..] => {
            let (exp, res) = parse_exp(rest);
            match res {
                [Token::THEN, re..] => {
                    let (ex, r) = parse_exp(re);
                    match r {
                        [Token::ELSE, rr..] => {
                            let (e, rrr) = parse_exp(rr);
                            (Exp::If(box exp, box ex, box e), rrr)
                        }
                        _ => panic!("{:?}", res),
                    }
                }
                _ => panic!("{:?}", rest),
            }
        }
        _ => panic!("{:?}", tokens),
    }
}

fn parse_fun(tokens: &[Token]) -> (Exp, &[Token]) {
    match tokens {
        [Token::FUNCTION, Token::VAR(s), Token::RARROW, rest..] => {
            let (exp, res) = parse_exp(rest);
            (Exp::Fun(Fun::new(s.clone(), exp)), res)
        }
        _ => panic!("{:?}", tokens),
    }
}

fn parse_rec_fun(var: Var, tokens: &[Token]) -> (RecFun, &[Token]) {
    match tokens {
        [Token::FUNCTION, Token::VAR(s), Token::RARROW, rest..] => {
            let (exp, res) = parse_exp(rest);
            (RecFun::new(var.clone(), s.clone(), exp), res)
        }
        _ => panic!("{:?}", tokens),
    }
}

#[test]
fn parse_pattern1() {
    let tokens = vec![Token::VAR("x".to_string())];
    let (p, _) = parse_pattern(&tokens);
    assert_eq!(p, Pattern::Var("x".to_string()))
}

#[test]
fn parse_pattern2() {
    let tokens = vec![
        Token::VAR("x".to_string()),
        Token::JOINER,
        Token::VAR("y".to_string()),
    ];
    let (p, _) = parse_pattern(&tokens);
    assert_eq!(
        p,
        Pattern::Cons(
            box Pattern::Var("x".to_string()),
            box Pattern::Var("y".to_string())
        )
    )
}

#[test]
fn parse_pattern3() {
    let tokens = vec![Token::VAR("x".to_string()), Token::RARROW];
    let (p, _) = parse_pattern(&tokens);
    assert_eq!(p, Pattern::Var("x".to_string()))
}
