#[derive(Clone,Debug,PartialEq)]
pub enum Token {
    INT(i64),
    LPAR,
    RPAR,
    LBRACKET, // [
    RBRACKET, // ]
    LT,
    EQUAL,
    PLUS,
    MINUS,
    MUL,
    LET,
    FUNCTION,
    VAR(String),
    NOT,
    TRUE,
    FALSE,
    IN,
    IF,
    RARROW,
    THEN,
    ELSE,
    EOF,
}

fn split_string(s: &str) -> Vec<char> {
    s.chars().collect()
}

fn get_str(str_vec: &[char]) -> (String, &[char]) {
    get_str_sub(str_vec, "".to_string())
}

fn get_str_sub(str_vec: &[char],acm: String) -> (String,&[char]) {
    match str_vec {
        [first,rest..] => match first {
            '\"' => (acm,rest),
            _c => get_str_sub(rest,format!("{}{}",acm,first))
        }
        &[] => (acm,&[]),
    }
}

fn get_keyword(str_vec: &[char]) -> (Token, &[char]){
    get_keyword_sub(str_vec,"".to_string())
}

fn get_keyword_sub(str_vec: &[char], acm: String) -> (Token, &[char]) {
    match str_vec {
        [first, rest..] if first.is_alphabetic() => {
            get_keyword_sub(rest, format!("{}{}", acm, first))
        }
        _ =>  match &*acm {
                "let" => (Token::LET, str_vec),
                "fun" => (Token::FUNCTION, str_vec),
                "true" => (Token::TRUE, str_vec),
                "false" => (Token::FALSE, str_vec),
                "in" => (Token::IN, str_vec),
                "if" => (Token::IF, str_vec),
                "then" => (Token::THEN, str_vec),
                "else" => (Token::ELSE, str_vec),
                s =>  (Token::VAR(s.to_string()),str_vec)
            }
    }
}

fn get_num_str(str_vec: &[char]) -> (String, &[char],bool,bool) {
    get_num_str_sub(str_vec,"".to_string(),false,false)
}


fn get_num_str_sub(str_vec: &[char], acm: String,is_float: bool,is_minus: bool) -> (String, &[char],bool,bool) {
    match &str_vec[..] {
        [first,rest..] => {
            if first.is_numeric() {
                get_num_str_sub(rest, format!("{}{}",acm,first),is_float,is_minus)
            } else if *first == '-' && is_minus == false {
                get_num_str_sub(rest, format!("{}{}",acm,first),is_float,is_minus)
            } else if *first == '.' && is_float == false {
                get_num_str_sub(rest,format!("{}{}",acm,first),true,is_minus)
            } else {
                (acm,str_vec,is_float,is_minus)
            }
        }
        &[] => (acm, &[],is_float,is_minus)
    }
}

//nightlyじゃないとvectorを分解できない...
fn next_token(slice: &[char]) -> (Token, &[char]) {
    match slice {
        [first, rest..] => match first {
            '\n' => next_token(rest),
            ' ' => next_token(rest),
            '\t' => next_token(rest),
            '=' => (Token::EQUAL, rest),
            '(' => (Token::LPAR, rest),
            ')' => (Token::RPAR, rest),
            '[' => (Token::LBRACKET, rest),
            ']' => (Token::RBRACKET, rest),
            '+' => (Token::PLUS, rest),
            '-' => match rest {
                ['>',res..] => (Token::RARROW,res),
                _ => (Token::MINUS, rest),
            }
            '<' => (Token::LT,rest),
            '*' => (Token::MUL, rest),
            c =>
                if c.is_numeric() || *c == '-' {
                    let (num_str, re,is_float,_) = get_num_str(slice); //moveもmutableな参照もしてないからここでslice使える
                    let num = num_str.parse::<i64>().unwrap();
                    (Token::INT(num), re)
                } else {
                    get_keyword(slice)
                }
        },
        [] => (Token::EOF, &[])
    }
}

fn get_tokens<'a>(slice: &[char],acm: &'a mut Vec<Token>) -> &'a Vec<Token> {
    match next_token(slice) {
        (Token::EOF,_) => acm,
        (token,slice) => {
            acm.push(token);
            get_tokens(slice,acm)
        },
    }
}

pub fn str_to_tokens(str: &str) -> Vec<Token> {
    let str_vec = split_string(str);
    get_tokens(&str_vec,&mut vec![]).to_owned()
}

