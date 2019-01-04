#[derive(Debug,Clone)]
pub enum Exp {
    Int(Int),
    Bool(Bool),
    Var(Var),
    Op(Op,Box<Exp>,Box<Exp>),
    If(Box<Exp>,Box<Exp>,Box<Exp>),
    Let(Var,Box<Exp>,Box<Exp>),
    Fun(Fun),
    App(Box<Exp>,Box<Exp>),
    RecFun(RecFun),
}

#[derive(Debug,Clone)]
pub enum Value {
    Int(Int),
    Bool(Bool),
    Closure(Env,Fun),
    Closurerec(Env,RecFun),
}

pub type Int = i64;

pub type Bool = bool;

pub type Var = &'static str;

#[derive(Debug,Clone)]
pub struct Fun(Var,Box<Exp>);

#[derive(Debug,Clone)]
pub struct RecFun(Var,Var,Box<Exp>,Box<Exp>);

#[derive(Debug,Clone)]
pub enum Op{
    Plus,
    Minus,
    Times,
}

#[derive(Debug,Clone)]
pub enum Env {
    None,
    Env(Box<Env>,Var,Box<Value>)
}

#[derive(Debug,Clone)]
pub enum Rule{
    EInt(Env,Int),
    EBool(Env,Bool),
    EIfT(Env,Exp,Exp,Box<Rule>,Box<Rule>),
    EIfF(Env,Exp,Exp,Box<Rule>,Box<Rule>),
    EPlus(Env,Exp,Exp,Box<Rule>,Box<Rule>,Box<Rule>),
    EMinus(Env,Exp,Exp,Box<Rule>,Box<Rule>,Box<Rule>),
    ETimes(Env,Exp,Exp,Box<Rule>,Box<Rule>,Box<Rule>),
    ELt(Env,Exp,Exp,Box<Rule>,Box<Rule>,Box<Rule>),
    EVar1(Env,Var),
    EVar2(Env,Box<Rule>,Var),
    ELet(Env,Var,Exp,Exp,Box<Rule>,Box<Rule>),
    EFun(Env,Fun),
    EApp(Env,Exp,Exp,Box<Rule>,Box<Rule>,Box<Rule>),
    BPlus(Int,Int),
    BMinus(Int,Int),
    BTimes(Int,Int),
    BLt(Int,Int),
}

impl Exp {

    pub fn eval(&self) -> Value {
        match self {
            Exp::Fun(Fun(v1,box Exp::Op(op,box Exp::Var(v2),box Exp::Int(i)))) => Value::Int(1),
            _ => Value::Int(-1000)
        }
    }

    pub fn string(&self) -> String{
        match self {
            Exp::Int(i) => format!("{}",i),
            Exp::Op(op,box e1,box e2) => match op {
                Op::Plus => format!("{} + {}",e1.string(),e2.string()),
                Op::Minus => format!("{} - {}",e1.string(),e2.string()),
                Op::Times=> format!("{} * {}",e1.string(),e2.string()),
            },
            Exp::Fun(Fun(var,box exp)) => {
                format!("fun {} -> {}",var,exp.string())
            },
            Exp::Var(var) => {
                format!("{}",var)
            },
            _ => panic!()
        }
    }

    pub fn solve(&self) -> Rule{
        match self {
            Exp::Fun(fun) => Rule::EFun(Env::None,fun.clone()),
            _ => panic!()
        }
    }
}

impl Rule {

    pub fn string(&self) -> String {
        match self {
            Rule::EFun(env,fun) =>{
                let cl = Value::Closure(env.clone(),fun.clone());
                format!("{} |- {} evalto {} by E-Fun{{}};",env.string(),fun.string(),cl.string())
                },
            _ => panic!()
        }   
    }
}

impl Value {
    fn string(&self) -> String {
        match self {
            Value::Closure(env,f) => format!("({})[{}]",env.string(),f.string()),
            _ => "".to_string()
        }
    } 
}

impl Fun {

    pub fn new(var: Var,exp: Exp) -> Fun {
        Fun(var,box exp)
    }

    fn string(&self) -> String{
        format!("fun {} -> {}",self.var(),self.body().string()) 
    }

    fn var(&self) -> Var {
        self.0
    }

    fn body(&self) -> &Exp {
        &(*self.1)
    }
}

impl Env {

    pub fn string(&self) -> String{
        format!("{}",self.string_sub())
    }

    pub fn string_sub(&self) -> String {
        match self {
            Env::None => "".to_string(),
            Env::Env(box env,var,box val) => format!("{},{}={}",env.string_sub(),var,val.string()) 
        }
    }
}