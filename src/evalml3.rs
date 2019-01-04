#[derive(Debug,Clone)]
pub enum Exp {
    Int(Int),
    Bool(Bool),
    Var(Var),
    Op(Op,Box<Exp>,Box<Exp>),
    If(Box<Exp>,Box<Exp>,Box<Exp>),
    Let(Let),
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
pub struct Let(Var,Box<Exp>,Box<Exp>);

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
    EPlus(Env,Exp,Exp,Box<Rule>,Box<Rule>,Box<Rule>,Value),
    EMinus(Env,Exp,Exp,Box<Rule>,Box<Rule>,Box<Rule>,Value),
    ETimes(Env,Exp,Exp,Box<Rule>,Box<Rule>,Box<Rule>,Value),
    ELt(Env,Exp,Exp,Box<Rule>,Box<Rule>,Box<Rule>),
    EVar1(Env,Var,Value),
    EVar2(Env,Var,Box<Rule>,Value),
    ELet(Env,Let,Box<Rule>,Box<Rule>,Value),
    EFun(Env,Fun,Value),
    EApp(Env,Exp,Exp,Box<Rule>,Box<Rule>,Box<Rule>,Value),
    BPlus(Int,Int,Value),
    BMinus(Int,Int),
    BTimes(Int,Int,Value),
    BLt(Int,Int),
}

impl Exp {

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
            Exp::App(box e1,box e2) => {
                format!("{} {}",e1.string(),e2.string())
            },
            Exp::Let(Let(var,box e1,box e2)) => format!("let {} = {} in {}",var,e1.string(),e2.string()),
            _ => panic!()
        }
    }

    pub fn solve(&self,env: &Env) -> Rule {
        match self {
            Exp::Fun(fun) => Rule::EFun(env.clone(),fun.clone(),Value::Closure(env.clone(),fun.clone())),
            Exp::Let(Let(var,box e1,box e2)) => {
                let r1 = e1.solve(&env);
                let r2 = e2.solve(&Env::Env(box env.clone(),var,box r1.value()));
                let l = Let(var,box e1.clone(),box e2.clone()); 
                let e2_val = r2.value();
                Rule::ELet(env.clone(),l,box r1,box r2,e2_val)
            },
            Exp::Int(i) => Rule::EInt(env.clone(),*i),
            Exp::App(box e1,box e2) => {
                let r1 = e1.solve(&env);
                match r1.value(){
                    Value::Closure(en,fun) => {
                        let r2 = e2.solve(&env);
                        let env2 = &Env::Env(box en.clone(),fun.var(),box r2.value());
                        let r3 = fun.body().clone().solve(&env2);
                        let e0_val = r3.value();
                        Rule::EApp(env.clone(),e1.clone(),e2.clone(),box r1,box r2,box r3,e0_val)
                    },
                    _ => panic!()
                }
            }
            Exp::Op(op,box e1,box e2) => match op {
                Op::Plus => {
                    let r1 = e1.solve(env);
                    let r2 = e2.solve(env);
                    let e1_val = r1.value();
                    let e2_val = r2.value();
                    match (e1_val,e2_val) {
                        (Value::Int(i1),Value::Int(i2)) => {
                            let r3 = Rule::BPlus(i1,i2,Value::Int(i1 + i2));
                            let val = r3.value();
                            Rule::EPlus(env.clone(),e1.clone(),e2.clone(),box r1,box r2,box r3,val)
                        }
                        _ => panic!()
                    }
                }
                Op::Times => {
                    let r1 = e1.solve(env);
                    let r2 = e2.solve(env);
                    let e1_val = r1.value();
                    let e2_val = r2.value();
                    match (e1_val,e2_val) {
                        (Value::Int(i1),Value::Int(i2)) => {
                            let r3 = Rule::BTimes(i1,i2,Value::Int(i1 * i2));
                            let val = r3.value();
                            Rule::ETimes(env.clone(),e1.clone(),e2.clone(),box r1,box r2,box r3,val)
                        }
                        _ => panic!()
                    }
                }
                _ => panic!()
            }
            Exp::Var(var) => {
                match env.search(var) {
                    Some(value) => Rule::EVar1(env.clone(),var,value.clone()),
                    None => {
                        let prev_env = &env.prev().unwrap();
                        let r = self.solve(prev_env);
                        let val = r.value();
                        Rule::EVar2(env.clone(),var,box r,val)
                    },
                }
            }
            _ => panic!()
        }
    }

}

impl Rule {

    pub fn string(&self) -> String {
        match self {
            Rule::EFun(env,fun,val) =>{
                format!("{} |- {} evalto {} by E-Fun{{}};",env.string(),fun.string(),val.string())
            },
            Rule::ELet(env,Let(var,e1,e2),box r1,box r2,val) =>{
                format!("{} |- let {} = {} in {} evalto {} by E-Let{{ {} {} }};",
                    env.string(),
                    var,
                    e1.string(),
                    e2.string(),
                    val.string(),
                    r1.string(),
                    r2.string(),
                ) 
            },    
            Rule::EInt(env,i) => {
                format!("{} |- {} evalto {} by E-Int{{}};",env.string(),i,i)
            },
            Rule::EVar1(env,var,value) => {
                format!("{} |- {} evalto {} by E-Var1{{}};",env.string(),var,value.string())
            },
            Rule::EApp(env,e1,e2,box r1,box r2,box r3,value) => {
                format!("{} |- {} {} evalto {} by E-App{{ {} {} {} }};",
                    env.string(),
                    e1.string(),
                    e2.string(),
                    value.string(),
                    r1.string(),
                    r2.string(),
                    r3.string(),
                )
            },
            Rule::EPlus(env,e1,e2,box r1,box r2,box r3,val) => {
                format!("{} |- {} + {} evalto {} by E-Plus{{ {} {} {}}};",
                    env.string(),
                    e1.string(),
                    e2.string(),
                    val.string(),
                    r1.string(),
                    r2.string(),
                    r3.string()
                )
            },
            Rule::ETimes(env,e1,e2,box r1,box r2,box r3,val) => {
                format!("{} |- {} * {} evalto {} by E-Times{{ {} {} {}}};",
                    env.string(),
                    e1.string(),
                    e2.string(),
                    val.string(),
                    r1.string(),
                    r2.string(),
                    r3.string()
                )
            },
            Rule::BPlus(i1,i2,val) => {
                format!("{} plus {} is {} by B-Plus{{}};",
                    i1,
                    i2,
                    val.string(),
                )
            },
            Rule::BTimes(i1,i2,val) => {
                format!("{} times {} is {} by B-Times{{}};",
                    i1,
                    i2,
                    val.string(),
                )
            },
            _ => panic!()
        }   
    }

    fn value (&self) -> Value {
        match self {
            Rule::EInt(env,i) => Value::Int(*i),
            Rule::ELet(_,_,_,_,v) => v.clone(),
            Rule::EFun(_,_,v) => v.clone(),
            Rule::EVar1(_,_,v) => v.clone(),
            Rule::EVar2(_,_,_,v) => v.clone(),
            Rule::EPlus(_,_,_,_,_,_,v) => v.clone(),
            Rule::ETimes(_,_,_,_,_,_,v) => v.clone(),
            Rule::BTimes(_,_,v) => v.clone(),
            Rule::BPlus(_,_,v) => v.clone(),
            Rule::EApp(_,_,_,_,_,_,v) => v.clone(),
            _ => panic!()

        }
    }
}

impl Let {

    pub fn new(var: Var,e1: Exp,e2: Exp) -> Let {
        Let(var,box e1,box e2)
    }
}

impl Value {
    fn string(&self) -> String {
        match self {
            Value::Closure(env,f) => format!("({})[{}]",env.string(),f.string()),
            Value::Int(i) => format!("{}",i),
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
            Env::Env(box Env::None,var,box val) => format!("{}={}",var,val.string()), 
            Env::Env(box env,var,box val) => format!("{},{}={}",env.string_sub(),var,val.string()) 
        }
    }

    fn search(&self,var: &Var) -> Option<Value> {
        match self {
            Env::None => None,
            Env::Env(box prev,va,box value) => if var == va {
                Some(value.clone())
            } else {
                None
            }
        }
    } 

    fn prev(&self) -> Option<&Env> {
        match self {
            Env::None => None,
            Env::Env(box env,_,_) => Some(env)
        }
    }
}