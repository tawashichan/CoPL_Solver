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
    ClosureRec(Env,RecFun),
}

pub type Int = i64;

pub type Bool = bool;

pub type Var = String;

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
    Lt,
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
    EIfT(Env,Exp,Exp,Exp,Box<Rule>,Box<Rule>,Value),
    EIfF(Env,Exp,Exp,Exp,Box<Rule>,Box<Rule>,Value),
    EPlus(Env,Exp,Exp,Box<Rule>,Box<Rule>,Box<Rule>,Value),
    EMinus(Env,Exp,Exp,Box<Rule>,Box<Rule>,Box<Rule>,Value),
    ETimes(Env,Exp,Exp,Box<Rule>,Box<Rule>,Box<Rule>,Value),
    ELt(Env,Exp,Exp,Box<Rule>,Box<Rule>,Box<Rule>,Value),
    EVar1(Env,Var,Value),
    EVar2(Env,Var,Box<Rule>,Value),
    ELet(Env,Let,Box<Rule>,Box<Rule>,Value),
    EFun(Env,Fun,Value),
    EApp(Env,Exp,Exp,Box<Rule>,Box<Rule>,Box<Rule>,Value),
    BPlus(Int,Int,Value),
    BMinus(Int,Int),
    BTimes(Int,Int,Value),
    BLt(Int,Int,Value),
}

impl Exp {

    pub fn string(&self) -> String{
        match self {
            Exp::Int(i) => format!("{}",i),
            Exp::Op(op,box e1,box e2) => match op {
                Op::Plus => format!("{} + {}",e1.string(),e2.string()),
                Op::Minus => format!("{} - {}",e1.string(),e2.string()),
                Op::Times=> format!("{} * {}",e1.string(),e2.string()),
                Op::Lt=> format!("{} < {}",e1.string(),e2.string()),
            },
            Exp::Fun(Fun(var,box exp)) => {
                format!("fun {} -> {}",var,exp.string())
            },
            Exp::Var(var) => {
                format!("{}",var)
            },
            Exp::App(box e1,box e2) => {
                let is_e1_atomic = e1.is_atomic();
                let is_e2_atomic = e2.is_atomic();
                match (is_e1_atomic,is_e2_atomic) {
                    (true,true) => format!("{} {}",e1.string(),e2.string()),
                    (true,false) => format!("{} ({})",e1.string(),e2.string()),
                    (false,true) => format!("({}) {}",e1.string(),e2.string()),
                    (false,false) => format!("({}) ({})",e1.string(),e2.string()),
                }
            },
            Exp::Let(Let(var,box e1,box e2)) => format!("let {} = {} in {}",var,e1.string(),e2.string()),
            Exp::If(box cond,box then,box els) => format!("if {} then {} else {}",cond.string(),then.string(),els.string()),
            _ => panic!()
        }
    }

    pub fn solve(&self,env: &Env) -> Rule {  
        match self {
            Exp::Fun(fun) => Rule::EFun(env.clone(),fun.clone(),Value::Closure(env.clone(),fun.clone())),
            Exp::Let(Let(var,box e1,box e2)) => {
                let r1 = e1.solve(&env);
                let r2 = e2.solve(&Env::Env(box env.clone(),var.to_string(),box r1.value()));
                let l = Let(var.to_string(),box e1.clone(),box e2.clone()); 
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
                Op::Lt => {
                    let r1 = e1.solve(env);
                    let r2 = e2.solve(env);
                    let e1_val = r1.value();
                    let e2_val = r2.value();
                    match (e1_val,e2_val) {
                        (Value::Int(i1),Value::Int(i2)) => {
                            let r3 = Rule::BLt(i1.clone(),i2.clone(),Value::Bool(i1 < i2));
                            let val = r3.value();
                            Rule::ELt(env.clone(),e1.clone(),e2.clone(),box r1,box r2,box r3,val)
                        }
                        _ => panic!()
                    }
                }
                _ => panic!()
            }
            Exp::Var(var) => {
                match env.search(var) {
                    Some(value) => Rule::EVar1(env.clone(),var.to_string(),value.clone()),
                    None => {
                        let prev_env = &env.prev().unwrap();
                        let r = self.solve(prev_env);
                        let val = r.value();
                        Rule::EVar2(env.clone(),var.to_string(),box r,val)
                    },
                }
            }
            Exp::If(box cond,box then,box els) => {
                let r1 = cond.solve(env);
                match r1.value() {
                    Value::Bool(b) => {
                        let r2 = if b { then.solve(env) } else { els.solve(env) };
                        let val = r2.value();
                        Rule::EIfT(env.clone(),cond.clone(),then.clone(),els.clone(),box r1,box r2,val)
                    }
                    _ => panic!()
                }
            }
            _ => panic!()
        }
    }

    fn is_atomic(&self) -> bool {
        match self {
            Exp::Int(_) => true,
            Exp::Bool(_) => true,
            Exp::Var(_) => true,
            _ => false
        }
    }

}

impl Rule {

    pub fn string(&self,depth: i32) -> String {
        let space = (0..depth).fold("".to_string(),|acm,_| format!("{}      ",acm) );
        let s = match self {
            Rule::EFun(env,fun,val) =>{
                format!("{} |- {} evalto {} by E-Fun {{",env.string(),fun.string(),val.string())
            },
            Rule::ELet(env,Let(var,e1,e2),box r1,box r2,val) =>{
                format!("{} |- let {} = {} in {} evalto {} by E-Let{{\n{}\n{}",
                    env.string(),
                    var,
                    e1.string(),
                    e2.string(),
                    val.string(),
                    r1.string(depth + 1),
                    r2.string(depth + 1),
                ) 
            },    
            Rule::EInt(env,i) => {
                format!("{} |- {} evalto {} by E-Int{{",env.string(),i,i)
            },
            Rule::EVar1(env,var,value) => {
                format!("{} |- {} evalto {} by E-Var1{{",env.string(),var,value.string())
            },
            Rule::EVar2(env,var,box rule,value) => {
                format!("{} |- {} evalto {} by E-Var2 {{\n{}",env.string(),var,value.string(),rule.string(depth + 1))
            },
            Rule::EApp(env,e1,e2,box r1,box r2,box r3,value) => {
                match (e1.is_atomic(),e2.is_atomic()) {
                    (true,true) => format!("{} |- {} {} evalto {} by E-App{{\n{}\n{}\n{}",
                        env.string(),
                        e1.string(),
                        e2.string(),
                        value.string(),
                        r1.string(depth + 1),
                        r2.string(depth + 1),
                        r3.string(depth + 1),
                    ),
                    (true,false) => format!("{} |- {} ({}) evalto {} by E-App{{\n{}\n{}\n{}",
                        env.string(),
                        e1.string(),
                        e2.string(),
                        value.string(),
                        r1.string(depth + 1),
                        r2.string(depth + 1),
                        r3.string(depth + 1),
                    ),
                    (false,true) => format!("{} |- ({}) {} evalto {} by E-App{{\n{}\n{}\n{}",
                        env.string(),
                        e1.string(),
                        e2.string(),
                        value.string(),
                        r1.string(depth + 1),
                        r2.string(depth + 1),
                        r3.string(depth + 1),
                    ),
                    (false,false) => format!("{} |- ({}) ({}) evalto {} by E-App{{\n{}\n{}\n{}",
                        env.string(),
                        e1.string(),
                        e2.string(),
                        value.string(),
                        r1.string(depth + 1),
                        r2.string(depth + 1),
                        r3.string(depth + 1),
                    ),
                }
            },
            Rule::EPlus(env,e1,e2,box r1,box r2,box r3,val) => {
                format!("{} |- {} + {} evalto {} by E-Plus{{\n{}\n{}\n{}",
                    env.string(),
                    e1.string(),
                    e2.string(),
                    val.string(),
                    r1.string(depth + 1),
                    r2.string(depth + 1),
                    r3.string(depth + 1)
                )
            },
            Rule::ETimes(env,e1,e2,box r1,box r2,box r3,val) => {
                format!("{} |- {} * {} evalto {} by E-Times{{\n{}\n{}\n{}",
                    env.string(),
                    e1.string(),
                    e2.string(),
                    val.string(),
                    r1.string(depth + 1),
                    r2.string(depth + 1),
                    r3.string(depth + 1)
                )
            },
            Rule::BPlus(i1,i2,val) => {
                format!("{} plus {} is {} by B-Plus{{",
                    i1,
                    i2,
                    val.string(),
                )
            },
            Rule::BTimes(i1,i2,val) => {
                format!("{} times {} is {} by B-Times{{",
                    i1,
                    i2,
                    val.string(),
                )
            },
            Rule::EIfT(env,e1,e2,e3,r1,r2,value) => {
                format!("{} |- if {} then {} else {} evalto {} by E-IfT {{\n{}\n{}",
                    env.string(),
                    e1.string(),
                    e2.string(),
                    e3.string(),
                    value.string(),
                    r1.string(depth + 1),
                    r2.string(depth + 1),
                )
            },
            Rule::EIfF(env,e1,e2,e3,r1,r2,value) => {
                format!("{} |- if {} then {} else {} evalto {} by E-IfT {{\n{}\n{}",
                    env.string(),
                    e1.string(),
                    e2.string(),
                    e3.string(),
                    value.string(),
                    r1.string(depth + 1),
                    r2.string(depth + 1),
                )
            },
            Rule::ELt(env,e1,e2,r1,r2,r3,value) => {
                format!("{} |- {} < {} evalto {} by E-Lt {{\n{}\n{}\n{}",
                    env.string(),
                    e1.string(),
                    e2.string(),
                    value.string(),
                    r1.string(depth + 1),
                    r2.string(depth + 1),
                    r3.string(depth + 1)
                )
            },
            Rule::BLt(i1,i2,value) => {
                format!("{} less than {} is {} by B-Lt {{",
                    i1,
                    i2,
                    value.string()
                )
            }
            _ => panic!("{:?}",self)
        };
        format!("{}{}\n{}}};",space,s,space)
    }

    pub fn value (&self) -> Value {
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
            Rule::EIfT(_,_,_,_,_,_,v) => v.clone(),
            Rule::ELt(_,_,_,_,_,_,v) => v.clone(),
            Rule::BLt(_,_,v) => v.clone(),
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
    pub fn string(&self) -> String {
        match self {
            Value::Closure(env,f) => format!("({})[{}]",env.string(),f.string()),
            Value::Int(i) => format!("{}",i),
            Value::Bool(b) => format!("{}",b),
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
        self.0.clone()
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

