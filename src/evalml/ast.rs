#[derive(Debug,Clone,PartialEq)]
pub struct Number(pub i64);

#[derive(Debug,Clone,PartialEq)]
pub enum Expr {
    Number(Number),
    Plus(Box<Expr>,Box<Expr>),
    Mul(Box<Expr>,Box<Expr>)
}

#[derive(Debug,Clone,PartialEq)]
pub enum ApplyRule{
    PZero(Number),
    PSucc(Number,Number,Box<ApplyRule>),
    EConst(Number),
    EPLUS(Expr,Expr,Box<ApplyRule>,Box<ApplyRule>,Box<ApplyRule>),
    ETimes(Expr,Expr,Box<ApplyRule>,Box<ApplyRule>,Box<ApplyRule>),
    TZero(Number),
    TSucc(Number,Number,Box<ApplyRule>,Box<ApplyRule>),
} 

impl Number {

    pub fn string(&self) -> String {
        match self.0 {
            0 =>  "Z".to_string(),
            n => format!("S({})",Number(n - 1).string())
        }
    }

    fn is_succ(&self,num: &Number) -> bool {
        self.0 + 1 == num.0
    } 

    fn plus(&self,num: &Number) -> Number{
        Number(self.0 + num.0)
    }

    fn mul(&self,num: &Number) -> Number{
        Number(self.0 * num.0)
    }

    fn value(&self) -> i64{
        self.0
    }
}

impl Expr {

    pub fn possible_rules(&self) -> Vec<ApplyRule> {
        match self {
            Expr::Number(n) => vec![ApplyRule::EConst(n.clone())],
            _ => vec![ApplyRule::EConst(Number(0))]
        }
    }

    pub fn string(&self) -> String{
        match self {
            Expr::Number(n) => n.string(),
            Expr::Plus(box e1,box e2) => format!("{} + {}",e1.string(),e2.string()),
            Expr::Mul(box e1,box e2) => format!("{} * {}",e1.string(),e2.string()),
        }
    }

    fn value(&self) -> Number {
        match self {
            Expr::Number(n) => n.clone(),
            Expr::Plus(e1,e2) => e1.value().plus(&e2.value()),
            Expr::Mul(e1,e2) => e1.value().mul(&e2.value()),
            _ => panic!()
        }
    }

    fn plus(&self,num: &Expr) -> Number {
        self.value().plus(&num.value())
    }

    fn mul(&self,num: &Expr) -> Number {
        self.value().mul(&num.value())
    }

    pub fn get_rule(&self) -> ApplyRule {
        match self {
            Expr::Number(n) => ApplyRule::EConst(n.clone()),
            Expr::Plus(box e1,box e2) => {
                let r1 = e1.get_rule();
                let r2 = e2.get_rule();
                let e1_val = e1.value();
                let e2_val = e2.value();
                ApplyRule::EPLUS(e1.clone(),e2.clone(),box r1,box r2,box ApplyRule::new_p_rule(&e1_val,&e2_val))
            }
            Expr::Mul(box e1,box e2) => {
                let r1 = e1.get_rule();
                let r2 = e2.get_rule();
                let e1_val = e1.value();
                let e2_val = e2.value();
                ApplyRule::ETimes(e1.clone(),e2.clone(),box r1,box r2,box ApplyRule::new_t_rule(&e1_val,&e2_val))
            }
            _ => panic!()
        }
    }

}

impl ApplyRule {

   pub fn string(&self) -> String{
       match self {
           ApplyRule::EConst(n) => format!("{} evalto {} by E-Const{{}};",n.string(),n.string()),
           ApplyRule::PZero(n) => format!("Z plus {} is {} by P-Zero{{}};",n.string(),n.string()),
           ApplyRule::PSucc(n1,n2,box rule) => format!("{} plus {} is {} by P-Succ{{ {} }};",
                n1.string(),
                n2.string(),
                n1.plus(n2).string(),
                rule.string()
           ),
           ApplyRule::EPLUS(e1,e2,box r1,box r2, box r3) => format!("{} + {} evalto {} by E-Plus {{ {} {} {} }};",
                e1.string(),
                e2.string(),
                e1.plus(e2).string(),
                r1.string(),
                r2.string(),
                r3.string()
           ),
            ApplyRule::ETimes(e1,e2,box r1,box r2, box r3) => format!("{} * {} evalto {} by E-Times {{ {} {} {} }};",
                e1.string(),
                e2.string(),
                e1.mul(e2).string(),
                r1.string(),
                r2.string(),
                r3.string()
           ),
           ApplyRule::TZero(n) => format!("Z times {} is Z by T-Zero{{}};",n.string()),
           ApplyRule::TSucc(n1,n2,box r1,box r2) => format!("{} times {} is {} by T-Succ{{ {} {} }};",
                n1.string(),
                n2.string(),
                n1.mul(n2).string(),
                r1.string(),
                r2.string()
            ),
           _ => format!("")
       }
   }

   fn new_p_rule(n1: &Number,n2: &Number) -> ApplyRule {
       if n1.value() == 0 {
           ApplyRule::PZero(n2.clone())
       } else {
           ApplyRule::PSucc(n1.clone(),n2.clone(),box ApplyRule::new_p_rule(&Number(n1.value() - 1),n2))
       }
   }

   fn new_t_rule(n1: &Number,n2: &Number) -> ApplyRule {
       if n1.value() == 0 {
           ApplyRule::TZero(n2.clone())
       } else {
           ApplyRule::TSucc(n1.clone(),n2.clone(),box ApplyRule::new_t_rule(&Number(n1.value() - 1), n2),box ApplyRule::new_p_rule(n2,&Number(n1.value() - 1 * n2.value())))
       }
   }


}