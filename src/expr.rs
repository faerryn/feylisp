#[derive(Debug, Clone)]
pub enum Expression {
    Number(i32),
    Symbol(String),
    List(List),
    Bool(bool),
    Builtin(Builtin),
    Closure(Closure),
    Macro(Closure),
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Number(number) => write!(f, "{}", number),
            Expression::Symbol(symbol) => write!(f, "{}", symbol),
            Expression::List(list) => write!(f, "({})", list),
            Expression::Bool(b) => write!(f, "{}", if *b { "#t" } else { "#f" }),
            Expression::Builtin(builtin) => write!(f, "{}", builtin),
            Expression::Closure(closure) => write!(f, "{}", closure),
            Expression::Macro(closure) => write!(f, "(macro {})", closure),
        }
    }
}

#[derive(Debug, Clone)]
pub enum List {
    Cons(Box<Expression>, Box<List>),
    Nil,
}

#[derive(Debug)]
pub struct ListError;

impl std::fmt::Display for ListError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl List {
    #[must_use]
    pub fn cons(expr: Expression, list: List) -> List {
        List::Cons(Box::new(expr), Box::new(list))
    }
    #[must_use]
    pub fn decons(self) -> Option<(Expression, List)> {
        if let List::Cons(head, tail) = self {
            Some((*head, *tail))
        } else {
            None
        }
    }
}

impl From<Vec<Expression>> for List {
    fn from(vec: Vec<Expression>) -> Self {
        let mut result = List::Nil;

        for expr in vec.into_iter().rev() {
            result = List::Cons(Box::new(expr), Box::new(result));
        }

        result
    }
}

impl std::fmt::Display for List {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            List::Cons(head, tail) => {
                write!(f, "{}", head)?;
                match &**tail {
                    List::Cons(_, _) => write!(f, " {}", tail),
                    List::Nil => Ok(()),
                }
            }
            List::Nil => Ok(()),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Builtin {
    Quote,
    Lambda,
    Macro,
    If,
    TestMonop(TestMonop),
    NumBinop(NumBinop),
    ListMonop(ListMonop),
    Cons,
    List,
    Let,
    Eval,
    Define,
}

const BUILTIN_NAME_ALIST: [(&str, Builtin); 20] = [
    ("quote", Builtin::Quote),
    ("lambda", Builtin::Lambda),
    ("macro", Builtin::Macro),
    ("if", Builtin::If),
    ("number?", Builtin::TestMonop(TestMonop::Number)),
    ("list?", Builtin::TestMonop(TestMonop::List)),
    ("nil?", Builtin::TestMonop(TestMonop::Nil)),
    ("+", Builtin::NumBinop(NumBinop::ArBinop(ArBinop::Add))),
    ("-", Builtin::NumBinop(NumBinop::ArBinop(ArBinop::Sub))),
    ("*", Builtin::NumBinop(NumBinop::ArBinop(ArBinop::Mul))),
    ("/", Builtin::NumBinop(NumBinop::ArBinop(ArBinop::Div))),
    ("=", Builtin::NumBinop(NumBinop::OrdBinop(OrdBinop::Eql))),
    ("<", Builtin::NumBinop(NumBinop::OrdBinop(OrdBinop::Lt))),
    ("head", Builtin::ListMonop(ListMonop::Head)),
    ("tail", Builtin::ListMonop(ListMonop::Tail)),
    ("cons", Builtin::Cons),
    ("list", Builtin::List),
    ("let", Builtin::Let),
    ("eval", Builtin::Eval),
    ("define", Builtin::Define),
];

impl std::fmt::Display for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (name, value) in BUILTIN_NAME_ALIST {
            if value == *self {
                write!(f, "{}", name)?;
                return Ok(());
            }
        }

        Err(std::fmt::Error)
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TestMonop {
    Number,
    List,
    Nil,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum NumBinop {
    ArBinop(ArBinop),
    OrdBinop(OrdBinop),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ArBinop {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum OrdBinop {
    Eql,
    Lt,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ListMonop {
    Head,
    Tail,
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub params: List,
    pub body: Box<Expression>,
    pub env: Environment,
}

impl std::fmt::Display for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(lambda ({}) {}) [{}]", self.params, self.body, self.env)
    }
}

#[derive(Debug, Clone)]
pub enum Environment {
    Cons(String, Box<Expression>, Box<Environment>),
    Nil,
}

impl Environment {
    #[must_use]
    pub fn get(&self, ident: &str) -> Option<Expression> {
        match self {
            Environment::Cons(name, value, _) if name == ident => Some(*value.clone()),
            Environment::Cons(_, _, parent) => parent.get(ident),
            Environment::Nil => None,
        }
    }

    #[must_use]
    pub fn cons(name: String, value: Expression, env: Environment) -> Environment {
        Environment::Cons(name, Box::new(value), Box::new(env))
    }
}

impl Default for Environment {
    fn default() -> Self {
        let mut result = Environment::Nil;
        for (name, value) in BUILTIN_NAME_ALIST {
            result = Environment::Cons(
                name.to_owned(),
                Box::new(Expression::Builtin(value)),
                Box::new(result),
            );
        }
        result
    }
}

impl std::fmt::Display for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Environment::Cons(name, value, parent) => {
                write!(f, "{}: {}", name, value)?;
                match **parent {
                    Environment::Cons(_, _, _) => write!(f, ", {}", parent),
                    Environment::Nil => Ok(()),
                }
            }
            Environment::Nil => Ok(()),
        }
    }
}
