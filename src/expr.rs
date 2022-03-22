use crate::eval::Environment;

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

impl<'a> IntoIterator for &'a List {
    type Item = &'a Expression;

    type IntoIter = ListVisitor<'a>;

    fn into_iter(self) -> Self::IntoIter {
        ListVisitor { list: self }
    }
}

pub struct ListVisitor<'a> {
    list: &'a List,
}

impl<'a> Iterator for ListVisitor<'a> {
    type Item = &'a Expression;

    fn next(&mut self) -> Option<Self::Item> {
        match self.list {
            List::Cons(expr, tail) => {
                self.list = tail;
                Some(expr)
            }
            List::Nil => None,
        }
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

#[derive(Debug, Clone, PartialEq)]
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

pub const BUILTIN_NAME_ALIST: [(&str, Builtin); 20] = [
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

#[derive(Debug, Clone, PartialEq)]
pub enum TestMonop {
    Number,
    List,
    Nil,
}

#[derive(Debug, Clone, PartialEq)]
pub enum NumBinop {
    ArBinop(ArBinop),
    OrdBinop(OrdBinop),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArBinop {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, PartialEq)]
pub enum OrdBinop {
    Eql,
    Lt,
}

#[derive(Debug, Clone, PartialEq)]
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
