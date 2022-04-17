use crate::eval::Environment;
use std::rc::Rc;

#[derive(Debug)]
pub enum Expression {
    Number(i32),
    Symbol(Rc<String>),
    List(Rc<List>),
    Bool(bool),
    Builtin(Builtin),
    Closure(Closure),
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Number(number) => write!(f, "{}", number),
            Expression::Symbol(symbol) => write!(f, "{}", symbol),
            Expression::List(list) => write!(f, "({})", list),
            Expression::Bool(b) => write!(f, "{}", if *b { "#t" } else { "#f" }),
            Expression::Builtin(builtin) => write!(f, "{}", builtin),
            Expression::Closure(closure) => write!(f, "(lambda {})", closure),
        }
    }
}

#[derive(Debug)]
pub enum List {
    Pair(Rc<Expression>, Rc<List>),
    Nil,
}

impl FromIterator<Expression> for List {
    fn from_iter<T: IntoIterator<Item = Expression>>(iter: T) -> Self {
        let mut iter = iter.into_iter();

        if let Some(expr) = iter.next() {
            List::Pair(Rc::new(expr), Rc::new(iter.collect()))
        } else {
            List::Nil
        }
    }
}

impl IntoIterator for List {
    type Item = Rc<Expression>;

    type IntoIter = ListVisitor;

    fn into_iter(self) -> Self::IntoIter {
        ListVisitor {
            list: Rc::new(self),
        }
    }
}

impl IntoIterator for &List {
    type Item = Rc<Expression>;

    type IntoIter = ListVisitor;

    fn into_iter(self) -> Self::IntoIter {
        ListVisitor {
            list: Rc::new(match self {
                List::Pair(head, tail) => List::Pair(Rc::clone(head), Rc::clone(tail)),
                List::Nil => List::Nil,
            }),
        }
    }
}

pub struct ListVisitor {
    list: Rc<List>,
}

impl Iterator for ListVisitor {
    type Item = Rc<Expression>;

    fn next(&mut self) -> Option<Self::Item> {
        if let List::Pair(expr, tail) = &*Rc::clone(&self.list) {
            self.list = Rc::clone(tail);
            Some(Rc::clone(expr))
        } else {
            None
        }
    }
}

impl std::fmt::Display for List {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            List::Pair(first, tail) => {
                write!(f, "{}", first)?;
                for expr in &**tail {
                    write!(f, " {}", expr)?;
                }
                Ok(())
            }
            List::Nil => Ok(()),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Builtin {
    Quote,
    Lambda,
    If,
    TestMonop(TestMonop),
    NumBinop(NumBinop),
    ListMonop(ListMonop),
    Pair,
    Let,
    Eval,
    Define,
}

pub const BUILTIN_NAME_ALIST: [(&str, Builtin); 18] = [
    ("quote", Builtin::Quote),
    ("lambda", Builtin::Lambda),
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
    ("pair", Builtin::Pair),
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

#[derive(Debug, PartialEq)]
pub enum TestMonop {
    Number,
    List,
    Nil,
}

#[derive(Debug, PartialEq)]
pub enum NumBinop {
    ArBinop(ArBinop),
    OrdBinop(OrdBinop),
}

#[derive(Debug, PartialEq)]
pub enum ArBinop {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, PartialEq)]
pub enum OrdBinop {
    Eql,
    Lt,
}

#[derive(Debug, PartialEq)]
pub enum ListMonop {
    Head,
    Tail,
}

#[derive(Debug)]
pub struct Closure {
    pub params: Rc<List>,
    pub body: Rc<Expression>,
    pub env: Rc<Environment>,
}

impl std::fmt::Display for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}) {}", self.params, self.body)
    }
}
