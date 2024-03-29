use crate::eval::Environment;
use std::rc::Rc;

pub enum Expression {
    Number(i32),
    Symbol(Rc<String>),
    List(Rc<List>),
    Bool(bool),
    Callable(Rc<Callable>),
}

pub enum Callable {
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
            Expression::Callable(callable) => match callable.as_ref() {
                Callable::Builtin(builtin) => write!(f, "{}", builtin),
                Callable::Closure(closure) => write!(f, "(lambda {})", closure),
                Callable::Macro(closure) => write!(f, "(macro {})", closure),
            },
        }
    }
}

impl std::fmt::Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Expression::Number(lhs), Expression::Number(rhs)) => lhs == rhs,
            (Expression::Symbol(lhs), Expression::Symbol(rhs)) => lhs == rhs,
            (Expression::List(lhs), Expression::List(rhs)) => lhs == rhs,
            (Expression::Bool(lhs), Expression::Bool(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

#[derive(PartialEq)]
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
        if let List::Pair(expr, tail) = Rc::clone(&self.list).as_ref() {
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
                for expr in tail.as_ref() {
                    write!(f, " {}", expr)?;
                }
                Ok(())
            }
            List::Nil => Ok(()),
        }
    }
}

pub enum Builtin {
    Quote,
    Enclosure(Enclosure),
    If,
    Type,
    Equal,
    NumBinop(NumBinop),
    ListMonop(ListMonop),
    Pair,
    Let,
    Eval,
    Define,
    Apply,
}

impl std::fmt::Display for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Builtin::Quote => "quote",
                Builtin::Enclosure(enclosure) => match enclosure {
                    Enclosure::Lambda => "lambda",
                    Enclosure::Macro => "macro",
                },
                Builtin::If => "if",
                Builtin::Type => "type",
                Builtin::Equal => "builtin=",
                Builtin::NumBinop(op) => match op {
                    NumBinop::ArBinop(op) => match op {
                        ArBinop::Add => "builtin+",
                        ArBinop::Sub => "builtin-",
                        ArBinop::Mul => "builtin*",
                        ArBinop::Div => "builtin/",
                    },
                    NumBinop::Lt => "builtin<",
                },
                Builtin::ListMonop(op) => match op {
                    ListMonop::Head => "head",
                    ListMonop::Tail => "tail",
                },
                Builtin::Pair => "pair",
                Builtin::Let => "let",
                Builtin::Eval => "eval",
                Builtin::Define => "define",
                Builtin::Apply => "apply",
            }
        )
    }
}

pub enum NumBinop {
    ArBinop(ArBinop),
    Lt,
}

pub enum ArBinop {
    Add,
    Sub,
    Mul,
    Div,
}

pub enum ListMonop {
    Head,
    Tail,
}

pub enum Enclosure {
    Lambda,
    Macro,
}

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
