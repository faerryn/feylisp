fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut env = Environment::Nil;
    for (key, val) in [
        ("quote", Builtin::Quote),
        ("zero?", Builtin::TestMonop(TestMonop::Zero)),
        ("nil?", Builtin::TestMonop(TestMonop::Nil)),
        ("+", Builtin::ArithBinop(ArithBinop::Add)),
        ("-", Builtin::ArithBinop(ArithBinop::Sub)),
        ("*", Builtin::ArithBinop(ArithBinop::Mul)),
        ("/", Builtin::ArithBinop(ArithBinop::Div)),
        ("hd", Builtin::ListMonop(ListMonop::Head)),
        ("tl", Builtin::ListMonop(ListMonop::Tail)),
    ] {
        env = Environment::Cons(key.to_owned(), Expression::Builtin(val), Box::new(env));
    }

    for arg in std::env::args().skip(1) {
        let src = parse(lex(&std::fs::read_to_string(arg)?)?)?;
        for expr in src {
            print!("{} -> ", expr);
            print!("{}", eval(expr, &env)?);
            println!();
        }
    }

    Ok(())
}

#[derive(Debug)]
enum Lexeme {
    Number(i32),
    Symbol(String),
    Open,
    Close,
}

fn lex(src: &str) -> Result<Vec<Lexeme>, std::num::ParseIntError> {
    let mut result = vec![];

    enum State {
        Start,
        Sign,
        Number,
        Symbol,
    }

    let mut state = State::Start;

    let mut prev = 0;

    for (curr, ch) in src.chars().enumerate() {
        match state {
            State::Start => {
                prev = curr;
                match ch {
                    ' ' | '\t' | '\n' | '\r' => {
                        state = State::Start;
                    }
                    '(' => {
                        result.push(Lexeme::Open);
                        state = State::Start;
                    }
                    ')' => {
                        result.push(Lexeme::Close);
                        state = State::Start;
                    }
                    '+' | '-' => {
                        state = State::Sign;
                    }
                    '0'..='9' => {
                        state = State::Number;
                    }
                    _ => {
                        state = State::Symbol;
                    }
                }
            }
            State::Sign => match ch {
                ' ' | '\t' | '\n' | '\r' => {
                    result.push(Lexeme::Symbol(src[prev..curr].to_owned()));
                    state = State::Start;
                }
                '(' => {
                    result.push(Lexeme::Symbol(src[prev..curr].to_owned()));
                    result.push(Lexeme::Open);
                    state = State::Start;
                }
                ')' => {
                    result.push(Lexeme::Symbol(src[prev..curr].to_owned()));
                    result.push(Lexeme::Close);
                    state = State::Start;
                }
                '0'..='9' => {
                    state = State::Number;
                }
                _ => {
                    state = State::Symbol;
                }
            },
            State::Number => match ch {
                ' ' | '\t' | '\n' | '\r' => {
                    result.push(Lexeme::Number(src[prev..curr].parse::<_>()?));
                    state = State::Start;
                }
                '(' => {
                    result.push(Lexeme::Number(src[prev..curr].parse::<_>()?));
                    result.push(Lexeme::Open);
                    state = State::Start;
                }
                ')' => {
                    result.push(Lexeme::Number(src[prev..curr].parse::<_>()?));
                    result.push(Lexeme::Close);
                    state = State::Start;
                }
                '0'..='9' => {
                    state = State::Number;
                }
                _ => {
                    state = State::Symbol;
                }
            },
            State::Symbol => match ch {
                ' ' | '\t' | '\n' | '\r' => {
                    result.push(Lexeme::Symbol(src[prev..curr].to_owned()));
                    state = State::Start;
                }
                '(' => {
                    result.push(Lexeme::Symbol(src[prev..curr].to_owned()));
                    result.push(Lexeme::Open);
                    state = State::Start;
                }
                ')' => {
                    result.push(Lexeme::Symbol(src[prev..curr].to_owned()));
                    result.push(Lexeme::Close);
                    state = State::Start;
                }
                _ => {
                    state = State::Symbol;
                }
            },
        }
    }

    match state {
        State::Start => {}
        State::Sign => {
            result.push(Lexeme::Symbol(src[prev..].to_owned()));
        }
        State::Number => {
            result.push(Lexeme::Number(src[prev..].parse::<_>()?));
        }
        State::Symbol => {
            result.push(Lexeme::Symbol(src[prev..].to_owned()));
        }
    }

    Ok(result)
}

#[derive(Clone, Debug)]
enum Expression {
    Number(i32),
    Symbol(String),
    List(List),
    Bool(bool),
    Builtin(Builtin),
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Number(number) => write!(f, "{}", number),
            Expression::Symbol(symbol) => write!(f, "{}", symbol),
            Expression::List(list) => write!(f, "({})", list),
            Expression::Bool(b) => write!(f, "{}", if *b { "#t" } else { "#f" }),
            Expression::Builtin(_) => todo!(),
        }
    }
}

#[derive(Clone, Debug)]
struct Cons {
    head: Expression,
    tail: Expression,
}

#[derive(Clone, Debug)]
enum List {
    Cons(Box<Cons>),
    Nil,
}

#[derive(Debug)]
struct ListError;

impl std::fmt::Display for ListError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "list error")
    }
}

impl std::error::Error for ListError {}

impl List {
    fn head_tail(list: List) -> Result<(Expression, Expression), ListError> {
        match list {
            List::Cons(cons) => Ok((cons.head, cons.tail)),
            List::Nil => Err(ListError),
        }
    }
    fn head_taillist(list: List) -> Result<(Expression, List), ListError> {
        match List::head_tail(list)? {
            (car, Expression::List(cdr)) => Ok((car, cdr)),
            _ => Err(ListError),
        }
    }
    fn single(list: List) -> Result<Expression, ListError> {
        match List::head_taillist(list)? {
            (car, List::Nil) => Ok(car),
            _ => Err(ListError),
        }
    }
}

impl From<Vec<Expression>> for List {
    fn from(vec: Vec<Expression>) -> Self {
        let mut result = List::Nil;

        for expr in vec.into_iter().rev() {
            result = List::Cons(Box::new(Cons {
                head: expr,
                tail: Expression::List(result),
            }));
        }

        result
    }
}

impl std::fmt::Display for List {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            List::Cons(cons) => {
                write!(f, "{}", cons.head)?;
                match &cons.tail {
                    Expression::List(tail) => match tail {
                        List::Cons(_) => write!(f, " {}", tail),
                        List::Nil => Ok(()),
                    },
                    _ => write!(f, " . {}", cons.tail),
                }
            }
            List::Nil => Ok(()),
        }
    }
}

#[derive(Clone, Debug)]
enum Builtin {
    Quote,
    TestMonop(TestMonop),
    ArithBinop(ArithBinop),
    ListMonop(ListMonop),
}

#[derive(Clone, Debug)]
enum TestMonop {
    Zero,
    Nil,
}

#[derive(Clone, Debug)]
enum ArithBinop {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Clone, Debug)]
enum ListMonop {
    Head,
    Tail,
}

#[derive(Debug)]
struct ParseError;

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "parse error")
    }
}

impl std::error::Error for ParseError {}

fn parse(src: Vec<Lexeme>) -> Result<Vec<Expression>, ParseError> {
    let mut stack = vec![vec![]];

    for lexeme in src {
        match lexeme {
            Lexeme::Number(number) => {
                stack
                    .last_mut()
                    .ok_or(ParseError)?
                    .push(Expression::Number(number));
            }
            Lexeme::Symbol(symbol) => {
                stack
                    .last_mut()
                    .ok_or(ParseError)?
                    .push(match symbol.as_str() {
                        "#t" => Expression::Bool(true),
                        "#f" => Expression::Bool(false),
                        _ => Expression::Symbol(symbol),
                    });
            }
            Lexeme::Open => {
                stack.push(vec![]);
            }
            Lexeme::Close => {
                let list = List::from(stack.pop().ok_or(ParseError)?);
                stack
                    .last_mut()
                    .ok_or(ParseError)?
                    .push(Expression::List(list));
            }
        }
    }

    match &mut stack[..] {
        [top] => Ok(std::mem::take(top)),
        _ => Err(ParseError),
    }
}

#[derive(Debug)]
enum Environment {
    Cons(String, Expression, Box<Environment>),
    Nil,
}

impl Environment {
    pub fn get(&self, ident: &str) -> Option<&Expression> {
        match self {
            Environment::Cons(key, val, parent) => {
                if key == ident {
                    Some(val)
                } else {
                    parent.get(ident)
                }
            }
            Environment::Nil => None,
        }
    }
}

#[derive(Debug)]
struct EvalError;

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "eval error")
    }
}

impl std::error::Error for EvalError {}

fn eval(expr: Expression, env: &Environment) -> Result<Expression, EvalError> {
    match expr {
        Expression::Symbol(ref ident) => Ok(env.get(ident).ok_or(EvalError)?.clone()),
        Expression::List(list) => match list {
            List::Cons(_) => {
                let (rator, rand) = List::head_taillist(list).or(Err(EvalError))?;
                let rator = eval(rator, env)?;

                match rator {
                    Expression::Builtin(builtin) => match builtin {
                        Builtin::Quote => {
                            let arg = List::single(rand).or(Err(EvalError))?;
                            Ok(arg)
                        }
                        Builtin::TestMonop(op) => {
                            let arg = List::single(rand).or(Err(EvalError))?;
                            let arg = eval(arg, env)?;
                            Ok(Expression::Bool(match op {
                                TestMonop::Zero => matches!(arg, Expression::Number(0)),
                                TestMonop::Nil => matches!(arg, Expression::List(List::Nil)),
                            }))
                        }
                        Builtin::ArithBinop(op) => {
                            let (rhs, rand) = List::head_taillist(rand).or(Err(EvalError))?;
                            let rhs = match eval(rhs, env)? {
                                Expression::Number(number) => Ok(number),
                                _ => Err(EvalError),
                            }?;
                            let lhs = List::single(rand).or(Err(EvalError))?;
                            let lhs = match eval(lhs, env)? {
                                Expression::Number(number) => Ok(number),
                                _ => Err(EvalError),
                            }?;
                            Ok(Expression::Number(match op {
                                ArithBinop::Add => rhs + lhs,
                                ArithBinop::Sub => rhs - lhs,
                                ArithBinop::Mul => rhs * lhs,
                                ArithBinop::Div => rhs / lhs,
                            }))
                        }
                        Builtin::ListMonop(op) => {
                            let arg = List::single(rand).or(Err(EvalError))?;
                            let arg = match eval(arg, env)? {
                                Expression::List(list) => Ok(list),
                                _ => Err(EvalError),
                            }?;
                            let (head, tail) = List::head_tail(arg).or(Err(EvalError))?;
                            match op {
                                ListMonop::Head => Ok(head),
                                ListMonop::Tail => Ok(tail),
                            }
                        }
                    },
                    _ => Err(EvalError),
                }
            }
            List::Nil => Ok(Expression::List(List::Nil)),
        },
        _ => Ok(expr),
    }
}
