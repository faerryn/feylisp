fn main() -> Result<(), Box<dyn std::error::Error>> {
    let env = std::rc::Rc::new(Environment::default());

    for arg in std::env::args().skip(1) {
        for expr in parse(lex(&std::fs::read_to_string(arg)?)?)? {
            let orig = format!("{}", expr);
            match eval(expr, std::rc::Rc::clone(&env)) {
                Ok(expr) => println!("{} -> {}", orig, expr),
                Err(err) => println!("{}: {}", orig, err),
            }
        }
    }

    Ok(())
}

#[test]
fn factorial() {
    let src = "
(((lambda (r)
    ((lambda (f) (f f))
     (lambda (f) (r (lambda (x) ((f f) x))))))
  (lambda (f) (lambda (n) (if (zero? n) 1 (* n (f (- n 1)))))))
 5)
";
    let exprs = parse(lex(src).unwrap()).unwrap();
    assert_eq!(exprs.len(), 1);
    let expr = exprs.into_iter().next().unwrap();
    let result = eval(expr, std::rc::Rc::new(Environment::default())).unwrap();
    assert!(matches!(result, Expression::Number(120)));
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
            Expression::Closure(Closure { params, body, env }) => {
                write!(f, "(lambda ({}) {}) [{}]", params, body, env)
            }
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
        write!(f, "{:?}", self)
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

#[derive(Copy, Clone, Debug)]
enum Builtin {
    Quote,
    Lambda,
    If,
    TestMonop(TestMonop),
    NumBinop(NumBinop),
    ListMonop(ListMonop),
    Cons,
    List,
    Let,
}

impl std::fmt::Display for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Builtin::Quote => "quote",
                Builtin::Lambda => "lambda",
                Builtin::If => "if",
                Builtin::TestMonop(op) => match op {
                    TestMonop::Zero => "zero?",
                    TestMonop::Nil => "nil?",
                },
                Builtin::NumBinop(op) => match op {
                    NumBinop::ArBinop(op) => match op {
                        ArBinop::Add => "+",
                        ArBinop::Sub => "-",
                        ArBinop::Mul => "*",
                        ArBinop::Div => "/",
                    },
                    NumBinop::OrdBinop(op) => match op {
                        OrdBinop::Eql => "=",
                        OrdBinop::Lt => "<",
                    },
                },
                Builtin::ListMonop(op) => match op {
                    ListMonop::Head => "hd",
                    ListMonop::Tail => "tl",
                },
                Builtin::Cons => "cons",
                Builtin::List => "list",
                Builtin::Let => "let",
            }
        )
    }
}

#[derive(Copy, Clone, Debug)]
enum TestMonop {
    Zero,
    Nil,
}

#[derive(Copy, Clone, Debug)]
enum NumBinop {
    ArBinop(ArBinop),
    OrdBinop(OrdBinop),
}

#[derive(Copy, Clone, Debug)]
enum ArBinop {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Copy, Clone, Debug)]
enum OrdBinop {
    Eql,
    Lt,
}

#[derive(Copy, Clone, Debug)]
enum ListMonop {
    Head,
    Tail,
}

#[derive(Clone, Debug)]
struct Closure {
    params: List,
    body: Box<Expression>,
    env: std::rc::Rc<Environment>,
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
    Cons(String, Expression, std::rc::Rc<Environment>),
    Nil,
}

impl Environment {
    fn get(&self, ident: &str) -> Option<&Expression> {
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

impl Default for Environment {
    fn default() -> Self {
        let mut env = Environment::Nil;
        for (key, val) in [
            ("quote", Builtin::Quote),
            ("lambda", Builtin::Lambda),
            ("if", Builtin::If),
            ("zero?", Builtin::TestMonop(TestMonop::Zero)),
            ("nil?", Builtin::TestMonop(TestMonop::Nil)),
            ("+", Builtin::NumBinop(NumBinop::ArBinop(ArBinop::Add))),
            ("-", Builtin::NumBinop(NumBinop::ArBinop(ArBinop::Sub))),
            ("*", Builtin::NumBinop(NumBinop::ArBinop(ArBinop::Mul))),
            ("/", Builtin::NumBinop(NumBinop::ArBinop(ArBinop::Div))),
            ("=", Builtin::NumBinop(NumBinop::OrdBinop(OrdBinop::Eql))),
            ("<", Builtin::NumBinop(NumBinop::OrdBinop(OrdBinop::Lt))),
            ("car", Builtin::ListMonop(ListMonop::Head)),
            ("cdr", Builtin::ListMonop(ListMonop::Tail)),
            ("cons", Builtin::Cons),
            ("list", Builtin::List),
            ("let", Builtin::Let),
        ] {
            env = Environment::Cons(
                key.to_owned(),
                Expression::Builtin(val),
                std::rc::Rc::new(env),
            );
        }
        env
    }
}

impl std::fmt::Display for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Environment::Cons(key, val, parent) => {
                write!(f, "{}: {}", key, val)?;
                match **parent {
                    Environment::Cons(_, _, _) => write!(f, ", {}", parent),
                    Environment::Nil => Ok(()),
                }
            }
            Environment::Nil => Ok(()),
        }
    }
}

#[derive(Debug)]
enum EvalError {
    FreeVariable,
    MalformedApply,
    Malformed(Builtin),
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for EvalError {}

fn eval(expr: Expression, env: std::rc::Rc<Environment>) -> Result<Expression, EvalError> {
    match expr {
        // TODO: use reference couting to get ride of this .aclone()
        Expression::Symbol(ref ident) => Ok(env.get(ident).ok_or(EvalError::FreeVariable)?.clone()),
        Expression::List(list) => match list {
            List::Cons(_) => {
                let (rator, rand) = List::head_taillist(list).or(Err(EvalError::MalformedApply))?;
                let rator = eval(rator, std::rc::Rc::clone(&env))?;

                match rator {
                    Expression::Builtin(builtin) => match builtin {
                        Builtin::Quote => {
                            let arg = List::single(rand).or(Err(EvalError::Malformed(builtin)))?;
                            Ok(arg)
                        }
                        Builtin::Lambda => {
                            let (params, rand) =
                                List::head_taillist(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let params = match params {
                                Expression::List(list) => Ok(list),
                                _ => Err(EvalError::Malformed(builtin)),
                            }?;
                            let body = List::single(rand).or(Err(EvalError::Malformed(builtin)))?;

                            Ok(Expression::Closure(Closure {
                                params,
                                body: Box::new(body),
                                env: std::rc::Rc::clone(&env),
                            }))
                        }
                        Builtin::If => {
                            let (cond, rand) =
                                List::head_taillist(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let (when, rand) =
                                List::head_taillist(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let unless =
                                List::single(rand).or(Err(EvalError::Malformed(builtin)))?;

                            if let Expression::Bool(true) = eval(cond, std::rc::Rc::clone(&env))? {
                                eval(when, std::rc::Rc::clone(&env))
                            } else {
                                eval(unless, std::rc::Rc::clone(&env))
                            }
                        }
                        Builtin::TestMonop(op) => {
                            let arg = List::single(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let arg = eval(arg, env)?;
                            Ok(Expression::Bool(match op {
                                TestMonop::Zero => matches!(arg, Expression::Number(0)),
                                TestMonop::Nil => matches!(arg, Expression::List(List::Nil)),
                            }))
                        }
                        Builtin::NumBinop(op) => {
                            let (rhs, rand) =
                                List::head_taillist(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let rhs = match eval(rhs, std::rc::Rc::clone(&env))? {
                                Expression::Number(number) => Ok(number),
                                _ => Err(EvalError::Malformed(builtin)),
                            }?;
                            let lhs = List::single(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let lhs = match eval(lhs, std::rc::Rc::clone(&env))? {
                                Expression::Number(number) => Ok(number),
                                _ => Err(EvalError::Malformed(builtin)),
                            }?;
                            Ok(match op {
                                NumBinop::ArBinop(op) => Expression::Number(match op {
                                    ArBinop::Add => rhs + lhs,
                                    ArBinop::Sub => rhs - lhs,
                                    ArBinop::Mul => rhs * lhs,
                                    ArBinop::Div => rhs / lhs,
                                }),
                                NumBinop::OrdBinop(op) => Expression::Bool(match op {
                                    OrdBinop::Eql => rhs == lhs,
                                    OrdBinop::Lt => rhs < lhs,
                                }),
                            })
                        }
                        Builtin::ListMonop(op) => {
                            let arg = List::single(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let arg = match eval(arg, env)? {
                                Expression::List(list) => Ok(list),
                                _ => Err(EvalError::Malformed(builtin)),
                            }?;
                            let (head, tail) =
                                List::head_tail(arg).or(Err(EvalError::Malformed(builtin)))?;
                            match op {
                                ListMonop::Head => Ok(head),
                                ListMonop::Tail => Ok(tail),
                            }
                        }
                        Builtin::Cons => {
                            let (head, rand) =
                                List::head_taillist(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let head = eval(head, std::rc::Rc::clone(&env))?;
                            let tail = List::single(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let tail = eval(tail, env)?;
                            Ok(Expression::List(List::Cons(Box::new(Cons { head, tail }))))
                        }
                        Builtin::List => {
                            fn list_eval(
                                env: std::rc::Rc<Environment>,
                                list: List,
                            ) -> Result<List, EvalError> {
                                match list {
                                    List::Cons(_) => {
                                        let (head, tail) = List::head_taillist(list)
                                            .or(Err(EvalError::Malformed(Builtin::List)))?;
                                        let head = eval(head, std::rc::Rc::clone(&env))?;
                                        let tail = Expression::List(list_eval(env, tail)?);
                                        Ok(List::Cons(Box::new(Cons { head, tail })))
                                    }
                                    List::Nil => Ok(List::Nil),
                                }
                            }
                            Ok(Expression::List(list_eval(env, rand)?))
                        }
                        Builtin::Let => {
                            let (name, rand) =
                                List::head_taillist(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let name = match name {
                                Expression::Symbol(symbole) => Ok(symbole),
                                _ => Err(EvalError::Malformed(builtin)),
                            }?;
                            let (value, rand) =
                                List::head_taillist(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let value = eval(value, std::rc::Rc::clone(&env))?;

                            let body = List::single(rand).or(Err(EvalError::Malformed(builtin)))?;

                            let new_env = std::rc::Rc::new(Environment::Cons(name, value, env));

                            eval(body, new_env)
                        }
                    },
                    Expression::Closure(Closure {
                        params,
                        body,
                        env: closure_env,
                    }) => {
                        fn create_call_env(
                            caller_env: std::rc::Rc<Environment>,
                            new_env: std::rc::Rc<Environment>,
                            params: List,
                            args: List,
                        ) -> Result<std::rc::Rc<Environment>, EvalError> {
                            match (&params, &args) {
                                (List::Cons(_), List::Cons(_)) => {
                                    let (param, params) = List::head_taillist(params)
                                        .or(Err(EvalError::MalformedApply))?;
                                    let param = match param {
                                        Expression::Symbol(symbol) => Ok(symbol),
                                        _ => Err(EvalError::MalformedApply),
                                    }?;
                                    let (arg, args) = List::head_taillist(args)
                                        .or(Err(EvalError::MalformedApply))?;
                                    let arg = eval(arg, std::rc::Rc::clone(&caller_env))?;

                                    let new_env = std::rc::Rc::new(Environment::Cons(
                                        param,
                                        arg,
                                        std::rc::Rc::clone(&new_env),
                                    ));

                                    create_call_env(caller_env, new_env, params, args)
                                }
                                (List::Nil, List::Nil) => Ok(new_env),
                                _ => Err(EvalError::MalformedApply),
                            }
                        }

                        eval(*body, create_call_env(env, closure_env, params, rand)?)
                    }
                    _ => Err(EvalError::MalformedApply),
                }
            }
            List::Nil => Ok(Expression::List(List::Nil)),
        },
        _ => Ok(expr),
    }
}
