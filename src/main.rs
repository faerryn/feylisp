fn main() {
    let env = Environment::default();

    for arg in std::env::args().skip(1) {
        match std::fs::read_to_string(arg) {
            Ok(src) => match pipeline(&src, &env) {
                Ok(exprs) => {
                    for expr in exprs {
                        println!("{}", expr);
                    }
                }
                Err(err) => eprintln!("error: {}", err),
            },
            Err(err) => eprintln!("error: {}", err),
        }
    }
}

#[test]
fn factorial() {
    let env = Environment::default();
    let src = "
(let ((Y (lambda (r) ((lambda (f) (f f)) (lambda (f) (r (lambda (x) ((f f) x)))))))
      (fact (lambda (f) (lambda (n) (if (zero? n) 1 (* n (f (- n 1))))))))
  ((Y fact) 5))
";
    let result = pipeline(src, &env);
    assert!(matches!(result.as_deref(), Ok([Expression::Number(120)])));
}

#[derive(Debug)]
enum PipelineError {
    LexError(LexError),
    ParseError(ParseError),
    EvalError(EvalError),
}

impl std::fmt::Display for PipelineError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for PipelineError {}

fn pipeline(src: &str, env: &Environment) -> Result<Vec<Expression>, PipelineError> {
    parse(lex(src).or_else(|err| Err(PipelineError::LexError(err)))?)
        .or_else(|err| Err(PipelineError::ParseError(err)))?
        .into_iter()
        .map(|expr| eval(expr, &env))
        .collect::<Result<_, EvalError>>()
        .or_else(|err| Err(PipelineError::EvalError(err)))
}

#[derive(Debug)]
enum Lexeme {
    Number(i32),
    Symbol(String),
    Open,
    Close,
}

type LexError = std::num::ParseIntError;

fn lex(src: &str) -> Result<Vec<Lexeme>, LexError> {
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
enum List {
    Cons(Box<Expression>, Box<Expression>),
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
            List::Cons(head, tail) => Ok((*head, *tail)),
            List::Nil => Err(ListError),
        }
    }
    fn head_taillist(list: List) -> Result<(Expression, List), ListError> {
        match List::head_tail(list)? {
            (head, Expression::List(tail)) => Ok((head, tail)),
            _ => Err(ListError),
        }
    }
    fn single(list: List) -> Result<Expression, ListError> {
        match List::head_taillist(list)? {
            (head, List::Nil) => Ok(head),
            _ => Err(ListError),
        }
    }
}

impl From<Vec<Expression>> for List {
    fn from(vec: Vec<Expression>) -> Self {
        let mut result = List::Nil;

        for expr in vec.into_iter().rev() {
            result = List::Cons(Box::new(expr), Box::new(Expression::List(result)));
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
                    Expression::List(tail) => match tail {
                        List::Cons(_, _) => write!(f, " {}", tail),
                        List::Nil => Ok(()),
                    },
                    _ => write!(f, " . {}", tail),
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
    Eval,
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
                Builtin::Eval => "eval",
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
    env: Box<Environment>,
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

#[derive(Clone, Debug)]
enum Environment {
    Cons(String, Expression, Box<Environment>),
    Nil,
}

impl Environment {
    fn get(&self, ident: &str) -> Option<&Expression> {
        match self {
            Environment::Cons(key, val, _) if key == ident => Some(val),
            Environment::Cons(_, _, parent) => parent.get(ident),
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
            ("head", Builtin::ListMonop(ListMonop::Head)),
            ("tail", Builtin::ListMonop(ListMonop::Tail)),
            ("cons", Builtin::Cons),
            ("list", Builtin::List),
            ("let", Builtin::Let),
            ("eval", Builtin::Eval),
        ] {
            env = Environment::Cons(key.to_owned(), Expression::Builtin(val), Box::new(env));
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

fn eval(expr: Expression, env: &Environment) -> Result<Expression, EvalError> {
    match expr {
        Expression::Symbol(ref ident) => env.get(ident).cloned().ok_or(EvalError::FreeVariable),
        Expression::List(list) => match list {
            List::Cons(_, _) => {
                let (rator, rand) = List::head_taillist(list).or(Err(EvalError::MalformedApply))?;
                let rator = eval(rator, env)?;

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
                                env: Box::new(env.clone()),
                            }))
                        }
                        Builtin::If => {
                            let (cond, rand) =
                                List::head_taillist(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let (when, rand) =
                                List::head_taillist(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let unless =
                                List::single(rand).or(Err(EvalError::Malformed(builtin)))?;

                            if let Expression::Bool(true) = eval(cond, &env)? {
                                eval(when, &env)
                            } else {
                                eval(unless, &env)
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
                            let rhs = match eval(rhs, &env)? {
                                Expression::Number(number) => Ok(number),
                                _ => Err(EvalError::Malformed(builtin)),
                            }?;
                            let lhs = List::single(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let lhs = match eval(lhs, &env)? {
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
                            let head = eval(head, &env)?;
                            let tail = List::single(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let tail = eval(tail, env)?;
                            Ok(Expression::List(List::Cons(Box::new(head), Box::new(tail))))
                        }
                        Builtin::List => {
                            fn list_eval(env: &Environment, list: List) -> Result<List, EvalError> {
                                match list {
                                    List::Cons(_, _) => {
                                        let (head, tail) = List::head_taillist(list)
                                            .or(Err(EvalError::Malformed(Builtin::List)))?;
                                        let head = eval(head, &env)?;
                                        let tail = Expression::List(list_eval(env, tail)?);
                                        Ok(List::Cons(Box::new(head), Box::new(tail)))
                                    }
                                    List::Nil => Ok(List::Nil),
                                }
                            }
                            Ok(Expression::List(list_eval(env, rand)?))
                        }
                        Builtin::Let => {
                            let (varlist, rand) =
                                List::head_taillist(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let varlist = match varlist {
                                Expression::List(list) => Ok(list),
                                _ => Err(EvalError::Malformed(builtin)),
                            }?;
                            let body = List::single(rand).or(Err(EvalError::Malformed(builtin)))?;

                            fn create_let_env(
                                caller_env: &Environment,
                                new_env: &Environment,
                                varlist: List,
                            ) -> Result<Environment, EvalError> {
                                match varlist {
                                    List::Cons(_, _) => {
                                        let (head, tail) = List::head_taillist(varlist)
                                            .or(Err(EvalError::Malformed(Builtin::Let)))?;
                                        let head = match head {
                                            Expression::List(list) => Ok(list),
                                            _ => Err(EvalError::Malformed(Builtin::Let)),
                                        }?;
                                        let (name, head) = List::head_taillist(head)
                                            .or(Err(EvalError::Malformed(Builtin::Let)))?;
                                        let name = match name {
                                            Expression::Symbol(symbol) => Ok(symbol),
                                            _ => Err(EvalError::Malformed(Builtin::Let)),
                                        }?;
                                        let value = List::single(head)
                                            .or(Err(EvalError::Malformed(Builtin::Let)))?;
                                        let value = eval(value, caller_env)?;
                                        let new_env = Environment::Cons(
                                            name,
                                            value,
                                            Box::new(new_env.clone()),
                                        );
                                        create_let_env(caller_env, &new_env, tail)
                                    }
                                    List::Nil => Ok(new_env.clone()),
                                }
                            }

                            eval(body, &create_let_env(&env.clone(), env, varlist)?)
                        }
                        Builtin::Eval => {
                            let arg = List::single(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let arg = eval(arg, &env)?;
                            eval(arg, env)
                        }
                    },
                    Expression::Closure(Closure {
                        params,
                        body,
                        env: closure_env,
                    }) => {
                        fn create_call_env(
                            caller_env: &Environment,
                            new_env: &Environment,
                            params: List,
                            args: List,
                        ) -> Result<Environment, EvalError> {
                            match (&params, &args) {
                                (List::Cons(_, _), List::Cons(_, _)) => {
                                    let (param, params) = List::head_taillist(params)
                                        .or(Err(EvalError::MalformedApply))?;
                                    let param = match param {
                                        Expression::Symbol(symbol) => Ok(symbol),
                                        _ => Err(EvalError::MalformedApply),
                                    }?;
                                    let (arg, args) = List::head_taillist(args)
                                        .or(Err(EvalError::MalformedApply))?;
                                    let arg = eval(arg, &caller_env)?;

                                    let new_env =
                                        Environment::Cons(param, arg, Box::new(new_env.clone()));

                                    create_call_env(caller_env, &new_env, params, args)
                                }
                                (List::Nil, List::Nil) => Ok(new_env.clone()),
                                _ => Err(EvalError::MalformedApply),
                            }
                        }

                        eval(*body, &create_call_env(&env, &closure_env, params, rand)?)
                    }
                    _ => Err(EvalError::MalformedApply),
                }
            }
            List::Nil => Ok(Expression::List(List::Nil)),
        },
        _ => Ok(expr),
    }
}
