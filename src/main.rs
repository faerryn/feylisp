#![warn(clippy::pedantic)]

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut env = Environment::Nil;

    let mut want_repl = false;
    let mut loaded_files = false;

    for arg in std::env::args().skip(1) {
        if arg == "--repl" {
            want_repl = true;
        } else {
            let src = std::fs::read_to_string(arg)?;
            let (exprs, new_env) = pipeline(&src, env)?;
            for expr in exprs {
                println!("{}", expr);
            }
            env = new_env;
            loaded_files = true;
        }
    }

    if want_repl || !loaded_files {
        env = repl(env)?;
        println!("[{}]", env);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn factorial() {
        let src = "
(let ((Y (lambda (r) ((lambda (f) (f f)) (lambda (f) (r (lambda (x) ((f f) x)))))))
      (fact (lambda (f) (lambda (n) (if (zero? n) 1 (* n (f (- n 1))))))))
  ((Y fact) 5))
";
        let env = Environment::Nil;
        let result = pipeline(src, env);
        let result = match result {
            Ok((ref exprs, env)) => Ok((exprs.as_slice(), env)),
            Err(err) => Err(err),
        };
        assert!(matches!(result, Ok(([Expression::Number(120)], _))));
    }

    #[test]
    fn fibonnaci() {
        let src = "
(define Y (lambda (r) ((lambda (f) (f f)) (lambda (f) (r (lambda (x) ((f f) x)))))))
(define fib (lambda (f) (lambda (n) (if (< n 2) n (+ (f (- n 1)) (f (- n 2)))))))
((Y fib) 10)
";
        let env = Environment::Nil;
        let result = pipeline(src, env);
        let result = match result {
            Ok((ref exprs, env)) => Ok((exprs.as_slice(), env)),
            Err(err) => Err(err),
        };
        assert!(matches!(result, Ok(([Expression::Number(55)], _))));
    }
}

fn repl(mut env: Environment) -> Result<Environment, Box<dyn std::error::Error>> {
    use std::io::prelude::*;
    let stdin = std::io::stdin();
    let mut stdout = std::io::stdout();

    let mut src = String::new();
    let mut line = String::new();

    print!("> ");
    stdout.flush()?;

    while stdin.read_line(&mut line)? > 0 {
        src.push_str(&line);
        line.clear();
        if let Some(exprs) = lex(&src).ok().and_then(|src| parse(src).ok()) {
            src.clear();
            for expr in exprs {
                let (expr, new_env) = eval(expr, env)?;
                env = new_env;
                if let Some(expr) = expr {
                    println!("{}", expr);
                }
            }
            print!("> ");
            stdout.flush()?;
        }
    }

    Ok(env)
}

#[derive(Debug)]
enum PipelineError {
    Lex(LexError),
    Parse(ParseError),
    Eval(EvalError),
}

impl std::fmt::Display for PipelineError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for PipelineError {}

fn pipeline(
    src: &str,
    mut env: Environment,
) -> Result<(Vec<Expression>, Environment), PipelineError> {
    let exprs = parse(lex(src).map_err(PipelineError::Lex)?).map_err(PipelineError::Parse)?;
    let mut result = vec![];
    result.reserve(exprs.len());

    for expr in exprs {
        let (expr, new_env) = eval(expr, env).map_err(PipelineError::Eval)?;
        if let Some(expr) = expr {
            result.push(expr);
        }
        env = new_env;
    }

    Ok((result, env))
}

#[derive(Debug)]
enum Lexeme {
    Open,
    Close,
    Number(i32),
    Symbol(String),
}

type LexError = std::num::ParseIntError;

fn lex(src: &str) -> Result<Vec<Lexeme>, LexError> {
    enum State {
        Start,
        Sign,
        Number,
        Symbol,
    }

    let mut result = vec![];
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
        State::Number => {
            result.push(Lexeme::Number(src[prev..].parse::<_>()?));
        }
        State::Symbol | State::Sign => {
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
    Cons(Box<Expression>, Box<List>),
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
    fn head_tail(list: List) -> Result<(Expression, List), ListError> {
        match list {
            List::Cons(head, tail) => Ok((*head, *tail)),
            List::Nil => Err(ListError),
        }
    }
    fn single(list: List) -> Result<Expression, ListError> {
        match List::head_tail(list)? {
            (head, List::Nil) => Ok(head),
            _ => Err(ListError),
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
    Define,
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
                Builtin::Define => "define",
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
            Lexeme::Open => {
                stack.push(vec![]);
            }
            Lexeme::Close => {
                let list = List::from(stack.pop().ok_or(ParseError)?);
                // TODO: Error if . shows up anywhere other than right before the last
                // TODO: and interpret it as appropriate
                stack
                    .last_mut()
                    .ok_or(ParseError)?
                    .push(Expression::List(list));
            }
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
    fn get(&self, ident: &str) -> Option<Expression> {
        match self {
            Environment::Cons(key, val, _) if key == ident => Some(val.clone()),
            Environment::Cons(_, _, parent) => parent.get(ident),
            Environment::Nil => Some(Expression::Builtin(match ident {
                "quote" => Builtin::Quote,
                "lambda" => Builtin::Lambda,
                "if" => Builtin::If,
                "zero?" => Builtin::TestMonop(TestMonop::Zero),
                "nil?" => Builtin::TestMonop(TestMonop::Nil),
                "+" => Builtin::NumBinop(NumBinop::ArBinop(ArBinop::Add)),
                "-" => Builtin::NumBinop(NumBinop::ArBinop(ArBinop::Sub)),
                "*" => Builtin::NumBinop(NumBinop::ArBinop(ArBinop::Mul)),
                "/" => Builtin::NumBinop(NumBinop::ArBinop(ArBinop::Div)),
                "=" => Builtin::NumBinop(NumBinop::OrdBinop(OrdBinop::Eql)),
                "<" => Builtin::NumBinop(NumBinop::OrdBinop(OrdBinop::Lt)),
                "head" => Builtin::ListMonop(ListMonop::Head),
                "tail" => Builtin::ListMonop(ListMonop::Tail),
                "cons" => Builtin::Cons,
                "list" => Builtin::List,
                "let" => Builtin::Let,
                "eval" => Builtin::Eval,
                "define" => Builtin::Define,
                _ => return None,
            })),
        }
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
    ExpectedExpression,
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for EvalError {}

fn eval(
    expr: Expression,
    env: Environment,
) -> Result<(Option<Expression>, Environment), EvalError> {
    match expr {
        Expression::Symbol(ref ident) => {
            Ok((Some(env.get(ident).ok_or(EvalError::FreeVariable)?), env))
        }
        Expression::List(list) => match list {
            List::Cons(_, _) => {
                let (rator, rand) = List::head_tail(list).or(Err(EvalError::MalformedApply))?;
                let (rator, env) = eval(rator, env)?;
                let rator = match rator {
                    Some(rator) => rator,
                    None => return Err(EvalError::ExpectedExpression),
                };

                match rator {
                    Expression::Builtin(builtin) => match builtin {
                        Builtin::Quote => {
                            let arg = List::single(rand).or(Err(EvalError::Malformed(builtin)))?;
                            Ok((Some(arg), env))
                        }
                        Builtin::Lambda => {
                            let (params, rand) =
                                List::head_tail(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let params = match params {
                                Expression::List(list) => list,
                                _ => return Err(EvalError::Malformed(builtin)),
                            };
                            let body = List::single(rand).or(Err(EvalError::Malformed(builtin)))?;

                            Ok((
                                Some(Expression::Closure(Closure {
                                    params,
                                    body: Box::new(body),
                                    env: Box::new(env.clone()),
                                })),
                                env,
                            ))
                        }
                        Builtin::If => {
                            let (cond, rand) =
                                List::head_tail(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let (when, rand) =
                                List::head_tail(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let unless =
                                List::single(rand).or(Err(EvalError::Malformed(builtin)))?;

                            let (cond, env) = eval(cond, env)?;
                            let cond = cond.ok_or(EvalError::ExpectedExpression)?;

                            if let Expression::Bool(true) = cond {
                                eval(when, env)
                            } else {
                                eval(unless, env)
                            }
                        }
                        Builtin::TestMonop(op) => {
                            let arg = List::single(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let (arg, env) = eval(arg, env)?;
                            let arg = arg.ok_or(EvalError::Malformed(builtin))?;
                            Ok((
                                Some(Expression::Bool(match op {
                                    TestMonop::Zero => matches!(arg, Expression::Number(0)),
                                    TestMonop::Nil => matches!(arg, Expression::List(List::Nil)),
                                })),
                                env,
                            ))
                        }
                        Builtin::NumBinop(op) => {
                            let (rhs, rand) =
                                List::head_tail(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let (rhs, env) = eval(rhs, env)?;
                            let rhs = rhs.ok_or(EvalError::Malformed(builtin))?;
                            let rhs = match rhs {
                                Expression::Number(number) => Ok(number),
                                _ => Err(EvalError::ExpectedExpression),
                            }?;
                            let lhs = List::single(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let (lhs, env) = eval(lhs, env)?;
                            let lhs = lhs.ok_or(EvalError::Malformed(builtin))?;
                            let lhs = match lhs {
                                Expression::Number(number) => Ok(number),
                                _ => Err(EvalError::Malformed(builtin)),
                            }?;

                            Ok((
                                Some(match op {
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
                                }),
                                env,
                            ))
                        }
                        Builtin::ListMonop(op) => {
                            let arg = List::single(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let (arg, env) = eval(arg, env)?;
                            let arg = arg.ok_or(EvalError::Malformed(builtin))?;
                            let arg = match arg {
                                Expression::List(list) => Ok(list),
                                _ => Err(EvalError::Malformed(builtin)),
                            }?;
                            let (head, tail) =
                                List::head_tail(arg).or(Err(EvalError::Malformed(builtin)))?;
                            Ok((
                                Some(match op {
                                    ListMonop::Head => head,
                                    ListMonop::Tail => Expression::List(tail),
                                }),
                                env,
                            ))
                        }
                        Builtin::Cons => {
                            let (head, rand) =
                                List::head_tail(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let (head, env) = eval(head, env)?;
                            let head = head.ok_or(EvalError::Malformed(builtin))?;
                            let tail = List::single(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let (tail, env) = eval(tail, env)?;
                            let tail = tail.ok_or(EvalError::Malformed(builtin))?;
                            let tail = match tail {
                                Expression::List(list) => list,
                                _ => return Err(EvalError::Malformed(builtin)),
                            };
                            Ok((
                                Some(Expression::List(List::Cons(Box::new(head), Box::new(tail)))),
                                env,
                            ))
                        }
                        Builtin::List => {
                            fn list_eval(
                                list: List,
                                env: Environment,
                            ) -> Result<(List, Environment), EvalError>
                            {
                                match list {
                                    List::Cons(_, _) => {
                                        let (head, tail) = List::head_tail(list)
                                            .or(Err(EvalError::Malformed(Builtin::List)))?;
                                        let (head, env) = eval(head, env)?;
                                        let head =
                                            head.ok_or(EvalError::Malformed(Builtin::List))?;
                                        let (tail, env) = list_eval(tail, env)?;
                                        Ok((List::Cons(Box::new(head), Box::new(tail)), env))
                                    }
                                    List::Nil => Ok((List::Nil, env)),
                                }
                            }
                            let (result, env) = list_eval(rand, env)?;
                            Ok((Some(Expression::List(result)), env))
                        }
                        Builtin::Let => {
                            fn create_let_env(
                                varlist: List,
                                new_env: Environment,
                                caller_env: Environment,
                            ) -> Result<(Environment, Environment), EvalError>
                            {
                                match varlist {
                                    List::Cons(_, _) => {
                                        let (head, tail) = List::head_tail(varlist)
                                            .or(Err(EvalError::Malformed(Builtin::Let)))?;
                                        let head = match head {
                                            Expression::List(list) => Ok(list),
                                            _ => Err(EvalError::Malformed(Builtin::Let)),
                                        }?;
                                        let (name, head) = List::head_tail(head)
                                            .or(Err(EvalError::Malformed(Builtin::Let)))?;
                                        let name = match name {
                                            Expression::Symbol(symbol) => Ok(symbol),
                                            _ => Err(EvalError::Malformed(Builtin::Let)),
                                        }?;
                                        let value = List::single(head)
                                            .or(Err(EvalError::Malformed(Builtin::Let)))?;
                                        let (value, caller_env) = eval(value, caller_env)?;
                                        let value = value.ok_or(EvalError::ExpectedExpression)?;
                                        let new_env =
                                            Environment::Cons(name, value, Box::new(new_env));
                                        create_let_env(tail, new_env, caller_env)
                                    }
                                    List::Nil => Ok((new_env, caller_env)),
                                }
                            }

                            let (varlist, rand) =
                                List::head_tail(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let varlist = match varlist {
                                Expression::List(list) => Ok(list),
                                _ => Err(EvalError::Malformed(builtin)),
                            }?;
                            let body = List::single(rand).or(Err(EvalError::Malformed(builtin)))?;

                            let (new_env, env) = create_let_env(varlist, env.clone(), env)?;
                            let (result, _) = eval(body, new_env)?;
                            Ok((result, env))
                        }
                        Builtin::Eval => {
                            let arg = List::single(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let (arg, env) = eval(arg, env)?;
                            let arg = arg.ok_or(EvalError::ExpectedExpression)?;
                            eval(arg, env)
                        }
                        Builtin::Define => {
                            let (name, rand) =
                                List::head_tail(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let name = match name {
                                Expression::Symbol(symbol) => Ok(symbol),
                                _ => Err(EvalError::Malformed(builtin)),
                            }?;
                            let value =
                                List::single(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let (value, env) = eval(value, env)?;
                            let value = value.ok_or(EvalError::ExpectedExpression)?;
                            let new_env = Environment::Cons(name, value, Box::new(env));
                            Ok((None, new_env))
                        }
                    },
                    Expression::Closure(Closure {
                        params,
                        body,
                        env: closure_env,
                    }) => {
                        fn create_call_env(
                            params: List,
                            args: List,
                            new_env: Environment,
                            caller_env: Environment,
                        ) -> Result<(Environment, Environment), EvalError> {
                            match (&params, &args) {
                                (List::Cons(_, _), List::Cons(_, _)) => {
                                    let (param, params) = List::head_tail(params)
                                        .or(Err(EvalError::MalformedApply))?;
                                    let param = match param {
                                        Expression::Symbol(symbol) => Ok(symbol),
                                        _ => Err(EvalError::MalformedApply),
                                    }?;
                                    let (arg, args) = List::head_tail(args)
                                        .or(Err(EvalError::MalformedApply))?;
                                    let (arg, caller_env) = eval(arg, caller_env)?;
                                    let arg = arg.ok_or(EvalError::ExpectedExpression)?;

                                    let new_env = Environment::Cons(param, arg, Box::new(new_env));

                                    create_call_env(params, args, new_env, caller_env)
                                }
                                (List::Nil, List::Nil) => Ok((new_env, caller_env)),
                                _ => Err(EvalError::MalformedApply),
                            }
                        }

                        let (new_env, env) = create_call_env(params, rand, *closure_env, env)?;
                        let (result, _) = eval(*body, new_env)?;
                        Ok((result, env))
                    }
                    _ => Err(EvalError::MalformedApply),
                }
            }
            List::Nil => Ok((Some(Expression::List(List::Nil)), env)),
        },
        _ => Ok((Some(expr), env)),
    }
}
