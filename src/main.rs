fn main() {
    let mut env = Environment::default();

    for arg in std::env::args().skip(1) {
        match std::fs::read_to_string(arg) {
            Ok(src) => match pipeline(&src, env.clone()) {
                Ok((exprs, new_env)) => {
                    for expr in exprs {
                        println!("{}", expr);
                    }
                    env = new_env;
                }
                Err(err) => eprintln!("error: {}", err),
            },
            Err(err) => eprintln!("error: {}", err),
        }
    }
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
        let env = Environment::default();
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
        let env = Environment::default();
        let result = pipeline(src, env);
        let result = match result {
            Ok((ref exprs, env)) => Ok((exprs.as_slice(), env)),
            Err(err) => Err(err),
        };
        assert!(matches!(result, Ok(([_, _, Expression::Number(55)], _))));
    }
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
        env = new_env;
        result.push(expr);
    }

    Ok((result, env))
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
            ("define", Builtin::Define),
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

fn eval(expr: Expression, env: Environment) -> Result<(Expression, Environment), EvalError> {
    match expr {
        Expression::Symbol(ref ident) => {
            Ok((env.get(ident).ok_or(EvalError::FreeVariable)?.clone(), env))
        }
        Expression::List(list) => match list {
            List::Cons(_, _) => {
                let (rator, rand) = List::head_taillist(list).or(Err(EvalError::MalformedApply))?;
                let (rator, env) = eval(rator, env)?;

                match rator {
                    Expression::Builtin(builtin) => match builtin {
                        Builtin::Quote => {
                            let arg = List::single(rand).or(Err(EvalError::Malformed(builtin)))?;
                            Ok((arg, env))
                        }
                        Builtin::Lambda => {
                            let (params, rand) =
                                List::head_taillist(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let params = match params {
                                Expression::List(list) => Ok(list),
                                _ => Err(EvalError::Malformed(builtin)),
                            }?;
                            let body = List::single(rand).or(Err(EvalError::Malformed(builtin)))?;

                            Ok((
                                Expression::Closure(Closure {
                                    params,
                                    body: Box::new(body),
                                    env: Box::new(env.clone()),
                                }),
                                env,
                            ))
                        }
                        Builtin::If => {
                            let (cond, rand) =
                                List::head_taillist(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let (when, rand) =
                                List::head_taillist(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let unless =
                                List::single(rand).or(Err(EvalError::Malformed(builtin)))?;

                            let (cond, env) = eval(cond, env)?;

                            if let Expression::Bool(true) = cond {
                                eval(when, env)
                            } else {
                                eval(unless, env)
                            }
                        }
                        Builtin::TestMonop(op) => {
                            let arg = List::single(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let (arg, env) = eval(arg, env)?;
                            Ok((
                                Expression::Bool(match op {
                                    TestMonop::Zero => matches!(arg, Expression::Number(0)),
                                    TestMonop::Nil => matches!(arg, Expression::List(List::Nil)),
                                }),
                                env,
                            ))
                        }
                        Builtin::NumBinop(op) => {
                            let (rhs, rand) =
                                List::head_taillist(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let (rhs, env) = eval(rhs, env)?;
                            let rhs = match rhs {
                                Expression::Number(number) => Ok(number),
                                _ => Err(EvalError::Malformed(builtin)),
                            }?;
                            let lhs = List::single(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let (lhs, env) = eval(lhs, env)?;
                            let lhs = match lhs {
                                Expression::Number(number) => Ok(number),
                                _ => Err(EvalError::Malformed(builtin)),
                            }?;
                            Ok((
                                match op {
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
                                },
                                env,
                            ))
                        }
                        Builtin::ListMonop(op) => {
                            let arg = List::single(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let (arg, env) = eval(arg, env)?;
                            let arg = match arg {
                                Expression::List(list) => Ok(list),
                                _ => Err(EvalError::Malformed(builtin)),
                            }?;
                            let (head, tail) =
                                List::head_tail(arg).or(Err(EvalError::Malformed(builtin)))?;
                            Ok((
                                match op {
                                    ListMonop::Head => head,
                                    ListMonop::Tail => tail,
                                },
                                env,
                            ))
                        }
                        Builtin::Cons => {
                            let (head, rand) =
                                List::head_taillist(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let (head, env) = eval(head, env)?;
                            let tail = List::single(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let (tail, env) = eval(tail, env)?;
                            Ok((
                                Expression::List(List::Cons(Box::new(head), Box::new(tail))),
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
                                        let (head, tail) = List::head_taillist(list)
                                            .or(Err(EvalError::Malformed(Builtin::List)))?;
                                        let (head, env) = eval(head, env)?;
                                        let (tail, env) = list_eval(tail, env)?;
                                        let tail = Expression::List(tail);
                                        Ok((List::Cons(Box::new(head), Box::new(tail)), env))
                                    }
                                    List::Nil => Ok((List::Nil, env)),
                                }
                            }
                            let (result, env) = list_eval(rand, env)?;
                            Ok((Expression::List(result), env))
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
                                varlist: List,
                                new_env: Environment,
                                caller_env: Environment,
                            ) -> Result<(Environment, Environment), EvalError>
                            {
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
                                        let (value, caller_env) = eval(value, caller_env)?;
                                        let new_env =
                                            Environment::Cons(name, value, Box::new(new_env));
                                        create_let_env(tail, new_env, caller_env)
                                    }
                                    List::Nil => Ok((new_env, caller_env)),
                                }
                            }

                            let (new_env, env) = create_let_env(varlist, env.clone(), env)?;
                            let (result, _) = eval(body, new_env)?;
                            Ok((result, env))
                        }
                        Builtin::Eval => {
                            let arg = List::single(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let (arg, env) = eval(arg, env)?;
                            eval(arg, env)
                        }
                        Builtin::Define => {
                            let (name, rand) =
                                List::head_taillist(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let name = match name {
                                Expression::Symbol(symbol) => Ok(symbol),
                                _ => Err(EvalError::Malformed(builtin)),
                            }?;
                            let value =
                                List::single(rand).or(Err(EvalError::Malformed(builtin)))?;
                            let (value, env) = eval(value, env)?;
                            let new_env = Environment::Cons(name, value.clone(), Box::new(env));
                            Ok((value, new_env))
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
                                    let (param, params) = List::head_taillist(params)
                                        .or(Err(EvalError::MalformedApply))?;
                                    let param = match param {
                                        Expression::Symbol(symbol) => Ok(symbol),
                                        _ => Err(EvalError::MalformedApply),
                                    }?;
                                    let (arg, args) = List::head_taillist(args)
                                        .or(Err(EvalError::MalformedApply))?;
                                    let (arg, caller_env) = eval(arg, caller_env)?;

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
            List::Nil => Ok((Expression::List(List::Nil), env)),
        },
        _ => Ok((expr, env)),
    }
}
