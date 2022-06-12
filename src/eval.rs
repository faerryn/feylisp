use crate::expr::{
    ArBinop, Builtin, Callable, Closure, Enclosure, Expression, List, ListMonop, NumBinop,
};

use std::rc::Rc;

pub enum Environment {
    Pair(Rc<String>, Rc<Expression>, Rc<Environment>),
    Nil,
}

pub const ELLIPSIS: &str = "...";

impl Environment {
    #[must_use]
    pub fn core_env() -> Self {
        let mut env = Environment::Nil;

        let default_environment = [
            ("nil", Expression::List(Rc::new(List::Nil))),
            ("#t", Expression::Bool(true)),
            ("#f", Expression::Bool(false)),
            (
                "quote",
                Expression::Callable(Rc::new(Callable::Builtin(Builtin::Quote))),
            ),
            (
                "lambda",
                Expression::Callable(Rc::new(Callable::Builtin(Builtin::Enclosure(
                    Enclosure::Lambda,
                )))),
            ),
            (
                "macro",
                Expression::Callable(Rc::new(Callable::Builtin(Builtin::Enclosure(
                    Enclosure::Macro,
                )))),
            ),
            (
                "if",
                Expression::Callable(Rc::new(Callable::Builtin(Builtin::If))),
            ),
            (
                "type",
                Expression::Callable(Rc::new(Callable::Builtin(Builtin::Type))),
            ),
            (
                "builtin=",
                Expression::Callable(Rc::new(Callable::Builtin(Builtin::Equal))),
            ),
            (
                "builtin+",
                Expression::Callable(Rc::new(Callable::Builtin(Builtin::NumBinop(
                    NumBinop::ArBinop(ArBinop::Add),
                )))),
            ),
            (
                "builtin-",
                Expression::Callable(Rc::new(Callable::Builtin(Builtin::NumBinop(
                    NumBinop::ArBinop(ArBinop::Sub),
                )))),
            ),
            (
                "builtin*",
                Expression::Callable(Rc::new(Callable::Builtin(Builtin::NumBinop(
                    NumBinop::ArBinop(ArBinop::Mul),
                )))),
            ),
            (
                "builtin/",
                Expression::Callable(Rc::new(Callable::Builtin(Builtin::NumBinop(
                    NumBinop::ArBinop(ArBinop::Div),
                )))),
            ),
            (
                "builtin<",
                Expression::Callable(Rc::new(Callable::Builtin(Builtin::NumBinop(NumBinop::Lt)))),
            ),
            (
                "head",
                Expression::Callable(Rc::new(Callable::Builtin(Builtin::ListMonop(
                    ListMonop::Head,
                )))),
            ),
            (
                "tail",
                Expression::Callable(Rc::new(Callable::Builtin(Builtin::ListMonop(
                    ListMonop::Tail,
                )))),
            ),
            (
                "pair",
                Expression::Callable(Rc::new(Callable::Builtin(Builtin::Pair))),
            ),
            (
                "let",
                Expression::Callable(Rc::new(Callable::Builtin(Builtin::Let))),
            ),
            (
                "eval",
                Expression::Callable(Rc::new(Callable::Builtin(Builtin::Eval))),
            ),
            (
                "define",
                Expression::Callable(Rc::new(Callable::Builtin(Builtin::Define))),
            ),
            (
                "apply",
                Expression::Callable(Rc::new(Callable::Builtin(Builtin::Apply))),
            ),
        ];

        for (name, value) in default_environment {
            env = Environment::Pair(Rc::new(name.to_string()), Rc::new(value), Rc::new(env));
        }

        env
    }

    #[must_use]
    pub fn get(&self, ident: &str) -> Option<Rc<Expression>> {
        match self {
            Environment::Pair(name, value, _) if name.as_ref() == ident => Some(Rc::clone(value)),
            Environment::Pair(_, _, parent) => parent.get(ident),
            Environment::Nil => None,
        }
    }
}

impl std::fmt::Display for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Environment::Pair(name, value, parent) => {
                write!(f, "{}: {}", name, value)?;
                match parent.as_ref() {
                    Environment::Pair(_, _, _) => write!(f, ", {}", parent),
                    Environment::Nil => Ok(()),
                }
            }
            Environment::Nil => Ok(()),
        }
    }
}

#[derive(Debug)]
pub enum Error {
    ExpectedCallable(Rc<Expression>),
    ExpectedList(Rc<Expression>),
    ExpectedPair(Rc<Expression>),
    ExpectedNumber(Rc<Expression>),
    ExpectedSymbol(Rc<Expression>),
    MismatchedOperand { received: usize, expected: usize },
    FreeVariable(Rc<String>),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for Error {}

pub fn eval(
    expr: Rc<Expression>,
    env: Rc<Environment>,
) -> Result<(Rc<Expression>, Rc<Environment>), Error> {
    match expr.as_ref() {
        Expression::Symbol(ident) => {
            if let Some(value) = env.get(ident) {
                Ok((value, env))
            } else {
                Err(Error::FreeVariable(Rc::clone(ident)))
            }
        }

        Expression::List(list) => match list.as_ref() {
            List::Pair(rator, rand) => {
                let (rator, env) = eval(Rc::clone(rator), env)?;

                match rator.as_ref() {
                    Expression::Callable(callable) => {
                        call(Rc::clone(callable), Rc::clone(rand), env)
                    }
                    _ => Err(Error::ExpectedCallable(expr)),
                }
            }

            List::Nil => Ok((expr, env)),
        },

        _ => Ok((expr, env)),
    }
}

fn call(
    rator: Rc<Callable>,
    rand: Rc<List>,
    env: Rc<Environment>,
) -> Result<(Rc<Expression>, Rc<Environment>), Error> {
    match rator.as_ref() {
        Callable::Builtin(builtin) => match builtin {
            Builtin::Quote => {
                let [arg] = unpack_args(rand)?;
                Ok((arg, env))
            }

            Builtin::Enclosure(enclosure) => {
                let [params, body] = unpack_args(rand)?;
                let params = to_list(params)?;
                let closure = Closure {
                    params,
                    body,
                    env: Rc::clone(&env),
                };
                let closure = match enclosure {
                    Enclosure::Lambda => Callable::Closure(closure),
                    Enclosure::Macro => Callable::Macro(closure),
                };
                Ok((Rc::new(Expression::Callable(Rc::new(closure))), env))
            }

            Builtin::If => {
                let [cond, when, unless] = unpack_args(rand)?;

                let (cond, env) = eval(cond, env)?;

                if matches!(cond.as_ref(), Expression::Bool(false)) {
                    eval(unless, env)
                } else {
                    eval(when, env)
                }
            }

            Builtin::NumBinop(op) => {
                let [rhs, lhs] = unpack_args(rand)?;

                let (rhs, env) = eval(rhs, env)?;
                let rhs = to_number(rhs)?;

                let (lhs, env) = eval(lhs, env)?;
                let lhs = to_number(lhs)?;

                Ok((
                    Rc::new(match op {
                        NumBinop::ArBinop(op) => Expression::Number(match op {
                            ArBinop::Add => rhs + lhs,
                            ArBinop::Sub => rhs - lhs,
                            ArBinop::Mul => rhs * lhs,
                            ArBinop::Div => rhs / lhs,
                        }),
                        NumBinop::Lt => Expression::Bool(rhs < lhs),
                    }),
                    env,
                ))
            }

            Builtin::ListMonop(op) => {
                let [arg] = unpack_args(rand)?;

                let (arg, env) = eval(arg, env)?;

                if let List::Pair(head, tail) = to_list(Rc::clone(&arg))?.as_ref() {
                    let result = match op {
                        ListMonop::Head => Rc::clone(head),
                        ListMonop::Tail => Rc::new(Expression::List(Rc::clone(tail))),
                    };
                    Ok((result, env))
                } else {
                    Err(Error::ExpectedPair(arg))
                }
            }

            Builtin::Pair => {
                let [head, tail] = unpack_args(rand)?;

                let (head, env) = eval(head, env)?;

                let (tail, env) = eval(tail, env)?;
                let tail = to_list(tail)?;

                let pair = Rc::new(Expression::List(Rc::new(List::Pair(head, tail))));
                Ok((pair, env))
            }

            Builtin::Let => {
                let [varlist, body] = unpack_args(rand)?;
                let varlist = to_list(varlist)?;
                let (new_env, env) = create_let_env(varlist, Rc::clone(&env), env)?;
                let (result, _) = eval(body, new_env)?;
                Ok((result, env))
            }

            Builtin::Eval => {
                let [arg] = unpack_args(rand)?;
                let (arg, env) = eval(arg, env)?;
                eval(arg, env)
            }

            Builtin::Define => {
                let [name, value] = unpack_args(rand)?;
                let name = to_symbol(name)?;
                let (value, env) = eval(value, env)?;
                let new_env = Environment::Pair(name, Rc::clone(&value), env);
                Ok((value, Rc::new(new_env)))
            }

            Builtin::Type => {
                let [arg] = unpack_args(rand)?;
                let (arg, env) = eval(arg, env)?;

                Ok((
                    Rc::new(Expression::Symbol(Rc::new(
                        match arg.as_ref() {
                            Expression::Number(_) => "number",
                            Expression::Symbol(_) => "symbol",
                            Expression::List(_) => "list",
                            Expression::Bool(_) => "bool",
                            Expression::Callable(callable) => match callable.as_ref() {
                                Callable::Builtin(_) => "builtin",
                                Callable::Closure(_) => "closure",
                                Callable::Macro(_) => "macro",
                            },
                        }
                        .to_string(),
                    ))),
                    env,
                ))
            }

            Builtin::Equal => {
                let [rhs, lhs] = unpack_args(rand)?;
                let (rhs, env) = eval(rhs, env)?;
                let (lhs, env) = eval(lhs, env)?;
                Ok((Rc::new(Expression::Bool(lhs == rhs)), env))
            }

            Builtin::Apply => {
                let [callable, args] = unpack_args(rand)?;
                let (closure, env) = eval(callable, env)?;
                let (args, env) = eval(args, env)?;
                let args = to_list(args)?;
                let args = args.as_ref().into_iter().map(quote).collect();
                match closure.as_ref() {
                    Expression::Callable(callable) => call(Rc::clone(callable), Rc::new(args), env),
                    _ => Err(Error::ExpectedCallable(closure)),
                }
            }
        },

        Callable::Closure(Closure {
            params,
            body,
            env: closure_env,
        }) => {
            let (args, env) = eval_list(rand, env)?;
            let (closure_env, env) =
                create_call_env(Rc::clone(params), args, 0, Rc::clone(closure_env), env)?;
            let (result, _) = eval(Rc::clone(body), closure_env)?;
            Ok((result, env))
        }

        Callable::Macro(Closure {
            params,
            body,
            env: closure_env,
        }) => {
            let (closure_env, env) =
                create_call_env(Rc::clone(params), rand, 0, Rc::clone(closure_env), env)?;
            let (result, closure_env) = eval(Rc::clone(body), closure_env)?;
            let (result, _) = eval(result, closure_env)?;
            Ok((result, env))
        }
    }
}

fn create_let_env(
    varlist: Rc<List>,
    env: Rc<Environment>,
    caller_env: Rc<Environment>,
) -> Result<(Rc<Environment>, Rc<Environment>), Error> {
    if let List::Pair(head, tail) = varlist.as_ref() {
        let head = to_list(Rc::clone(head))?;

        let [name, value] = unpack_args(head)?;

        let name = to_symbol(name)?;

        let (value, caller_env) = eval(value, caller_env)?;

        let new_env = Environment::Pair(name, value, env);
        create_let_env(Rc::clone(tail), Rc::new(new_env), caller_env)
    } else {
        Ok((env, caller_env))
    }
}

fn create_call_env(
    params: Rc<List>,
    args: Rc<List>,
    matched: usize,
    new_env: Rc<Environment>,
    caller_env: Rc<Environment>,
) -> Result<(Rc<Environment>, Rc<Environment>), Error> {
    match (params.as_ref(), args.as_ref()) {
        (List::Nil, List::Nil) => Ok((new_env, caller_env)),

        (List::Pair(param, params), _)
            if matches!(param.as_ref(), Expression::Symbol(symbol) if symbol.as_str() == ELLIPSIS)
                && matches!(params.as_ref(), List::Nil) =>
        {
            let param = to_symbol(Rc::clone(param))?;
            let new_env = Environment::Pair(param, Rc::new(Expression::List(args)), new_env);
            Ok((Rc::new(new_env), caller_env))
        }

        (List::Pair(param, params), List::Pair(arg, args)) => {
            let param = to_symbol(Rc::clone(param))?;

            let new_env = Environment::Pair(param, Rc::clone(arg), new_env);

            create_call_env(
                Rc::clone(params),
                Rc::clone(args),
                matched + 1,
                Rc::new(new_env),
                caller_env,
            )
        }

        _ => {
            let mut expected = matched;
            let mut params = params.as_ref().into_iter().peekable();
            while let Some(param) = params.next() {
                if !(params.peek().is_none()
                    && matches!(param.as_ref(), Expression::Symbol(symbol) if symbol.as_str() == ELLIPSIS))
                {
                    expected += 1;
                }
            }

            let mut received = matched;
            for _ in args.as_ref().into_iter() {
                received += 1;
            }

            Err(Error::MismatchedOperand { received, expected })
        }
    }
}

fn unpack_args<const S: usize>(list: Rc<List>) -> Result<[Rc<Expression>; S], Error> {
    let result: Vec<_> = list.as_ref().into_iter().collect();
    let result_len = result.len();

    result.try_into().map_err(|_| Error::MismatchedOperand {
        received: result_len,
        expected: S,
    })
}

fn to_number(expr: Rc<Expression>) -> Result<i32, Error> {
    match expr.as_ref() {
        Expression::Number(num) => Ok(*num),
        _ => Err(Error::ExpectedNumber(expr)),
    }
}

fn to_list(expr: Rc<Expression>) -> Result<Rc<List>, Error> {
    match expr.as_ref() {
        Expression::List(list) => Ok(Rc::clone(list)),
        _ => Err(Error::ExpectedList(expr)),
    }
}

fn to_symbol(expr: Rc<Expression>) -> Result<Rc<String>, Error> {
    match expr.as_ref() {
        Expression::Symbol(symbol) => Ok(Rc::clone(symbol)),
        _ => Err(Error::ExpectedSymbol(expr)),
    }
}

fn eval_list(list: Rc<List>, env: Rc<Environment>) -> Result<(Rc<List>, Rc<Environment>), Error> {
    match list.as_ref() {
        List::Pair(head, tail) => {
            let (head, env) = eval(Rc::clone(head), env)?;
            let (tail, env) = eval_list(Rc::clone(tail), env)?;
            Ok((Rc::new(List::Pair(head, tail)), env))
        }
        List::Nil => Ok((list, env)),
    }
}

pub fn quote(expr: Rc<Expression>) -> Expression {
    let quote = Rc::new(Expression::Callable(Rc::new(Callable::Builtin(
        Builtin::Quote,
    ))));
    let nil = Rc::new(List::Nil);
    Expression::List(Rc::new(List::Pair(
        quote,
        Rc::new(List::Pair(expr, nil)),
    )))
}
