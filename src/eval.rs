use crate::expr::{ArBinop, Builtin, Callable, Closure, Expression, List, ListMonop, NumBinop};

use std::rc::Rc;

#[derive(Debug)]
pub enum Environment {
    Pair(Rc<String>, Rc<Expression>, Rc<Environment>),
    Nil,
}

pub const DEFAULT_ENVIRONMENT: [(&str, Expression); 22] = [
    ("nil", Expression::List(List::Nil)),
    ("#t", Expression::Bool(true)),
    ("#f", Expression::Bool(false)),
    ("quote", Expression::Builtin(Builtin::Quote)),
    (
        "lambda",
        Expression::Builtin(Builtin::Callable(Callable::Lambda)),
    ),
    (
        "macro",
        Expression::Builtin(Builtin::Callable(Callable::Macro)),
    ),
    ("if", Expression::Builtin(Builtin::If)),
    ("nil?", Expression::Builtin(Builtin::TestNil)),
    ("type", Expression::Builtin(Builtin::Type)),
    ("eql", Expression::Builtin(Builtin::Eql)),
    (
        "_+",
        Expression::Builtin(Builtin::NumBinop(NumBinop::ArBinop(ArBinop::Add))),
    ),
    (
        "-",
        Expression::Builtin(Builtin::NumBinop(NumBinop::ArBinop(ArBinop::Sub))),
    ),
    (
        "*",
        Expression::Builtin(Builtin::NumBinop(NumBinop::ArBinop(ArBinop::Mul))),
    ),
    (
        "/",
        Expression::Builtin(Builtin::NumBinop(NumBinop::ArBinop(ArBinop::Div))),
    ),
    ("=", Expression::Builtin(Builtin::Eql)),
    ("<", Expression::Builtin(Builtin::NumBinop(NumBinop::Lt))),
    (
        "head",
        Expression::Builtin(Builtin::ListMonop(ListMonop::Head)),
    ),
    (
        "tail",
        Expression::Builtin(Builtin::ListMonop(ListMonop::Tail)),
    ),
    ("pair", Expression::Builtin(Builtin::Pair)),
    ("let", Expression::Builtin(Builtin::Let)),
    ("eval", Expression::Builtin(Builtin::Eval)),
    ("define", Expression::Builtin(Builtin::Define)),
];

impl Environment {
    #[must_use]
    pub fn core_env() -> Self {
        let mut env = Environment::Nil;

        for (name, value) in DEFAULT_ENVIRONMENT {
            env = Environment::Pair(Rc::new(name.to_owned()), Rc::new(value), Rc::new(env));
        }

        env
    }

    #[must_use]
    pub fn get(&self, ident: &str) -> Option<Rc<Expression>> {
        match self {
            Environment::Pair(name, value, _) if **name == ident => Some(Rc::clone(value)),
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
                match **parent {
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
    ExpectedCallable,
    ExpectedList,
    ExpectedPair,
    ExpectedNumber,
    ExpectedSymbol,
    ExpectedLikeType,
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
    match &*expr {
        Expression::Symbol(ident) => {
            if let Some(value) = env.get(ident) {
                Ok((value, env))
            } else {
                Err(Error::FreeVariable(Rc::clone(ident)))
            }
        }

        Expression::List(list) => match &*list {
            List::Pair(rator, rand) => {
                let (rator, env) = eval(Rc::clone(rator), env)?;

                match &*rator {
                    Expression::Builtin(builtin) => match builtin {
                        Builtin::Quote => {
                            let [arg] = unpack_args(List::clone(rand))?;
                            Ok((arg, env))
                        }

                        Builtin::Callable(callable) => {
                            let [params, body] = unpack_args(List::clone(rand))?;
                            let params = to_list(params)?;
                            let closure = Closure {
                                params,
                                body,
                                env: Rc::clone(&env),
                            };
                            let closure = match callable {
                                Callable::Lambda => Expression::Closure(closure),
                                Callable::Macro => Expression::Macro(closure),
                            };
                            Ok((Rc::new(closure), env))
                        }

                        Builtin::If => {
                            let [cond, when, unless] = unpack_args(List::clone(rand))?;

                            let (cond, env) = eval(cond, env)?;

                            if matches!(*cond, Expression::Bool(false)) {
                                eval(unless, env)
                            } else {
                                eval(when, env)
                            }
                        }

                        Builtin::TestNil => {
                            let [arg] = unpack_args(List::clone(rand))?;

                            let (arg, env) = eval(arg, env)?;
                            let arg = to_list(arg)?;

                            Ok((Rc::new(Expression::Bool(matches!(arg, List::Nil))), env))
                        }

                        Builtin::NumBinop(op) => {
                            let [rhs, lhs] = unpack_args(List::clone(rand))?;

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
                            let [arg] = unpack_args(List::clone(rand))?;

                            let (arg, env) = eval(arg, env)?;
                            let arg = to_list(arg)?;

                            if let List::Pair(head, tail) = arg {
                                Ok((
                                    match op {
                                        ListMonop::Head => head,
                                        ListMonop::Tail => {
                                            Rc::new(Expression::List(List::clone(&tail)))
                                        }
                                    },
                                    env,
                                ))
                            } else {
                                Err(Error::ExpectedPair)
                            }
                        }

                        Builtin::Pair => {
                            let [head, tail] = unpack_args(List::clone(rand))?;

                            let (head, env) = eval(head, env)?;

                            let (tail, env) = eval(tail, env)?;
                            let tail = to_list(tail)?;

                            Ok((
                                Rc::new(Expression::List(List::Pair(head, Rc::new(tail)))),
                                env,
                            ))
                        }

                        Builtin::Let => {
                            let [varlist, body] = unpack_args(List::clone(rand))?;
                            let varlist = to_list(varlist)?;
                            let (new_env, env) = create_let_env(varlist, Rc::clone(&env), env)?;
                            let (result, _) = eval(body, new_env)?;
                            Ok((result, env))
                        }

                        Builtin::Eval => {
                            let [arg] = unpack_args(List::clone(rand))?;
                            let (arg, env) = eval(arg, env)?;
                            eval(arg, env)
                        }

                        Builtin::Define => {
                            let [name, value] = unpack_args(List::clone(rand))?;
                            let name = to_symbol(name)?;
                            let (value, env) = eval(value, env)?;
                            let new_env = Environment::Pair(name, Rc::clone(&value), env);
                            Ok((value, Rc::new(new_env)))
                        }

                        Builtin::Type => {
                            let [arg] = unpack_args(List::clone(rand))?;
                            let (arg, env) = eval(arg, env)?;

                            Ok((
                                Rc::new(Expression::Symbol(Rc::new(String::from(
                                    match arg.as_ref() {
                                        Expression::Number(_) => "number",
                                        Expression::Symbol(_) => "symbol",
                                        Expression::List(_) => "list",
                                        Expression::Bool(_) => "bool",
                                        Expression::Builtin(_) => "builting",
                                        Expression::Closure(_) => "closure",
                                        Expression::Macro(_) => "macro",
                                    },
                                )))),
                                env,
                            ))
                        }

                        Builtin::Eql => {
                            let [rhs, lhs] = unpack_args(List::clone(rand))?;

                            let (rhs, env) = eval(rhs, env)?;

                            let (lhs, env) = eval(lhs, env)?;

                            match (rhs.as_ref(), lhs.as_ref()) {
                                (Expression::Number(lhs), Expression::Number(rhs)) => {
                                    Ok((Rc::new(Expression::Bool(lhs == rhs)), env))
                                }
                                (Expression::Symbol(lhs), Expression::Symbol(rhs)) => {
                                    Ok((Rc::new(Expression::Bool(lhs == rhs)), env))
                                }
                                (Expression::Bool(lhs), Expression::Bool(rhs)) => {
                                    Ok((Rc::new(Expression::Bool(lhs == rhs)), env))
                                }
                                _ => Err(Error::ExpectedLikeType),
                            }
                        }
                    },

                    Expression::Closure(Closure {
                        params,
                        body,
                        env: closure_env,
                    }) => {
                        let (closure_env, env) = create_closure_env(
                            List::clone(params),
                            List::clone(rand),
                            0,
                            Rc::clone(closure_env),
                            env,
                            true,
                        )?;
                        let (result, _) = eval(Rc::clone(body), closure_env)?;
                        Ok((result, env))
                    }

                    Expression::Macro(Closure {
                        params,
                        body,
                        env: closure_env,
                    }) => {
                        let (closure_env, env) = create_closure_env(
                            List::clone(params),
                            List::clone(rand),
                            0,
                            Rc::clone(closure_env),
                            env,
                            false,
                        )?;
                        let (result, closure_env) = eval(Rc::clone(body), closure_env)?;
                        let (result, _) = eval(result, closure_env)?;
                        Ok((result, env))
                    }

                    _ => Err(Error::ExpectedCallable),
                }
            }

            List::Nil => Ok((expr, env)),
        },

        _ => Ok((expr, env)),
    }
}

fn create_let_env(
    varlist: List,
    env: Rc<Environment>,
    caller_env: Rc<Environment>,
) -> Result<(Rc<Environment>, Rc<Environment>), Error> {
    if let List::Pair(head, tail) = varlist {
        let head = to_list(head)?;

        let [name, value] = unpack_args(head)?;

        let name = to_symbol(name)?;

        let (value, caller_env) = eval(value, caller_env)?;

        let new_env = Environment::Pair(name, value, env);
        create_let_env(List::clone(&tail), Rc::new(new_env), caller_env)
    } else {
        Ok((env, caller_env))
    }
}

fn create_closure_env(
    params: List,
    args: List,
    matched: usize,
    new_env: Rc<Environment>,
    caller_env: Rc<Environment>,
    eval_args: bool,
) -> Result<(Rc<Environment>, Rc<Environment>), Error> {
    match (params, args) {
        (List::Nil, List::Nil) => Ok((new_env, caller_env)),

        (List::Pair(param, params), args)
            if matches!(param.as_ref(), Expression::Symbol(symbol) if symbol.as_ref() == "...")
                && matches!(*params, List::Nil) =>
        {
            let param = to_symbol(param)?;
            let (args, caller_env) = if eval_args {
                eval_list(args, caller_env)?
            } else {
                (args, caller_env)
            };

            let new_env = Environment::Pair(param, Rc::new(Expression::List(args)), new_env);
            Ok((Rc::new(new_env), caller_env))
        }

        (List::Pair(param, params), List::Pair(arg, args)) => {
            let param = to_symbol(param)?;

            let (arg, caller_env) = if eval_args {
                eval(arg, caller_env)?
            } else {
                (arg, caller_env)
            };

            let new_env = Environment::Pair(param, arg, new_env);

            create_closure_env(
                List::clone(&params),
                List::clone(&args),
                matched + 1,
                Rc::new(new_env),
                caller_env,
                eval_args,
            )
        }

        (params, args) => {
            let mut expected = matched;
            let params = params.into_iter();
            for _ in params {
                expected += 1;
            }

            let mut received = matched;
            let args = args.into_iter();
            for _ in args {
                received += 1;
            }

            Err(Error::MismatchedOperand { received, expected })
        }
    }
}

fn unpack_args<const S: usize>(list: List) -> Result<[Rc<Expression>; S], Error> {
    let result: Vec<_> = list.into_iter().collect();
    let result_len = result.len();

    result.try_into().map_err(|_| Error::MismatchedOperand {
        received: result_len,
        expected: S,
    })
}

fn to_number(expr: Rc<Expression>) -> Result<i32, Error> {
    match &*expr {
        Expression::Number(num) => Ok(*num),
        _ => Err(Error::ExpectedNumber),
    }
}

fn to_list(expr: Rc<Expression>) -> Result<List, Error> {
    match &*expr {
        Expression::List(list) => Ok(List::clone(list)),
        _ => Err(Error::ExpectedList),
    }
}

fn to_symbol(expr: Rc<Expression>) -> Result<Rc<String>, Error> {
    match &*expr {
        Expression::Symbol(symbol) => Ok(Rc::clone(symbol)),
        _ => Err(Error::ExpectedSymbol),
    }
}

fn eval_list(list: List, env: Rc<Environment>) -> Result<(List, Rc<Environment>), Error> {
    match list {
        List::Pair(head, tail) => {
            let (head, env) = eval(head, env)?;
            let (tail, env) = eval_list(List::clone(&tail), env)?;
            Ok((List::Pair(head, Rc::new(tail)), env))
        }
        List::Nil => Ok((list, env)),
    }
}
