use crate::expr::{
    ArBinop, Builtin, Closure, Expression, List, ListMonop, NumBinop, OrdBinop, TestMonop,
};

use std::rc::Rc;

#[derive(Debug)]
pub enum Environment {
    Cons(Rc<String>, Rc<Expression>, Rc<Environment>),
    Nil,
}

impl Environment {
    #[must_use]
    pub fn core_env() -> Self {
        let mut result = Environment::Nil;
        for (name, value) in crate::expr::BUILTIN_NAME_ALIST {
            result = Environment::Cons(
                Rc::new(name.to_string()),
                Rc::new(Expression::Builtin(value)),
                Rc::new(result),
            );
        }
        result
    }

    #[must_use]
    pub fn get(&self, ident: &str) -> Option<Rc<Expression>> {
        match self {
            Environment::Cons(name, value, _) if **name == ident => Some(Rc::clone(value)),
            Environment::Cons(_, _, parent) => parent.get(ident),
            Environment::Nil => None,
        }
    }
}

impl std::fmt::Display for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Environment::Cons(name, value, parent) => {
                write!(f, "{}: {}", name, value)?;
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
pub enum Error {
    ExpectedValue,
    ExpectedCallable,
    ExpectedList,
    ExpectedPair,
    ExpectedNumber,
    ExpectedSymbol,
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
) -> Result<(Option<Rc<Expression>>, Rc<Environment>), Error> {
    match &*expr {
        Expression::Symbol(ident) => {
            if let Some(value) = env.get(ident) {
                Ok((Some(value), env))
            } else {
                Err(Error::FreeVariable(Rc::clone(ident)))
            }
        }
        Expression::List(list) => match &**list {
            List::Cons(rator, rand) => {
                let (rator, env) = eval(Rc::clone(rator), env)?;
                let rator = rator.ok_or(Error::ExpectedValue)?;

                match &*rator {
                    Expression::Builtin(builtin) => match builtin {
                        Builtin::Quote => {
                            let [arg] = unpack_args(Rc::clone(rand))?;
                            Ok((Some(arg), env))
                        }
                        Builtin::Lambda => {
                            let [params, body] = unpack_args(Rc::clone(rand))?;
                            Ok((
                                Some(Rc::new(Expression::Closure(Closure {
                                    params,
                                    body,
                                    env: Rc::clone(&env),
                                }))),
                                env,
                            ))
                        }
                        Builtin::If => {
                            let [cond, when, unless] = unpack_args(Rc::clone(rand))?;

                            let (cond, env) = eval(cond, env)?;
                            let cond = cond.ok_or(Error::ExpectedValue)?;

                            if matches!(*cond, Expression::Bool(false)) {
                                eval(unless, env)
                            } else {
                                eval(when, env)
                            }
                        }
                        Builtin::TestMonop(op) => {
                            let [arg] = unpack_args(Rc::clone(rand))?;

                            let (arg, env) = eval(arg, env)?;
                            let arg = arg.ok_or(Error::ExpectedValue)?;
                            Ok((
                                Some(Rc::new(Expression::Bool(match op {
                                    TestMonop::Number => matches!(*arg, Expression::Number(_)),
                                    TestMonop::List => matches!(*arg, Expression::List(_)),
                                    TestMonop::Nil => {
                                        if let Expression::List(list) = &*arg {
                                            matches!(**list, List::Nil)
                                        } else {
                                            false
                                        }
                                    }
                                }))),
                                env,
                            ))
                        }
                        Builtin::NumBinop(op) => {
                            let [rhs, lhs] = unpack_args(Rc::clone(rand))?;

                            let (rhs, env) = eval(rhs, env)?;
                            let rhs = rhs.ok_or(Error::ExpectedValue)?;
                            let rhs = to_number(rhs)?;

                            let (lhs, env) = eval(lhs, env)?;
                            let lhs = lhs.ok_or(Error::ExpectedValue)?;
                            let lhs = to_number(lhs)?;

                            Ok((
                                Some(Rc::new(match op {
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
                                })),
                                env,
                            ))
                        }
                        Builtin::ListMonop(op) => {
                            let [arg] = unpack_args(Rc::clone(rand))?;

                            let (arg, env) = eval(arg, env)?;
                            let arg = arg.ok_or(Error::ExpectedValue)?;
                            let arg = to_list(arg)?;

                            if let List::Cons(head, tail) = &*arg {
                                Ok((
                                    Some(match op {
                                        ListMonop::Head => Rc::clone(head),
                                        ListMonop::Tail => {
                                            Rc::new(Expression::List(Rc::clone(tail)))
                                        }
                                    }),
                                    env,
                                ))
                            } else {
                                Err(Error::ExpectedPair)
                            }
                        }
                        Builtin::Cons => {
                            let [head, tail] = unpack_args(Rc::clone(rand))?;

                            let (head, env) = eval(head, env)?;
                            let head = head.ok_or(Error::ExpectedValue)?;

                            let (tail, env) = eval(tail, env)?;
                            let tail = tail.ok_or(Error::ExpectedValue)?;
                            let tail = to_list(tail)?;

                            Ok((
                                Some(Rc::new(Expression::List(Rc::new(List::Cons(head, tail))))),
                                env,
                            ))
                        }
                        Builtin::Let => {
                            let [varlist, body] = unpack_args(Rc::clone(rand))?;

                            let varlist = to_list(varlist)?;

                            let (new_env, env) = create_let_env(varlist, Rc::clone(&env), env)?;
                            let (result, _) = eval(body, new_env)?;
                            Ok((result, env))
                        }
                        Builtin::Eval => {
                            let [arg] = unpack_args(Rc::clone(rand))?;

                            let (arg, env) = eval(arg, env)?;
                            let arg = arg.ok_or(Error::ExpectedValue)?;

                            eval(arg, env)
                        }
                        Builtin::Define => {
                            let [name, value] = unpack_args(Rc::clone(rand))?;

                            let name = to_symbol(name)?;

                            let (value, env) = eval(value, env)?;
                            let value = value.ok_or(Error::ExpectedValue)?;

                            let new_env = Environment::Cons(name, value, env);
                            Ok((None, Rc::new(new_env)))
                        }
                    },
                    Expression::Closure(Closure {
                        params,
                        body,
                        env: closure_env,
                    }) => {
                        let params = to_list(Rc::clone(params))?;
                        let (closure_env, new_env) = create_closure_env(
                            params,
                            Rc::clone(rand),
                            0,
                            Rc::clone(closure_env),
                            env,
                        )?;
                        let (result, _) = eval(Rc::clone(body), closure_env)?;
                        Ok((result, new_env))
                    }
                    _ => Err(Error::ExpectedCallable),
                }
            }
            List::Nil => Ok((Some(expr), env)),
        },
        _ => Ok((Some(expr), env)),
    }
}

fn create_let_env(
    varlist: Rc<List>,
    env: Rc<Environment>,
    caller_env: Rc<Environment>,
) -> Result<(Rc<Environment>, Rc<Environment>), Error> {
    if let List::Cons(head, tail) = &*varlist {
        let head = to_list(Rc::clone(head))?;

        let [name, value] = unpack_args(head)?;

        let name = to_symbol(name)?;

        let (value, caller_env) = eval(value, caller_env)?;
        let value = value.ok_or(Error::ExpectedValue)?;

        let new_env = Environment::Cons(name, value, env);
        create_let_env(Rc::clone(tail), Rc::new(new_env), caller_env)
    } else {
        Ok((env, caller_env))
    }
}

fn create_closure_env(
    params: Rc<List>,
    args: Rc<List>,
    matched: usize,
    new_env: Rc<Environment>,
    caller_env: Rc<Environment>,
) -> Result<(Rc<Environment>, Rc<Environment>), Error> {
    match (&*params, &*args) {
        (List::Nil, List::Nil) => Ok((new_env, caller_env)),
        (List::Cons(param, params), List::Cons(arg, args)) => {
            let param = to_symbol(Rc::clone(param))?;
            let (arg, caller_env) = eval(Rc::clone(arg), caller_env)?;
            let arg = arg.ok_or(Error::ExpectedValue)?;

            let new_env = Environment::Cons(param, arg, new_env);

            create_closure_env(
                Rc::clone(params),
                Rc::clone(args),
                matched + 1,
                Rc::new(new_env),
                caller_env,
            )
        }
        _ => {
            let mut expected = matched;
            let params = (&*params).into_iter();
            for _ in params {
                expected += 1;
            }

            let mut received = matched;
            let args = (&*args).into_iter();
            for _ in args {
                received += 1;
            }

            Err(Error::MismatchedOperand { received, expected })
        }
    }
}

fn unpack_args<const S: usize>(list: Rc<List>) -> Result<[Rc<Expression>; S], Error> {
    let result: Vec<_> = (&*list).into_iter().collect();
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

fn to_list(expr: Rc<Expression>) -> Result<Rc<List>, Error> {
    match &*expr {
        Expression::List(list) => Ok(Rc::clone(list)),
        _ => Err(Error::ExpectedList),
    }
}

fn to_symbol(expr: Rc<Expression>) -> Result<Rc<String>, Error> {
    match &*expr {
        Expression::Symbol(symbol) => Ok(Rc::clone(symbol)),
        _ => Err(Error::ExpectedSymbol),
    }
}
