use crate::expr::{
    ArBinop, Builtin, Closure, Environment, Expression, List, ListMonop, NumBinop, OrdBinop,
    TestMonop,
};

#[derive(Debug)]
pub enum Error {
    FreeVariable(String),
    MalformedApply,
    Malformed(Builtin),
    ExpectedExpression,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for Error {}

pub fn eval(
    expr: Expression,
    env: Environment,
) -> Result<(Option<Expression>, Environment), Error> {
    match expr {
        Expression::Symbol(ident) => Ok((
            Some(env.get(&ident).ok_or(Error::FreeVariable(ident))?),
            env,
        )),
        Expression::List(list) => {
            if let List::Cons(rator, rand) = list {
                let (rator, rand) = (*rator, *rand);
                let (rator, env) = eval(rator, env)?;
                let rator = match rator {
                    Some(rator) => rator,
                    None => return Err(Error::ExpectedExpression),
                };

                match rator {
                    Expression::Builtin(builtin) => match builtin {
                        Builtin::Quote => {
                            let (arg, rand) = rand.decons().ok_or(Error::Malformed(builtin))?;
                            if matches!(rand, List::Cons(_, _)) {
                                return Err(Error::Malformed(builtin));
                            }

                            Ok((Some(arg), env))
                        }
                        Builtin::Lambda => {
                            let (params, rand) =
                                List::decons(rand).ok_or(Error::Malformed(builtin))?;
                            let params = match params {
                                Expression::List(list) => list,
                                _ => return Err(Error::Malformed(builtin)),
                            };
                            let (body, rand) = rand.decons().ok_or(Error::Malformed(builtin))?;
                            if matches!(rand, List::Cons(_, _)) {
                                return Err(Error::Malformed(builtin));
                            }

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
                                List::decons(rand).ok_or(Error::Malformed(builtin))?;
                            let (when, rand) =
                                List::decons(rand).ok_or(Error::Malformed(builtin))?;
                            let (unless, rand) = rand.decons().ok_or(Error::Malformed(builtin))?;
                            if matches!(rand, List::Cons(_, _)) {
                                return Err(Error::Malformed(builtin));
                            }

                            let (cond, env) = eval(cond, env)?;
                            let cond = cond.ok_or(Error::ExpectedExpression)?;

                            if let Expression::Bool(true) = cond {
                                eval(when, env)
                            } else {
                                eval(unless, env)
                            }
                        }
                        Builtin::TestMonop(op) => {
                            let (arg, rand) = rand.decons().ok_or(Error::Malformed(builtin))?;
                            if matches!(rand, List::Cons(_, _)) {
                                return Err(Error::Malformed(builtin));
                            }

                            let (arg, env) = eval(arg, env)?;
                            let arg = arg.ok_or(Error::Malformed(builtin))?;
                            Ok((
                                Some(Expression::Bool(match op {
                                    TestMonop::Number => matches!(arg, Expression::Number(_)),
                                    TestMonop::List => matches!(arg, Expression::List(_)),
                                    TestMonop::Nil => matches!(arg, Expression::List(List::Nil)),
                                })),
                                env,
                            ))
                        }
                        Builtin::NumBinop(op) => {
                            let (rhs, rand) =
                                List::decons(rand).ok_or(Error::Malformed(builtin))?;
                            let (rhs, env) = eval(rhs, env)?;
                            let rhs = rhs.ok_or(Error::Malformed(builtin))?;
                            let rhs = match rhs {
                                Expression::Number(number) => Ok(number),
                                _ => Err(Error::ExpectedExpression),
                            }?;
                            let (lhs, rand) = rand.decons().ok_or(Error::Malformed(builtin))?;
                            if matches!(rand, List::Cons(_, _)) {
                                return Err(Error::Malformed(builtin));
                            }

                            let (lhs, env) = eval(lhs, env)?;
                            let lhs = lhs.ok_or(Error::Malformed(builtin))?;
                            let lhs = match lhs {
                                Expression::Number(number) => Ok(number),
                                _ => Err(Error::Malformed(builtin)),
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
                            let (arg, rand) = rand.decons().ok_or(Error::Malformed(builtin))?;
                            if matches!(rand, List::Cons(_, _)) {
                                return Err(Error::Malformed(builtin));
                            }

                            let (arg, env) = eval(arg, env)?;
                            let arg = arg.ok_or(Error::Malformed(builtin))?;
                            let arg = match arg {
                                Expression::List(list) => Ok(list),
                                _ => Err(Error::Malformed(builtin)),
                            }?;
                            let (head, tail) =
                                List::decons(arg).ok_or(Error::Malformed(builtin))?;
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
                                List::decons(rand).ok_or(Error::Malformed(builtin))?;
                            let (head, env) = eval(head, env)?;
                            let head = head.ok_or(Error::Malformed(builtin))?;
                            let (tail, rand) = rand.decons().ok_or(Error::Malformed(builtin))?;
                            if matches!(rand, List::Cons(_, _)) {
                                return Err(Error::Malformed(builtin));
                            }

                            let (tail, env) = eval(tail, env)?;
                            let tail = tail.ok_or(Error::Malformed(builtin))?;
                            let tail = match tail {
                                Expression::List(list) => list,
                                _ => return Err(Error::Malformed(builtin)),
                            };
                            Ok((Some(Expression::List(List::cons(head, tail))), env))
                        }
                        Builtin::List => {
                            fn list_eval(
                                list: List,
                                env: Environment,
                            ) -> Result<(List, Environment), Error> {
                                if let List::Cons(head, tail) = list {
                                    let (head, tail) = (*head, *tail);
                                    let (head, env) = eval(head, env)?;
                                    let head = head.ok_or(Error::Malformed(Builtin::List))?;
                                    let (tail, env) = list_eval(tail, env)?;
                                    Ok((List::cons(head, tail), env))
                                } else {
                                    Ok((list, env))
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
                            ) -> Result<(Environment, Environment), Error>
                            {
                                if let List::Cons(head, tail) = varlist {
                                    let (head, tail) = (*head, *tail);
                                    let head = match head {
                                        Expression::List(list) => Ok(list),
                                        _ => Err(Error::Malformed(Builtin::Let)),
                                    }?;
                                    let (name, head) =
                                        List::decons(head).ok_or(Error::Malformed(Builtin::Let))?;
                                    let name = match name {
                                        Expression::Symbol(symbol) => Ok(symbol),
                                        _ => Err(Error::Malformed(Builtin::Let)),
                                    }?;
                                    let (value, head) =
                                        head.decons().ok_or(Error::Malformed(Builtin::Let))?;
                                    if matches!(head, List::Cons(_, _)) {
                                        return Err(Error::Malformed(Builtin::Let));
                                    }

                                    let (value, caller_env) = eval(value, caller_env)?;
                                    let value = value.ok_or(Error::ExpectedExpression)?;
                                    let new_env = Environment::Cons(name, value, Box::new(new_env));
                                    create_let_env(tail, new_env, caller_env)
                                } else {
                                    Ok((new_env, caller_env))
                                }
                            }

                            let (varlist, rand) =
                                List::decons(rand).ok_or(Error::Malformed(builtin))?;
                            let varlist = match varlist {
                                Expression::List(list) => Ok(list),
                                _ => Err(Error::Malformed(builtin)),
                            }?;
                            let (body, rand) = rand.decons().ok_or(Error::Malformed(builtin))?;
                            if matches!(rand, List::Cons(_, _)) {
                                return Err(Error::Malformed(builtin));
                            }

                            let (new_env, env) = create_let_env(varlist, env.clone(), env)?;
                            let (result, _) = eval(body, new_env)?;
                            Ok((result, env))
                        }
                        Builtin::Eval => {
                            let (arg, rand) = rand.decons().ok_or(Error::Malformed(builtin))?;
                            if matches!(rand, List::Cons(_, _)) {
                                return Err(Error::Malformed(builtin));
                            }

                            let (arg, env) = eval(arg, env)?;
                            let arg = arg.ok_or(Error::ExpectedExpression)?;
                            eval(arg, env)
                        }
                        Builtin::Define => {
                            let (name, rand) =
                                List::decons(rand).ok_or(Error::Malformed(builtin))?;
                            let name = match name {
                                Expression::Symbol(symbol) => Ok(symbol),
                                _ => Err(Error::Malformed(builtin)),
                            }?;
                            let (value, rand) = rand.decons().ok_or(Error::Malformed(builtin))?;
                            if matches!(rand, List::Cons(_, _)) {
                                return Err(Error::Malformed(builtin));
                            }

                            let (value, env) = eval(value, env)?;
                            let value = value.ok_or(Error::ExpectedExpression)?;
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
                        ) -> Result<(Environment, Environment), Error> {
                            match (params, args) {
                                (List::Nil, List::Nil) => Ok((new_env, caller_env)),
                                (List::Cons(param, params), List::Cons(arg, args)) => {
                                    let (param, params) = (*param, *params);
                                    let (arg, args) = (*arg, *args);
                                    let param = match param {
                                        Expression::Symbol(symbol) => Ok(symbol),
                                        _ => Err(Error::MalformedApply),
                                    }?;
                                    let (arg, caller_env) = eval(arg, caller_env)?;
                                    let arg = arg.ok_or(Error::ExpectedExpression)?;

                                    let new_env = Environment::Cons(param, arg, Box::new(new_env));

                                    create_call_env(params, args, new_env, caller_env)
                                }
                                _ => Err(Error::MalformedApply),
                            }
                        }

                        let (new_env, env) = create_call_env(params, rand, *closure_env, env)?;
                        let (result, _) = eval(*body, new_env)?;
                        Ok((result, env))
                    }
                    _ => Err(Error::MalformedApply),
                }
            } else {
                Ok((Some(Expression::List(list)), env))
            }
        }
        _ => Ok((Some(expr), env)),
    }
}