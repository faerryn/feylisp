use crate::expr::{
    ArBinop, Builtin, Closure, Expression, List, ListMonop, NumBinop, OrdBinop, TestMonop,
};

#[derive(Debug, Clone)]
pub enum Environment {
    Cons(String, Box<Expression>, Box<Environment>),
    Nil,
}

impl Environment {
    #[must_use]
    pub fn core_env() -> Self {
        let mut result = Environment::Nil;
        for (name, value) in crate::expr::BUILTIN_NAME_ALIST {
            result = Environment::Cons(
                name.to_string(),
                Box::new(Expression::Builtin(value.clone())),
                Box::new(result),
            );
        }
        result
    }

    #[must_use]
    pub fn get(&self, ident: &str) -> Option<Expression> {
        match self {
            Environment::Cons(name, value, _) if name == ident => Some(*value.clone()),
            Environment::Cons(_, _, parent) => parent.get(ident),
            Environment::Nil => None,
        }
    }

    #[must_use]
    pub fn cons(name: String, value: Expression, env: Environment) -> Environment {
        Environment::Cons(name, Box::new(value), Box::new(env))
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

#[must_use]
pub fn eval(expr: Expression, env: Environment) -> (Option<Expression>, Environment) {
    match expr {
        Expression::Symbol(ident) => (
            Some(
                env.get(&ident)
                    .expect(&format!("free variable `{}'", ident)),
            ),
            env,
        ),
        Expression::List(list) => match list {
            List::Cons(rator, rand) => {
                let (rator, rand) = (*rator, *rand);
                let (rator, env) = eval(rator, env);
                let rator = match rator {
                    Some(rator) => rator,
                    None => panic!("malformed rator"),
                };

                match rator {
                    Expression::Builtin(builtin) => match builtin {
                        Builtin::Quote => {
                            let (arg, rand) = rand.decons().expect("malformed quote arg");
                            if matches!(rand, List::Cons(_, _)) {
                                panic!("malformed quote arg");
                            }
                            (Some(arg), env)
                        }
                        Builtin::Lambda => {
                            let (params, rand) =
                                List::decons(rand).expect("malformed lambda params");
                            let params = match params {
                                Expression::List(list) => list,
                                _ => panic!("malformed lambda params"),
                            };
                            let (body, rand) = rand.decons().expect("malformed lambda body");
                            if matches!(rand, List::Cons(_, _)) {
                                panic!("malformed lambda body");
                            }

                            (
                                Some(Expression::Closure(Closure {
                                    params,
                                    body: Box::new(body),
                                    env: env.clone(),
                                })),
                                env,
                            )
                        }
                        Builtin::Macro => {
                            let (params, rand) =
                                List::decons(rand).expect("malformed macro params");
                            let params = match params {
                                Expression::List(list) => list,
                                _ => panic!("malformed macro params"),
                            };
                            let (body, rand) = rand.decons().expect("malformed macro body");
                            if matches!(rand, List::Cons(_, _)) {
                                panic!("malformed macro body");
                            }

                            (
                                Some(Expression::Macro(Closure {
                                    params,
                                    body: Box::new(body),
                                    env: env.clone(),
                                })),
                                env,
                            )
                        }
                        Builtin::If => {
                            let (cond, rand) = List::decons(rand).expect("malformed if cond");
                            let (cond, env) = eval(cond, env);
                            let cond = cond.expect("malformed if cond");

                            let (when, rand) = List::decons(rand).expect("malformed if when");

                            let (unless, rand) = rand.decons().expect("malformed if unless");

                            if matches!(rand, List::Cons(_, _)) {
                                panic!("malformed if unless");
                            }

                            if let Expression::Bool(false) = cond {
                                eval(unless, env)
                            } else {
                                eval(when, env)
                            }
                        }
                        Builtin::TestMonop(op) => {
                            let (arg, rand) = rand.decons().expect("malformed testmonop arg");
                            if matches!(rand, List::Cons(_, _)) {
                                panic!("malformed testmonop arg");
                            }

                            let (arg, env) = eval(arg, env);
                            let arg = arg.expect("malformed testmonop arg");
                            (
                                Some(Expression::Bool(match op {
                                    TestMonop::Number => matches!(arg, Expression::Number(_)),
                                    TestMonop::List => matches!(arg, Expression::List(_)),
                                    TestMonop::Nil => matches!(arg, Expression::List(List::Nil)),
                                })),
                                env,
                            )
                        }
                        Builtin::NumBinop(op) => {
                            let (rhs, rand) = List::decons(rand).expect("malformed numbinop rhs");
                            let (rhs, env) = eval(rhs, env);
                            let rhs = rhs.expect("malformed numbinop rhs");
                            let rhs = match rhs {
                                Expression::Number(number) => number,
                                _ => panic!("malformed numbinop rhs"),
                            };

                            let (lhs, rand) = List::decons(rand).expect("malformed numbinop lhs");
                            let (lhs, env) = eval(lhs, env);
                            let lhs = lhs.expect("malformed numbinop lhs");
                            let lhs = match lhs {
                                Expression::Number(number) => number,
                                _ => panic!("malformed numbinop lhs"),
                            };
                            if matches!(rand, List::Cons(_, _)) {
                                panic!("malformed numbinop lhs")
                            }

                            (
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
                            )
                        }
                        Builtin::ListMonop(op) => {
                            let (arg, rand) = rand.decons().expect("malformed listmonop arg");
                            if matches!(rand, List::Cons(_, _)) {
                                panic!("malformed listmonop arg");
                            }

                            let (arg, env) = eval(arg, env);
                            let arg = arg.expect("malformed listmonop arg");
                            let arg = match arg {
                                Expression::List(list) => list,
                                _ => panic!("malformed listmonop arg"),
                            };

                            let (head, tail) = List::decons(arg).expect("malformed listmonop arg");
                            (
                                Some(match op {
                                    ListMonop::Head => head,
                                    ListMonop::Tail => Expression::List(tail),
                                }),
                                env,
                            )
                        }
                        Builtin::Cons => {
                            let (head, rand) = List::decons(rand).expect("malformed cons head");
                            let (head, env) = eval(head, env);
                            let head = head.expect("malformed cons head");

                            let (tail, rand) = rand.decons().expect("malformed cons tail");
                            let (tail, env) = eval(tail, env);
                            let tail = tail.expect("malformed cons tail");
                            let tail = match tail {
                                Expression::List(list) => list,
                                _ => panic!("malformed cons tail"),
                            };
                            if matches!(rand, List::Cons(_, _)) {
                                panic!("malformed cons tail");
                            }

                            (Some(Expression::List(List::cons(head, tail))), env)
                        }
                        Builtin::Let => {
                            let (varlist, rand) =
                                List::decons(rand).expect("malformed let varlist");
                            let varlist = match varlist {
                                Expression::List(list) => list,
                                _ => panic!("malformed let varlist"),
                            };

                            let (body, rand) = rand.decons().expect("malformed let body");
                            if matches!(rand, List::Cons(_, _)) {
                                panic!("malformed let body");
                            }

                            let (new_env, env) = let_env(varlist, env.clone(), env);
                            let (result, _) = eval(body, new_env);
                            (result, env)
                        }
                        Builtin::Eval => {
                            let (arg, rand) = rand.decons().expect("malformed eval arg");
                            let (arg, env) = eval(arg, env);
                            let arg = arg.expect("malformed eval arg");
                            if matches!(rand, List::Cons(_, _)) {
                                panic!("malformed eval arg");
                            }

                            eval(arg, env)
                        }
                        Builtin::Define => {
                            let (name, rand) = List::decons(rand).expect("malformed define name");
                            let name = match name {
                                Expression::Symbol(symbol) => symbol,
                                _ => panic!("malformed define name"),
                            };

                            let (value, rand) = rand.decons().expect("malformed define value");
                            let (value, env) = eval(value, env);
                            let value = value.expect("malformed define value");
                            if matches!(rand, List::Cons(_, _)) {
                                panic!("malformed define value");
                            }

                            let new_env = Environment::cons(name, value, env);
                            (None, new_env)
                        }
                    },
                    Expression::Closure(Closure {
                        params,
                        body,
                        env: closure_env,
                    }) => {
                        let (new_env, env) = call_env(params, rand, closure_env, env, true);
                        let (result, _) = eval(*body, new_env);
                        (result, env)
                    }
                    Expression::Macro(Closure {
                        params,
                        body,
                        env: closure_env,
                    }) => {
                        let (new_env, env) = call_env(params, rand, closure_env, env, false);
                        let (result, _) = eval(*body, new_env);
                        let result = result.expect("malformed call macro");
                        eval(result, env)
                    }
                    _ => panic!("malformed rator"),
                }
            }
            List::Nil => (Some(Expression::List(List::Nil)), env),
        },
        _ => (Some(expr), env),
    }
}

fn let_env(
    varlist: List,
    new_env: Environment,
    caller_env: Environment,
) -> (Environment, Environment) {
    if let List::Cons(head, tail) = varlist {
        let (head, tail) = (*head, *tail);
        let head = match head {
            Expression::List(list) => list,
            _ => panic!("malformed let varlist"),
        };

        let (name, head) = List::decons(head).expect("malformed let varlist name");
        let name = match name {
            Expression::Symbol(symbol) => symbol,
            _ => panic!("malformed let varlist"),
        };

        let (value, head) = head.decons().expect("malformed let varlist value");
        let (value, caller_env) = eval(value, caller_env);
        let value = value.expect("malformed let varlist value");
        if matches!(head, List::Cons(_, _)) {
            panic!("malformed let varlist value");
        }

        let new_env = Environment::cons(name, value, new_env);
        let_env(tail, new_env, caller_env)
    } else {
        (new_env, caller_env)
    }
}

fn call_env(
    params: List,
    args: List,
    new_env: Environment,
    caller_env: Environment,
    eval_args: bool,
) -> (Environment, Environment) {
    match (params, args) {
        (List::Nil, List::Nil) => (new_env, caller_env),
        (List::Cons(param, params), List::Cons(arg, args)) => {
            let (param, params) = (*param, *params);
            let (arg, args) = (*arg, *args);
            let param = match param {
                Expression::Symbol(symbol) => symbol,
                _ => panic!("malformed call args"),
            };
            let (arg, caller_env) = if eval_args {
                let (arg, caller_env) = eval(arg, caller_env);
                let arg = arg.expect("malformed call args");
                (arg, caller_env)
            } else {
                (arg, caller_env)
            };

            let new_env = Environment::cons(param, arg, new_env);

            call_env(params, args, new_env, caller_env, eval_args)
        }
        _ => panic!("malformed call args"),
    }
}
