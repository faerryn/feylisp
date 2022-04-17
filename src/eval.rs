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

#[must_use]
pub fn eval(
    expr: Rc<Expression>,
    env: Rc<Environment>,
) -> (Option<Rc<Expression>>, Rc<Environment>) {
    match &*expr {
        Expression::Symbol(ident) => (
            Some(
                env.get(ident)
                    .unwrap_or_else(|| panic!("free variable `{}'", ident)),
            ),
            env,
        ),
        Expression::List(list) => match &**list {
            List::Cons(rator, rand) => {
                let (rator, env) = eval(Rc::clone(rator), env);
                let rator = match rator {
                    Some(rator) => rator,
                    None => panic!("malformed rator"),
                };

                match &*rator {
                    Expression::Builtin(builtin) => match builtin {
                        Builtin::Quote => {
                            let (arg, rand) = rand.decons().expect("malformed quote arg");
                            if matches!(*rand, List::Cons(_, _)) {
                                panic!("malformed quote arg");
                            }
                            (Some(arg), env)
                        }
                        Builtin::Lambda => {
                            let (params, rand) = rand.decons().expect("malformed lambda params");
                            let (body, rand) = rand.decons().expect("malformed lambda body");
                            if matches!(*rand, List::Cons(_, _)) {
                                panic!("malformed lambda body");
                            }

                            (
                                Some(Rc::new(Expression::Closure(Closure {
                                    params,
                                    body,
                                    env: Rc::clone(&env),
                                }))),
                                env,
                            )
                        }
                        Builtin::If => {
                            let (cond, rand) = rand.decons().expect("malformed if cond");
                            let (cond, env) = eval(cond, env);
                            let cond = cond.expect("malformed if cond");

                            let (when, rand) = rand.decons().expect("malformed if when");

                            let (unless, rand) = rand.decons().expect("malformed if unless");

                            if matches!(*rand, List::Cons(_, _)) {
                                panic!("malformed if unless");
                            }

                            if matches!(*cond, Expression::Bool(false)) {
                                eval(unless, env)
                            } else {
                                eval(when, env)
                            }
                        }
                        Builtin::TestMonop(op) => {
                            let (arg, rand) = rand.decons().expect("malformed testmonop arg");
                            if matches!(*rand, List::Cons(_, _)) {
                                panic!("malformed testmonop arg");
                            }

                            let (arg, env) = eval(arg, env);
                            let arg = arg.expect("malformed testmonop arg");
                            (
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
                            )
                        }
                        Builtin::NumBinop(op) => {
                            let (rhs, rand) = rand.decons().expect("malformed numbinop rhs");
                            let (rhs, env) = eval(rhs, env);
                            let rhs = rhs.expect("malformed numbinop rhs");
                            let rhs = match *rhs {
                                Expression::Number(number) => number,
                                _ => panic!("malformed numbinop rhs"),
                            };

                            let (lhs, rand) = rand.decons().expect("malformed numbinop lhs");
                            let (lhs, env) = eval(lhs, env);
                            let lhs = lhs.expect("malformed numbinop lhs");
                            let lhs = match *lhs {
                                Expression::Number(number) => number,
                                _ => panic!("malformed numbinop lhs"),
                            };
                            if matches!(*rand, List::Cons(_, _)) {
                                panic!("malformed numbinop lhs")
                            }

                            (
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
                            )
                        }
                        Builtin::ListMonop(op) => {
                            let (arg, rand) = rand.decons().expect("malformed listmonop arg");
                            if matches!(*rand, List::Cons(_, _)) {
                                panic!("malformed listmonop arg");
                            }

                            let (arg, env) = eval(arg, env);
                            let arg = arg.expect("malformed listmonop arg");
                            let arg = match &*arg {
                                Expression::List(list) => list,
                                _ => panic!("malformed listmonop arg"),
                            };

                            let (head, tail) = arg.decons().expect("malformed listmonop arg");
                            (
                                Some(match op {
                                    ListMonop::Head => head,
                                    ListMonop::Tail => Rc::new(Expression::List(tail)),
                                }),
                                env,
                            )
                        }
                        Builtin::Cons => {
                            let (head, rand) = rand.decons().expect("malformed cons head");
                            let (head, env) = eval(head, env);
                            let head = head.expect("malformed cons head");

                            let (tail, rand) = rand.decons().expect("malformed cons tail");
                            let (tail, env) = eval(tail, env);
                            let tail = tail.expect("malformed cons tail");
                            let tail = match &*tail {
                                Expression::List(list) => list,
                                _ => panic!("malformed cons tail"),
                            };
                            if matches!(*rand, List::Cons(_, _)) {
                                panic!("malformed cons tail");
                            }

                            (
                                Some(Rc::new(Expression::List(Rc::new(List::Cons(
                                    head,
                                    Rc::clone(tail),
                                ))))),
                                env,
                            )
                        }
                        Builtin::Let => {
                            let (varlist, rand) = rand.decons().expect("malformed let varlist");
                            let varlist = match &*varlist {
                                Expression::List(list) => list,
                                _ => panic!("malformed let varlist"),
                            };

                            let (body, rand) = rand.decons().expect("malformed let body");
                            if matches!(*rand, List::Cons(_, _)) {
                                panic!("malformed let body");
                            }

                            let (new_env, env) = let_env(Rc::clone(varlist), Rc::clone(&env), env);
                            let (result, _) = eval(body, new_env);
                            (result, env)
                        }
                        Builtin::Eval => {
                            let (arg, rand) = rand.decons().expect("malformed eval arg");
                            let (arg, env) = eval(arg, env);
                            let arg = arg.expect("malformed eval arg");
                            if matches!(*rand, List::Cons(_, _)) {
                                panic!("malformed eval arg");
                            }

                            eval(arg, env)
                        }
                        Builtin::Define => {
                            let (name, rand) = rand.decons().expect("malformed define name");
                            let name = match &*name {
                                Expression::Symbol(symbol) => symbol,
                                _ => panic!("malformed define name"),
                            };

                            let (value, rand) = rand.decons().expect("malformed define value");
                            let (value, env) = eval(value, env);
                            let value = value.expect("malformed define value");
                            if matches!(*rand, List::Cons(_, _)) {
                                panic!("malformed define value");
                            }

                            let new_env = Environment::Cons(Rc::clone(name), value, env);
                            (None, Rc::new(new_env))
                        }
                    },
                    Expression::Closure(Closure {
                        params,
                        body,
                        env: closure_env,
                    }) => {
                        let params = if let Expression::List(list) = &**params {
                            list
                        } else {
                            panic!("malformed parameters")
                        };
                        let (new_env, env) = call_env(
                            Rc::clone(params),
                            Rc::clone(rand),
                            Rc::clone(closure_env),
                            env,
                        );
                        let (result, _) = eval(Rc::clone(body), new_env);
                        (result, env)
                    }
                    _ => panic!("malformed rator"),
                }
            }
            List::Nil => (Some(expr), env),
        },
        _ => (Some(expr), env),
    }
}

fn let_env(
    varlist: Rc<List>,
    new_env: Rc<Environment>,
    caller_env: Rc<Environment>,
) -> (Rc<Environment>, Rc<Environment>) {
    if let List::Cons(head, tail) = &*varlist {
        let head = match &**head {
            Expression::List(list) => list,
            _ => panic!("malformed let varlist"),
        };

        let (name, head) = head.decons().expect("malformed let varlist name");
        let name = match &*name {
            Expression::Symbol(symbol) => symbol,
            _ => panic!("malformed let varlist"),
        };

        let (value, head) = head.decons().expect("malformed let varlist value");
        let (value, caller_env) = eval(value, caller_env);
        let value = value.expect("malformed let varlist value");
        if matches!(*head, List::Cons(_, _)) {
            panic!("malformed let varlist value");
        }

        let new_env = Environment::Cons(Rc::clone(name), value, new_env);
        let_env(Rc::clone(tail), Rc::new(new_env), caller_env)
    } else {
        (new_env, caller_env)
    }
}

fn call_env(
    params: Rc<List>,
    args: Rc<List>,
    new_env: Rc<Environment>,
    caller_env: Rc<Environment>,
) -> (Rc<Environment>, Rc<Environment>) {
    match (&*params, &*args) {
        (List::Nil, List::Nil) => (new_env, caller_env),
        (List::Cons(param, params), List::Cons(arg, args)) => {
            let param = match &**param {
                Expression::Symbol(symbol) => symbol,
                _ => panic!("malformed call args"),
            };
            let (arg, caller_env) = eval(Rc::clone(arg), caller_env);
            let arg = arg.expect("malformed call args");

            let new_env = Environment::Cons(Rc::clone(param), arg, new_env);

            call_env(
                Rc::clone(params),
                Rc::clone(args),
                Rc::new(new_env),
                caller_env,
            )
        }
        _ => panic!("malformed call args"),
    }
}
