use std::rc::Rc;

use feylisp::{eval_src, repl, standard_env};

fn main() {
    let mut env = standard_env();
    for file in std::env::args().skip(1) {
        if let Ok(src) = std::fs::read_to_string(file) {
            match eval_src(&src, Rc::clone(&env)) {
                Ok((exprs, new_env)) => {
                    for expr in exprs {
                        println!("{}", expr);
                    }
                    env = new_env;
                }
                Err(err) => eprintln!("{}", err),
            }
        }
    }

    match repl(env) {
        Ok(_) => println!(),
        Err(err) => eprintln!("{}", err),
    }
}
