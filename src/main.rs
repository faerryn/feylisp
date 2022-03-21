use feylisp::{eval_src, repl, expr::Environment};

fn main() {
    let mut env = Environment::default();
    for file in std::env::args().skip(1) {
        if let Ok(src) = std::fs::read_to_string(file) {
            match eval_src(&src, env) {
                Ok((exprs, new_env)) => {
                    for expr in exprs {
                        println!("{}", expr);
                    }
                    env = new_env;
                }
                Err(_err) => todo!(),
            }
        }
    }

    match repl(env) {
        Ok(env) => {
            println!("[{}]", env);
        }
        Err(_err) => todo!(),
    }
}
