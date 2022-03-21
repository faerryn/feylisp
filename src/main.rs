use feylisp::{eval_src, repl, Environment};

fn main() {
    let mut env = Environment::default();
    for file in std::env::args().skip(1) {
        if let Ok(src) = std::fs::read_to_string(file) {
            if let Ok((exprs, new_env)) = eval_src(&src, env) {
                for expr in exprs {
                    println!("{}", expr);
                }
                env = new_env;
            } else {
                todo!();
            }
        }
    }

    if let Ok(env) = repl(env) {
        println!("[{}]", env);
    }
}
