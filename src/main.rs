use feylisp::{eval::Environment, eval_src, repl};

fn main() {
    let mut env = Environment::standard_env();
    for file in std::env::args().skip(1) {
        if let Ok(src) = std::fs::read_to_string(file) {
            let (exprs, new_env) = eval_src(&src, env);
            for expr in exprs {
                println!("{}", expr);
            }
            env = new_env;
        }
    }

    let env = repl(env);
    println!("[{}]", env);
}
