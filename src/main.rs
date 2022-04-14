use feylisp::{eval_src, repl, standard_env};

fn main() {
    let mut env = standard_env();
    for file in std::env::args().skip(1) {
        if let Ok(src) = std::fs::read_to_string(file) {
            let (exprs, new_env) = eval_src(&src, env);
            for expr in exprs {
                println!("{}", expr);
            }
            env = new_env;
        }
    }

    _ = repl(env);
    println!();
}
