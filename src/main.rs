use feylisp::{eval::Environment, eval_src, repl, standard_env};

fn main() {
    let mut env = standard_env();
    let mut args = std::env::args().peekable();

    let executable = args.next().unwrap_or_else(|| "feylisp".to_owned());

    let mut want_repl = args.peek().is_none();

    for file in args {
        if file == "-" {
            want_repl = true;
        } else if let Ok(src) = std::fs::read_to_string(&file) {
            match eval_src(&src, Environment::clone(&env)) {
                Ok((exprs, new_env)) => {
                    for expr in exprs {
                        println!("{}", expr);
                    }
                    env = new_env;
                }
                Err(err) => eprintln!("{}", err),
            }
        } else {
            eprintln!("{}: {}: No such file or directory", executable, file);
        }
    }

    if want_repl {
        match repl(env) {
            Err(err) => eprintln!("{}", err),
            Ok(_) => println!(),
        }
    }
}
