#[derive(Debug)]
pub enum Lexeme {
    Open,
    Close,
    Number(i32),
    Symbol(String),
}

#[must_use]
pub fn lex(src: &str) -> Vec<Lexeme> {
    enum State {
        Start,
        Sign,
        Number,
        Symbol,
    }

    let mut result = vec![];
    let mut state = State::Start;
    let mut start_index = 0;

    let mut sign = 0;
    let mut mag = 0;

    for (curr_index, ch) in src.chars().enumerate() {
        match state {
            State::Start => {
                start_index = curr_index;
                match ch {
                    ' ' | '\t' | '\n' | '\r' => {
                        state = State::Start;
                    }
                    '(' => {
                        result.push(Lexeme::Open);
                        state = State::Start;
                    }
                    ')' => {
                        result.push(Lexeme::Close);
                        state = State::Start;
                    }
                    '+' => {
                        sign = 1;
                        state = State::Sign;
                    }
                    '-' => {
                        sign = -1;
                        state = State::Sign;
                    }
                    '0'..='9' => {
                        sign = 1;
                        mag += ch as i32 - '0' as i32;
                        state = State::Number;
                    }
                    _ => {
                        state = State::Symbol;
                    }
                }
            }
            State::Sign => match ch {
                ' ' | '\t' | '\n' | '\r' => {
                    result.push(Lexeme::Symbol(src[start_index..curr_index].to_owned()));
                    state = State::Start;
                }
                '(' => {
                    result.push(Lexeme::Symbol(src[start_index..curr_index].to_owned()));
                    result.push(Lexeme::Open);
                    state = State::Start;
                }
                ')' => {
                    result.push(Lexeme::Symbol(src[start_index..curr_index].to_owned()));
                    result.push(Lexeme::Close);
                    state = State::Start;
                }
                '0'..='9' => {
                    mag += ch as i32 - '0' as i32;
                    state = State::Number;
                }
                _ => {
                    state = State::Symbol;
                }
            },
            State::Number => match ch {
                ' ' | '\t' | '\n' | '\r' => {
                    result.push(Lexeme::Number(sign * mag));
                    state = State::Start;
                }
                '(' => {
                    result.push(Lexeme::Number(sign * mag));
                    result.push(Lexeme::Open);
                    state = State::Start;
                }
                ')' => {
                    result.push(Lexeme::Number(sign * mag));
                    result.push(Lexeme::Close);
                    state = State::Start;
                }
                '0'..='9' => {
                    state = State::Number;
                }
                _ => {
                    state = State::Symbol;
                }
            },
            State::Symbol => match ch {
                ' ' | '\t' | '\n' | '\r' => {
                    result.push(Lexeme::Symbol(src[start_index..curr_index].to_owned()));
                    state = State::Start;
                }
                '(' => {
                    result.push(Lexeme::Symbol(src[start_index..curr_index].to_owned()));
                    result.push(Lexeme::Open);
                    state = State::Start;
                }
                ')' => {
                    result.push(Lexeme::Symbol(src[start_index..curr_index].to_owned()));
                    result.push(Lexeme::Close);
                    state = State::Start;
                }
                _ => {
                    state = State::Symbol;
                }
            },
        }
    }

    match state {
        State::Start => {}
        State::Number => {
            result.push(Lexeme::Number(sign * mag));
        }
        State::Symbol | State::Sign => {
            result.push(Lexeme::Symbol(src[start_index..].to_owned()));
        }
    }

    result
}
