pub enum Lexeme {
    Open,
    Close,
    Number(i32),
    Symbol(String),
    Quote,
}

#[must_use]
pub fn lex(src: &str) -> Vec<Lexeme> {
    enum State {
        Start,
        Sign,
        Number,
        Symbol,
        Comment,
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
                sign = 0;
                mag = 0;

                match ch {
                    ' ' | '\t' | '\n' | '\r' => {
                        state = State::Start;
                    }
                    '\'' => {
                        result.push(Lexeme::Quote);
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
                        mag = 10 * mag + (ch as i32 - '0' as i32);
                        state = State::Number;
                    }
                    ';' => state = State::Comment,
                    _ => state = State::Symbol,
                }
            }
            State::Sign => match ch {
                ' ' | '\t' | '\n' | '\r' => {
                    result.push(Lexeme::Symbol(src[start_index..curr_index].to_string()));
                    state = State::Start;
                }
                '(' => {
                    result.push(Lexeme::Symbol(src[start_index..curr_index].to_string()));
                    result.push(Lexeme::Open);
                    state = State::Start;
                }
                ')' => {
                    result.push(Lexeme::Symbol(src[start_index..curr_index].to_string()));
                    result.push(Lexeme::Close);
                    state = State::Start;
                }
                '\'' => {
                    result.push(Lexeme::Symbol(src[start_index..curr_index].to_string()));
                    result.push(Lexeme::Quote);
                    state = State::Start;
                }
                '0'..='9' => {
                    mag = 10 * mag + (ch as i32 - '0' as i32);
                    state = State::Number;
                }
                ';' => {
                    result.push(Lexeme::Symbol(src[start_index..curr_index].to_string()));
                    result.push(Lexeme::Close);
                    state = State::Comment;
                }
                _ => state = State::Symbol,
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
                '\'' => {
                    result.push(Lexeme::Number(sign * mag));
                    result.push(Lexeme::Quote);
                    state = State::Start;
                }
                '0'..='9' => {
                    mag = 10 * mag + (ch as i32 - '0' as i32);
                    state = State::Number;
                }
                ';' => {
                    result.push(Lexeme::Number(sign * mag));
                    result.push(Lexeme::Quote);
                    state = State::Comment;
                }
                _ => state = State::Symbol,
            },
            State::Symbol => match ch {
                ' ' | '\t' | '\n' | '\r' => {
                    result.push(Lexeme::Symbol(src[start_index..curr_index].to_string()));
                    state = State::Start;
                }
                '(' => {
                    result.push(Lexeme::Symbol(src[start_index..curr_index].to_string()));
                    result.push(Lexeme::Open);
                    state = State::Start;
                }
                ')' => {
                    result.push(Lexeme::Symbol(src[start_index..curr_index].to_string()));
                    result.push(Lexeme::Close);
                    state = State::Start;
                }
                '\'' => {
                    result.push(Lexeme::Symbol(src[start_index..curr_index].to_string()));
                    result.push(Lexeme::Quote);
                    state = State::Start;
                }
                ';' => {
                    result.push(Lexeme::Number(sign * mag));
                    result.push(Lexeme::Symbol(src[start_index..curr_index].to_string()));
                    result.push(Lexeme::Quote);
                }
                _ => state = State::Symbol,
            },
            State::Comment => match ch {
                '\n' | '\r' => state = State::Start,
                _ => {}
            },
        }
    }

    match state {
        State::Start | State::Comment => {}
        State::Number => result.push(Lexeme::Number(sign * mag)),
        State::Symbol | State::Sign => result.push(Lexeme::Symbol(src[start_index..].to_string())),
    }

    result
}
