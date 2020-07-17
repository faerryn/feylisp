const std = @import("std");

pub const Tokenizer = struct {
    buffer: []const u8,
    index: usize,

    pub const Token = struct {
        id: Id,
        start: usize,
        end: usize,

        pub const Id = enum {
            invalid,
            identifier,
            string_literal,
            integer_literal,
            float_literal,
            line_comment,
            open_paren,
            close_paren,
            quote,
        };
    };

    pub fn init(buffer: []const u8) Tokenizer {
        return Tokenizer{
            .buffer = buffer,
            .index = 0,
        };
    }

    const State = enum {
        start,
        integer_literal,
        float_fraction,
        string_literal,
        string_literal_backslash,
        identifier,
        line_comment,
        sign,
        period,
    };

    pub fn next(self: *Tokenizer) ?Token {
        const start_index = self.index;
        var state = State.start;
        var eof = true;
        var result = Token{
            .id = .invalid,
            .start = start_index,
            .end = undefined,
        };
        while (self.index < self.buffer.len) : (self.index += 1) {
            const c = self.buffer[self.index];
            switch (state) {
                .start => switch (c) {
                    ' ', '\t', '\r', '\n' => {
                        result.start = self.index + 1;
                    },
                    '"' => {
                        eof = false;
                        state = .string_literal;
                    },
                    '(' => {
                        eof = false;
                        result.id = .open_paren;
                        self.index += 1;
                        break;
                    },
                    ')' => {
                        eof = false;
                        result.id = .close_paren;
                        self.index += 1;
                        break;
                    },
                    '\'' => {
                        eof = false;
                        result.id = .quote;
                        self.index += 1;
                        break;
                    },
                    '0'...'9' => {
                        eof = false;
                        state = .integer_literal;
                        result.id = .integer_literal;
                    },
                    '+', '-' => {
                        eof = false;
                        state = .sign;
                    },
                    '.' => {
                        eof = false;
                        state = .period;
                    },
                    ';' => {
                        eof = false;
                        state = .line_comment;
                        result.id = .line_comment;
                    },
                    else => {
                        eof = false;
                        state = .identifier;
                        result.id = .identifier;
                    },
                },
                .string_literal => {
                    switch (c) {
                        '\\' => {
                            state = .string_literal_backslash;
                        },
                        '"' => {
                            result.id = .string_literal;
                            self.index += 1;
                            break;
                        },
                        else => {},
                    }
                },
                .string_literal_backslash => {
                    state = .string_literal;
                },
                .integer_literal => {
                    switch (c) {
                        '0'...'9' => {},
                        '.' => {
                            state = .float_fraction;
                            result.id = .float_literal;
                        },
                        ' ', '\t', '\r', '\n', '(', ')', ';' => break,
                        else => {
                            state = .identifier;
                            result.id = .identifier;
                        },
                    }
                },
                .float_fraction => {
                    switch (c) {
                        '0'...'9' => {},
                        ' ', '\t', '\r', '\n', '(', ')', ';' => break,
                        else => {
                            state = .identifier;
                            result.id = .identifier;
                        },
                    }
                },
                .sign => {
                    switch (c) {
                        '0'...'9' => {
                            state = .integer_literal;
                            result.id = .integer_literal;
                        },
                        '.' => {
                            state = .float_fraction;
                            result.id = .float_literal;
                        },
                        ' ', '\t', '\r', '\n', '(', ')', ';' => {
                            result.id = .identifier;
                            break;
                        },
                        else => {
                            state = .identifier;
                            result.id = .identifier;
                        },
                    }
                },
                .period => {
                    switch (c) {
                        '0'...'9' => {
                            state = .float_fraction;
                            result.id = .float_literal;
                        },
                        ' ', '\t', '\r', '\n', '(', ')', ';' => {
                            result.id = .identifier;
                            break;
                        },
                        else => {
                            state = .identifier;
                            result.id = .identifier;
                        },
                    }
                },
                .line_comment => {
                    switch (c) {
                        '\r', '\n' => break,
                        else => {},
                    }
                },
                .identifier => {
                    switch (c) {
                        ' ', '\t', '\r', '\n', '(', ')', ';' => break,
                        else => {},
                    }
                },
            }
        }
        if (eof) {
            return null;
        } else {
            result.end = self.index;
            return result;
        }
    }
};
