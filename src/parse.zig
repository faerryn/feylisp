const std = @import("std");
const interpret = @import("interpret.zig");
const LispExpr = interpret.LispExpr;
const LispInterpreter = interpret.LispInterpreter;

pub const Token = struct {
    id: Id,
    start: usize,
    end: usize,

    pub const Id = enum {
        identifier,
        string_literal,
        integer_literal,
        float_literal,
        line_comment,
        open_paren,
        close_paren,
    };
};

pub const LispTokenizer = struct {
    source: []const u8,
    index: usize,

    pub fn init(source: []const u8) LispTokenizer {
        return LispTokenizer{
            .source = source,
            .index = 0,
        };
    }

    const State = enum {
        start,
        identifier,
        string_literal,
        unclosed_string_literal,
        unclosed_string_literal_backslash,
        integer_literal,
        float_fraction,
        sign,
        period,
        line_comment,
        open_paren,
        close_paren,
    };

    pub fn next(self: *LispTokenizer) !?Token {
        const start_index = self.index;
        var state = State.start;
        var result = Token{
            .id = undefined,
            .start = start_index,
            .end = undefined,
        };
        while (self.index < self.source.len) : (self.index += 1) {
            const c = self.source[self.index];
            switch (state) {
                .start => switch (c) {
                    ' ', '\t', '\r', '\n' => result.start = self.index + 1,
                    '"' => state = .unclosed_string_literal,
                    '(' => {
                        state = .open_paren;
                        result.id = .open_paren;
                        self.index += 1;
                        break;
                    },
                    ')' => {
                        state = .close_paren;
                        result.id = .close_paren;
                        self.index += 1;
                        break;
                    },
                    '0'...'9' => {
                        state = .integer_literal;
                        result.id = .integer_literal;
                    },
                    '+', '-' => state = .sign,
                    '.' => state = .period,
                    ';' => {
                        state = .line_comment;
                        result.id = .line_comment;
                    },
                    else => {
                        state = .identifier;
                        result.id = .identifier;
                    },
                },
                .string_literal => break,
                .unclosed_string_literal => {
                    switch (c) {
                        '\\' => state = .unclosed_string_literal_backslash,
                        '"' => {
                            state = .string_literal;
                            result.id = .string_literal;
                        },
                        else => {},
                    }
                },
                .unclosed_string_literal_backslash => state = .unclosed_string_literal,
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
                else => unreachable,
            }
        }
        result.end = self.index;
        switch (state) {
            .start => return null,
            .unclosed_string_literal,
            .unclosed_string_literal_backslash,
            => return error.TokenizerUnclosedStringLiteral,
            else => return result,
        }
    }
};

pub const LispParser = struct {
    interpreter: *LispInterpreter,
    source: []const u8,
    tokens: []Token,
    index: usize,

    pub fn init(interpreter: *LispInterpreter, source: []const u8, tokens: []Token) LispParser {
        return LispParser{
            .interpreter = interpreter,
            .source = source,
            .tokens = tokens,
            .index = 0,
        };
    }

    pub fn next(self: *LispParser) anyerror!?LispExpr {
        if (self.index >= self.tokens.len) return null;
        const t = self.tokens[self.index];
        self.index += 1;
        switch (t.id) {
            .line_comment => return try self.next(),
            .identifier => {
                var list = try self.interpreter.allocator.create(std.ArrayList(u8));
                errdefer self.interpreter.allocator.destroy(list);
                list.* = std.ArrayList(u8).init(self.interpreter.allocator);
                errdefer list.deinit();
                try list.appendSlice(self.source[t.start..t.end]);
                return try self.interpreter.store(LispExpr{ .identifier = list });
            },
            .string_literal => {
                var list = try self.interpreter.allocator.create(std.ArrayList(u8));
                errdefer self.interpreter.allocator.destroy(list);
                list.* = try std.ArrayList(u8).initCapacity(self.interpreter.allocator, (t.end - t.start - 2) / 2);
                errdefer list.deinit();
                var backslash = false;
                for (self.source[t.start + 1 .. t.end - 1]) |string_c| {
                    if (backslash) {
                        switch (string_c) {
                            'n' => try list.append('\n'),
                            't' => try list.append('\t'),
                            else => try list.append(string_c),
                        }
                        backslash = false;
                    } else {
                        if (string_c == '\\') {
                            backslash = true;
                        } else {
                            try list.append(string_c);
                        }
                    }
                }
                return try self.interpreter.store(LispExpr{ .string = list });
            },
            .integer_literal, .float_literal => {
                const num = try std.fmt.parseInt(isize, self.source[t.start..t.end], 10);
                return LispExpr{ .number = num };
            },
            .open_paren => {
                var list = try self.interpreter.allocator.create(std.ArrayList(LispExpr));
                errdefer self.interpreter.allocator.destroy(list);
                list.* = std.ArrayList(LispExpr).init(self.interpreter.allocator);
                errdefer list.deinit();
                errdefer for (list.items) |branch| branch.deinit();
                while (self.tokens[self.index].id != .close_paren) {
                    if (try self.next()) |expr| {
                        try list.append(expr);
                    } else {
                        return error.ParserUnclosedParen;
                    }
                }
                self.index += 1;
                return try self.interpreter.store(LispExpr{ .list = list });
            },
            .close_paren => return error.ParserOverclosedParen,
        }
    }
};
