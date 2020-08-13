const std = @import("std");
const interpret = @import("interpret.zig");

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

pub const Tokenizer = struct {
    source: []const u8,
    index: usize,

    pub fn init(source: []const u8) Tokenizer {
        return Tokenizer{
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

    pub fn next(self: *Tokenizer) !?Token {
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
                .unclosed_string_literal => {
                    switch (c) {
                        '\\' => state = .unclosed_string_literal_backslash,
                        '"' => {
                            state = .string_literal;
                            result.id = .string_literal;
                            self.index += 1;
                            break;
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

pub const Parser = struct {
    interpreter: *interpret.Interpreter,
    source: []const u8,
    tokens: []Token,
    index: usize,

    pub fn init(interpreter: *interpret.Interpreter, source: []const u8, tokens: []Token) Parser {
        return Parser{
            .interpreter = interpreter,
            .source = source,
            .tokens = tokens,
            .index = 0,
        };
    }

    pub fn next(self: *Parser) anyerror!?*interpret.Expr {
        if (self.index >= self.tokens.len) return null;
        const t = self.tokens[self.index];
        self.index += 1;
        const expr = switch (t.id) {
            .line_comment => return try self.next(),
            .identifier, .string_literal => blk: {
                var list = std.ArrayList(u8).init(self.interpreter.allocator);
                errdefer list.deinit();
                try list.appendSlice(self.source[t.start..t.end]);
                break :blk switch (t.id) {
                    .identifier => interpret.Expr{ .identifier = list },
                    .string_literal => interpret.Expr{ .string = list },
                    else => unreachable,
                };
            },
            .integer_literal => blk: {
                const i = try std.fmt.parseInt(i64, self.source[t.start..t.end], 10);
                break :blk interpret.Expr{ .integer = i };
            },
            .float_literal => blk: {
                const f = try std.fmt.parseFloat(f64, self.source[t.start..t.end]);
                break :blk interpret.Expr{ .float = f };
            },
            .open_paren => blk: {
                var list = std.ArrayList(*interpret.Expr).init(self.interpreter.allocator);
                errdefer {
                    defer list.deinit();
                    for (list.items) |branch| branch.deinit();
                }
                while (self.tokens[self.index].id != .close_paren) {
                    if (try self.next()) |expr| {
                        try list.append(expr);
                    } else {
                        return error.ParserUnclosedParen;
                    }
                }
                self.index += 1;
                break :blk interpret.Expr{ .list = list };
            },
            .close_paren => return error.ParserOverclosedParen,
        };
        try self.interpreter.mem.append(expr);
        return &self.interpreter.mem.items[self.interpreter.mem.items.len - 1];
    }
};
