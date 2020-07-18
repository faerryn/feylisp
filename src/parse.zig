const std = @import("std");
const lisp = @import("lisp.zig");

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
        quote,
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
        quote,
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
                    '\'' => {
                        state = .quote;
                        result.id = .quote;
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
    allocator: *std.mem.Allocator,
    source: []const u8,
    tokens: []Token,
    index: usize,

    pub fn init(allocator: *std.mem.Allocator, source: []const u8, tokens: []Token) Parser {
        return Parser{
            .allocator = allocator,
            .source = source,
            .tokens = tokens,
            .index = 0,
        };
    }

    pub fn next(self: *Parser) anyerror!?lisp.Expr {
        if (self.index >= self.tokens.len) return null;
        const t = self.tokens[self.index];
        self.index += 1;
        switch (t.id) {
            .identifier => {
                var list = std.ArrayList(u8).init(self.allocator);
                errdefer list.deinit();
                try list.appendSlice(self.source[t.start..t.end]);
                return lisp.Expr{ .identifier = list };
            },
            .string_literal => {
                var list = std.ArrayList(u8).init(self.allocator);
                errdefer list.deinit();
                try list.appendSlice(self.source[t.start..t.end]);
                return lisp.Expr{ .string = list };
            },
            .integer_literal => {
                const i = try std.fmt.parseInt(i64, self.source[t.start..t.end], 10);
                return lisp.Expr{ .integer = i };
            },
            .float_literal => {
                const f = try std.fmt.parseFloat(f64, self.source[t.start..t.end]);
                return lisp.Expr{ .float = f };
            },
            .line_comment => return try self.next(),
            .open_paren => {
                var list = std.ArrayList(lisp.Expr).init(self.allocator);
                errdefer {
                    defer list.deinit();
                    for (list.items) |sublist| sublist.deinit();
                }
                while (self.tokens[self.index].id != .close_paren) {
                    if (try self.next()) |expr| {
                        try list.append(expr);
                    } else {
                        return error.ParserUnclosedParen;
                    }
                }
                self.index += 1;
                return lisp.Expr{ .list = list };
            },
            .close_paren => return error.ParserOverclosedParen,
            .quote => {
                if (self.tokens[self.index].id != .open_paren) return error.ParserInvalidQuote;
                if (try self.next()) |list| {
                    return lisp.Expr{ .quoted_list = list.list };
                } else {
                    return error.ParserInvalidQuote;
                }
            },
        }
    }
};
