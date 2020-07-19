const std = @import("std");
const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();
const stdin = std.io.getStdIn().reader();
const parse = @import("parse.zig");
const lisp = @import("lisp.zig");

pub fn main() anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);

    var source = std.ArrayList(u8).init(&arena.allocator);
    defer source.deinit();
    var source_index: usize = 0;

    var tokens = std.ArrayList(parse.Token).init(&arena.allocator);
    defer tokens.deinit();
    var tokens_index: usize = 0;

    var exprs = std.ArrayList(lisp.Expr).init(&arena.allocator);
    defer {
        defer exprs.deinit();
        for (exprs.items) |expr| expr.deinit();
    }
    var exprs_index: usize = 0;

    var lisp_engine = lisp.Lisp.init(&arena.allocator, null);
    defer lisp_engine.deinit();
    try lisp_engine.letIdentifier("+", lisp.Expr{ .native_func = @ptrToInt(add) });

    repl_loop: while (true) {
        try stdout.print(" (fey lisp) ", .{});

        var parens: usize = 0;
        while (true) {
            const c = stdin.readByte() catch |err| switch (err) {
                error.EndOfStream => break :repl_loop,
                else => return err,
            };
            try source.append(c);
            switch (c) {
                '(' => parens += 1,
                ')' => {
                    if (parens > 0) {
                        parens -= 1;
                    } else {
                        try stderr.print("{}\n", .{error.REPLOverclosedParen});
                        while (stdin.readByte() catch |err| switch (err) {
                            error.EndOfStream => break :repl_loop,
                            else => return err,
                        } != '\n') {}
                        continue :repl_loop;
                    }
                },
                '\n' => {
                    if (parens > 0) {
                        var i: usize = 0;
                        while (i < parens + 3) : (i += 1) {
                            try stdout.print("    ", .{});
                        }
                    } else {
                        break;
                    }
                },
                else => {},
            }
        }

        const line_source = source.items[source_index..];

        var tokenizer = parse.Tokenizer.init(line_source);
        while (try tokenizer.next()) |token| try tokens.append(token);

        const line_tokens = tokens.items[tokens_index..];

        var parser = parse.Parser.init(&arena.allocator, line_source, line_tokens);
        while (try parser.next()) |real_expr| {
            var expr = real_expr;
            try exprs.append(expr);
            const result = lisp_engine.eval(&expr);
            try stdout.print(" {}\n", .{result});
        }

        source_index = source.items.len;
        tokens_index = tokens.items.len;
        exprs_index = exprs.items.len;
    }
}

fn add(lisp_engine: *lisp.Lisp, exprs: []lisp.Expr) anyerror!lisp.Expr {
    const lhs = switch (try lisp_engine.eval(&exprs[0])) {
        .integer => |integer| integer,
        else => return error.AddNotInteger,
    };
    const rhs = switch (try lisp_engine.eval(&exprs[1])) {
        .integer => |integer| integer,
        else => return error.AddNotInteger,
    };
    return lisp.Expr{ .integer = lhs + rhs };
}
