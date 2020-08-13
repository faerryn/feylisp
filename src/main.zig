const std = @import("std");
const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();
const stdin = std.io.getStdIn().reader();

const parse = @import("parse.zig");
const interpret = @import("interpret.zig");

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = &gpa.allocator;

    var core = interpret.Interpreter.init(allocator, null);
    defer core.deinit();
    try core.scope.put("+", try core.store(interpret.Expr{ .native_func = @ptrToInt(add) }));

    var interpreter = interpret.Interpreter.init(allocator, &core);
    defer interpreter.deinit();

    repl_loop: while (true) {
        try stdout.print(" (fey lisp) ", .{});

        var source = std.ArrayList(u8).init(allocator);
        defer source.deinit();

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
                        try stdout.print(" " ** 12, .{});
                    } else {
                        break;
                    }
                },
                else => {},
            }
        }

        var tokenizer = parse.Tokenizer.init(source.items);
        var tokens = std.ArrayList(parse.Token).init(allocator);
        defer tokens.deinit();
        while (try tokenizer.next()) |token| try tokens.append(token);

        var parser = parse.Parser.init(&interpreter, source.items, tokens.items);
        while (try parser.next()) |expr| {
            const result = interpreter.eval(expr);
            try stdout.print("{}\n", .{result});
        }
    }
}

fn add(interpreter: *interpret.Interpreter, args: []*interpret.Expr) !*interpret.Expr {
    var acc: f64 = 0.0;
    for (args) |arg| {
        switch (arg.*) {
            .number => |num| acc += num,
            else => return error.AddNotANumber,
        }
    }
    return try interpreter.store(interpret.Expr{ .number = acc });
}
