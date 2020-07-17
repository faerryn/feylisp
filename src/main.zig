const std = @import("std");
const Lexer = @import("lex.zig").Lexer;
const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();
const stdin = std.io.getStdIn().reader();

pub fn main() anyerror!void {
    var source = std.ArrayList(u8).init(std.heap.page_allocator);
    defer source.deinit();
    var source_index: usize = 0;

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

        const line = source.items[source_index..];
        source_index = source.items.len;
        var lexer = Lexer.init(line);
        while (lexer.next()) |token| {
            try stdout.print(" {}: {}\n", .{ token.id, line[token.start..token.end] });
        }
    }
}
