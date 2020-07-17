const std = @import("std");
const Lexer = @import("lex.zig").Lexer;
const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();

pub fn main() anyerror!void {
    var source = std.ArrayList(u8).init(std.heap.page_allocator);
    defer source.deinit();
    var source_index: usize = 0;

    try stdout.print(" (fey lisp) ", .{});

    repl_loop: while (true) {
        var parens: usize = 0;
        var newline = false;
        while (parens > 0 or !newline) {
            const c = stdin.readByte() catch |err| switch (err) {
                error.EndOfStream => break :repl_loop,
                else => return err,
            };
            switch (c) {
                '(' => parens += 1,
                ')' => parens -= 1,
                '\n' => {
                    newline = true;
                    if (parens > 0) {
                        try stdout.print("            ", .{});
                    } else {
                        try stdout.print(" (fey lisp) ", .{});
                    }
                },
                else => {},
            }
            try source.append(c);
        }

        const line = source.items[source_index..];
        source_index = source.items.len;
        var lexer = Lexer.init(line);
        while (lexer.next()) |token| {
            try stdout.print(" {}: {}\n", .{ token.id, line[token.start..token.end] });
        }
    }
}
