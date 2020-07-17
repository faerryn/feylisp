const std = @import("std");
const Lexer = @import("lex.zig").Lexer;
const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();

pub fn main() anyerror!void {
    var line = std.ArrayList(u8).init(std.heap.page_allocator);
    defer line.deinit();

    while (true) {
        try stdout.print(" (fey lisp) ", .{});

        line.shrink(0); // clear the line
        stdin.readUntilDelimiterArrayList(&line, '\n', std.math.maxInt(usize)) catch |err| switch (err) {
            error.EndOfStream => break,
            else => |e| return e,
        };
        var lexer = Lexer.init(line.items);
        while (lexer.next()) |token| {
            try stdout.print(" {}: {}\n", .{ token.id, line.items[token.start..token.end] });
        }
    }
}
