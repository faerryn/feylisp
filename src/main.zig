const std = @import("std");
const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();
const stdin = std.io.getStdIn().reader();

const parse = @import("parse.zig");
const interpret = @import("interpret.zig");
const library = @import("library.zig");

pub fn main() !void {
    var main_allocator = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer _ = main_allocator.deinit();
    const allocator = &main_allocator.allocator;

    var core = try library.initCore(allocator);
    defer core.deinit();
    var interpreter = interpret.Interpreter.init(allocator, &core);
    defer interpreter.deinit();

    repl_loop: while (true) {
        const PROMPT = " >> ";
        _ = try stdout.write(PROMPT);

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
                        _ = try stdout.write(" " ** PROMPT.len);
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
