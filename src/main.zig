const std = @import("std");
const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();
const stdin = std.io.getStdIn().reader();

const parse = @import("parse.zig");
const interpret = @import("interpret.zig");
const library = @import("library.zig");

pub fn main() !void {
    var main_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = main_allocator.deinit();
    const allocator = &main_allocator.allocator;

    var core = try library.initCore(allocator);
    defer core.deinit();
    var interpreter = interpret.Interpreter.init(allocator, &core);
    defer interpreter.deinit();

    var args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len > 1) {
        for (args[1..]) |path| {
            var file = try std.fs.cwd().openFile(path, .{});
            defer file.close();
            const len = try file.getEndPos();
            var source = std.ArrayList(u8).init(allocator);
            defer source.deinit();
            try file.reader().readAllArrayList(&source, len);
            try evalSource(allocator, &interpreter, source.items, false);
        }
    } else {
        try repl(allocator, &core);
    }
}

pub fn repl(allocator: *std.mem.Allocator, interpreter: *interpret.Interpreter) !void {
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
        try evalSource(allocator, interpreter, source.items, true);
    }
}

pub fn evalSource(allocator: *std.mem.Allocator, interpreter: *interpret.Interpreter, source: []const u8, in_repl: bool) !void {
    var tokenizer = parse.Tokenizer.init(source);
    var tokens = std.ArrayList(parse.Token).init(allocator);
    defer tokens.deinit();
    while (try tokenizer.next()) |token| try tokens.append(token);

    var parser = parse.Parser.init(interpreter, source, tokens.items);
    while (try parser.next()) |expr| {
        if (interpreter.eval(expr)) |result| {
            if (in_repl) try stdout.print("{}\n", .{result});
        } else |err| {
            try stderr.print("{}\n", .{err});
        }
    }
}
