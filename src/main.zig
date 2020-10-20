const std = @import("std");

const parse = @import("parse.zig");
const LispToken = parse.LispToken;
const LispTokenizer = parse.LispTokenizer;
const LispParser = parse.LispParser;

const interpret = @import("interpret.zig");
const LispExpr = interpret.LispExpr;
const LispInterpreter = interpret.LispInterpreter;

const library = @import("library.zig");

pub fn main() !void {
    var main_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = main_allocator.deinit();
    const allocator = &main_allocator.allocator;

    var core = try library.initCore(allocator);
    defer core.deinit();
    var interpreter = LispInterpreter.init(allocator, &core);
    defer interpreter.deinit();

    var args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    for (args[1..]) |path| try run_file(allocator, &interpreter, path);

    try repl(allocator, &interpreter);
}

fn run_file(allocator: *std.mem.Allocator, interpreter: *LispInterpreter, path: []const u8) !void {
    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();

    var source = blk: {
        var file = try std.fs.cwd().openFile(path, .{ .read = true });
        defer file.close();

        var source = std.ArrayList(u8).init(allocator);

        const len = try file.getEndPos();
        try file.reader().readAllArrayList(&source, len);

        break :blk source;
    };
    defer source.deinit();

    var tokenizer = LispTokenizer.init(source.items);
    var tokens = std.ArrayList(LispToken).init(allocator);

    defer tokens.deinit();
    while (try tokenizer.next()) |token| try tokens.append(token);
    var parser = LispParser.init(interpreter, source.items, tokens.items);
    while (try parser.next()) |expr| {
        _ = interpreter.eval(expr) catch |err| try stderr.print("{}\n", .{err});
    }
}

fn repl(allocator: *std.mem.Allocator, interpreter: *LispInterpreter) !void {
    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();
    const stdin = std.io.getStdIn().reader();

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

        var tokenizer = LispTokenizer.init(source.items);
        var tokens = std.ArrayList(LispToken).init(allocator);
        defer tokens.deinit();
        while (try tokenizer.next()) |token| try tokens.append(token);
        var parser = LispParser.init(interpreter, source.items, tokens.items);
        while (try parser.next()) |expr| {
            if (interpreter.eval(expr)) |result| {
                try stdout.print("{}\n", .{result});
            } else |err| {
                try stderr.print("{}\n", .{err});
            }
        }
    }
}
