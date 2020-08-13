const std = @import("std");
const stdout = std.io.getStdOut().writer();

const interpret = @import("interpret.zig");

pub fn initCore(allocator: *std.mem.Allocator) !interpret.Interpreter {
    var core = interpret.Interpreter.init(allocator, null);
    errdefer core.deinit();
    try core.scope.put("+", try core.store(interpret.Expr{ .native_func = @ptrToInt(add) }));
    try core.scope.put("-", try core.store(interpret.Expr{ .native_func = @ptrToInt(sub) }));
    try core.scope.put("print", try core.store(interpret.Expr{ .native_func = @ptrToInt(print) }));
    return core;
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

fn sub(interpreter: *interpret.Interpreter, args: []*interpret.Expr) !*interpret.Expr {
    var acc: f64 = 0.0;
    for (args) |arg| {
        switch (arg.*) {
            .number => |num| acc -= num,
            else => return error.AddNotANumber,
        }
    }
    return try interpreter.store(interpret.Expr{ .number = acc });
}

fn print(interpreter: *interpret.Interpreter, args: []*interpret.Expr) !*interpret.Expr {
    if (args.len == 0) return error.PrintNoArguments;
    for (args) |arg| {
        switch (arg.*) {
            .string => |string| try stdout.print("{}", .{string.items}),
            else => try stdout.print("{}", .{arg}),
        }
    }
    try stdout.print("\n", .{});
    return args[args.len - 1];
}
