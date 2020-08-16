const std = @import("std");
const stdout = std.io.getStdOut().writer();

const interpret = @import("interpret.zig");

pub fn initCore(allocator: *std.mem.Allocator) !interpret.Interpreter {
    var core = interpret.Interpreter.init(allocator, null);
    errdefer core.deinit();
    try core.scope.put("+", try core.store(interpret.Expr{ .native_func = @ptrToInt(Accumulator(.add).accumulate) }));
    try core.scope.put("-", try core.store(interpret.Expr{ .native_func = @ptrToInt(Accumulator(.sub).accumulate) }));
    try core.scope.put("*", try core.store(interpret.Expr{ .native_func = @ptrToInt(Accumulator(.mul).accumulate) }));
    try core.scope.put("/", try core.store(interpret.Expr{ .native_func = @ptrToInt(Accumulator(.div).accumulate) }));
    try core.scope.put("print", try core.store(interpret.Expr{ .native_func = @ptrToInt(print) }));
    try core.scope.put("let", try core.store(interpret.Expr{ .native_macro = @ptrToInt(let) }));
    try core.scope.put("func", try core.store(interpret.Expr{ .native_macro = @ptrToInt(func) }));
    try core.scope.put("macro", try core.store(interpret.Expr{ .native_macro = @ptrToInt(macro) }));
    try core.scope.put("list", try core.store(interpret.Expr{ .native_func = @ptrToInt(list) }));
    try core.scope.put("if", try core.store(interpret.Expr{ .native_macro = @ptrToInt(@"if") }));
    return core;
}

const Operation = enum { add, sub, mul, div };
fn Accumulator(op: Operation) type {
    return struct {
        fn accumulate(interpreter: *interpret.Interpreter, args: []*interpret.Expr) !*interpret.Expr {
            var acc: f64 = switch (op) {
                .add, .sub => 0.0,
                .mul, .div => 1.0,
            };
            for (args) |arg| {
                switch (arg.*) {
                    .number => |num| switch (op) {
                        .add => acc += num,
                        .sub => acc -= num,
                        .mul => acc *= num,
                        .div => acc /= num,
                    },
                    else => return error.AddNotANumber,
                }
            }
            return try interpreter.store(interpret.Expr{ .number = acc });
        }
    };
}

fn print(interpreter: *interpret.Interpreter, args: []*interpret.Expr) !*interpret.Expr {
    if (args.len == 0) return error.PrintNoArguments;
    for (args) |arg| {
        switch (arg.*) {
            .string => |string| try stdout.print("{}", .{string.items}),
            else => try stdout.print("{}", .{arg}),
        }
    }
    return args[args.len - 1];
}

fn let(interpreter: *interpret.Interpreter, args: []*interpret.Expr) !*interpret.Expr {
    if (args.len != 2) return error.LetInvalidArgumentsLength;
    switch (args[0].*) {
        .identifier => |identifier| {
            const value = try interpreter.eval(args[1]);
            try interpreter.scope.put(identifier.items, value);
            return value;
        },
        else => return error.LetInvalidIdentifier,
    }
}

fn func(interpreter: *interpret.Interpreter, args: []*interpret.Expr) !*interpret.Expr {
    if (args.len < 2) return error.FuncInvalidArgumentsLength;
    const params = switch (args[0].*) {
        .list => |params| blk: {
            var params_copy = try std.ArrayList(*interpret.Expr).initCapacity(interpreter.allocator, params.items.len);
            errdefer params.deinit();
            for (params.items) |param| switch (param.*) {
                .identifier => |_| try params_copy.append(param),
                else => return error.FuncInvalidParameter,
            };
            break :blk params_copy;
        },
        else => return error.FuncInvalidParametersList,
    };
    var body = try std.ArrayList(*interpret.Expr).initCapacity(interpreter.allocator, args.len - 1);
    for (args[1..]) |expr| try body.append(expr);
    return interpreter.store(interpret.Expr{ .func = .{ .params = params, .body = body } });
}

fn macro(interpreter: *interpret.Interpreter, args: []*interpret.Expr) !*interpret.Expr {
    if (args.len < 2) return error.MacroInvalidArgumentsLength;
    const params = switch (args[0].*) {
        .list => |params| blk: {
            var params_copy = try std.ArrayList(*interpret.Expr).initCapacity(interpreter.allocator, params.items.len);
            errdefer params.deinit();
            for (params.items) |param| switch (param.*) {
                .identifier => |_| try params_copy.append(param),
                else => return error.MacroInvalidParameter,
            };
            break :blk params_copy;
        },
        else => return error.MacroInvalidParametersList,
    };
    var body = try std.ArrayList(*interpret.Expr).initCapacity(interpreter.allocator, args.len - 1);
    for (args[1..]) |expr| try body.append(expr);
    return interpreter.store(interpret.Expr{ .macro = .{ .params = params, .body = body } });
}

fn list(interpreter: *interpret.Interpreter, args: []*interpret.Expr) !*interpret.Expr {
    var exprs = try std.ArrayList(*interpret.Expr).initCapacity(interpreter.allocator, args.len);
    errdefer exprs.deinit();
    for (args) |arg| try exprs.append(arg);
    return interpreter.store(interpret.Expr{ .list = exprs });
}

fn @"if"(interpreter: *interpret.Interpreter, args: []*interpret.Expr) !*interpret.Expr {
    if (args.len < 3) return error.IfInvalidArgumentsLength;
    const cond = switch ((try interpreter.eval(args[0])).*) {
        .list => |cond_expr| cond_expr.items.len > 0,
        else => true,
    };
    if (cond) {
        return try interpreter.eval(args[1]);
    } else {
        if (args.len > 3) {
            for (args[2 .. args.len - 1]) |branch| _ = try interpreter.eval(branch);
        }
        return try interpreter.eval(args[args.len - 1]);
    }
}
