const std = @import("std");
const stdout = std.io.getStdOut().writer();

const interpret = @import("interpret.zig");

pub fn initCore(allocator: *std.mem.Allocator) !interpret.Interpreter {
    var core = interpret.Interpreter.init(allocator, null);
    errdefer core.deinit();
    try core.scope.put("+", try core.store(interpret.Expr{ .native_func = @ptrToInt(OperationAccumulator(.add).accumulate) }));
    try core.scope.put("-", try core.store(interpret.Expr{ .native_func = @ptrToInt(OperationAccumulator(.sub).accumulate) }));
    try core.scope.put("*", try core.store(interpret.Expr{ .native_func = @ptrToInt(OperationAccumulator(.mul).accumulate) }));
    try core.scope.put("/", try core.store(interpret.Expr{ .native_func = @ptrToInt(OperationAccumulator(.div).accumulate) }));
    try core.scope.put("=", try core.store(interpret.Expr{ .native_func = @ptrToInt(ComparisonAccumulator(.eq).accumulate) }));
    try core.scope.put("!=", try core.store(interpret.Expr{ .native_func = @ptrToInt(ComparisonAccumulator(.neq).accumulate) }));
    try core.scope.put("<", try core.store(interpret.Expr{ .native_func = @ptrToInt(ComparisonAccumulator(.lt).accumulate) }));
    try core.scope.put("<=", try core.store(interpret.Expr{ .native_func = @ptrToInt(ComparisonAccumulator(.lteq).accumulate) }));
    try core.scope.put(">", try core.store(interpret.Expr{ .native_func = @ptrToInt(ComparisonAccumulator(.gt).accumulate) }));
    try core.scope.put(">=", try core.store(interpret.Expr{ .native_func = @ptrToInt(ComparisonAccumulator(.gteq).accumulate) }));
    try core.scope.put("print", try core.store(interpret.Expr{ .native_func = @ptrToInt(print) }));
    try core.scope.put("let", try core.store(interpret.Expr{ .native_macro = @ptrToInt(let) }));
    try core.scope.put("func", try core.store(interpret.Expr{ .native_macro = @ptrToInt(func) }));
    try core.scope.put("macro", try core.store(interpret.Expr{ .native_macro = @ptrToInt(macro) }));
    try core.scope.put("if", try core.store(interpret.Expr{ .native_macro = @ptrToInt(@"if") }));
    try core.scope.put("while", try core.store(interpret.Expr{ .native_macro = @ptrToInt(@"while") }));
    try core.scope.put("list", try core.store(interpret.Expr{ .native_func = @ptrToInt(list) }));
    try core.scope.put("len", try core.store(interpret.Expr{ .native_func = @ptrToInt(len) }));
    try core.scope.put("at", try core.store(interpret.Expr{ .native_func = @ptrToInt(at) }));
    try core.scope.put("push", try core.store(interpret.Expr{ .native_func = @ptrToInt(push) }));
    return core;
}

const Operation = enum { add, sub, mul, div };
fn OperationAccumulator(op: Operation) type {
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

const Comparison = enum { eq, neq, gt, gteq, lt, lteq };
fn ComparisonAccumulator(comp: Comparison) type {
    return struct {
        fn accumulate(interpreter: *interpret.Interpreter, args: []*interpret.Expr) !*interpret.Expr {
            const first = switch (args[0].*) {
                .number => |number| number,
                else => return error.ComparisonNotANumber,
            };
            for (args[1..]) |branch| switch (branch.*) {
                .number => |number| if (!switch (comp) {
                    .eq => first == number,
                    .neq => first != number,
                    .gt => first > number,
                    .gteq => first >= number,
                    .lt => first < number,
                    .lteq => first <= number,
                }) return try interpreter.store(.{ .list = std.ArrayList(*interpret.Expr).init(interpreter.allocator) }),
                else => return error.ComparisonNotANumber,
            };
            return try interpreter.store(.{ .number = 1.0 });
        }
    };
}

fn print(interpreter: *interpret.Interpreter, args: []*interpret.Expr) !*interpret.Expr {
    if (args.len == 0) return error.PrintInvalidArguments;
    for (args) |arg| {
        switch (arg.*) {
            .string => |string| try stdout.print("{}", .{string.items}),
            else => try stdout.print("{}", .{arg}),
        }
    }
    return args[args.len - 1];
}

fn let(interpreter: *interpret.Interpreter, args: []*interpret.Expr) !*interpret.Expr {
    if (args.len != 2) return error.LetInvalidArguments;
    switch (args[0].*) {
        .identifier => |identifier| {
            const value = try interpreter.eval(args[1]);
            try interpreter.scope.put(identifier.items, value);
            return value;
        },
        else => return error.LetNoIdentifier,
    }
}

fn func(interpreter: *interpret.Interpreter, args: []*interpret.Expr) !*interpret.Expr {
    if (args.len < 2) return error.FuncInvalidArguments;
    if (args[0].* != .list) return error.FuncNoParameters;
    var params = try std.ArrayList(*interpret.Expr).initCapacity(interpreter.allocator, args[0].list.items.len);
    errdefer params.deinit();
    for (args[0].list.items) |param| {
        if (param.* == .identifier) {
            try params.append(param);
        } else {
            return error.FuncInvalidParameter;
        }
    }
    var body = try std.ArrayList(*interpret.Expr).initCapacity(interpreter.allocator, args.len - 1);
    for (args[1..]) |expr| try body.append(expr);
    return try interpreter.store(interpret.Expr{ .func = .{ .params = params, .body = body } });
}

fn macro(interpreter: *interpret.Interpreter, args: []*interpret.Expr) !*interpret.Expr {
    if (args.len < 2) return error.MacroInvalidArguments;
    if (args[0].* != .list) return error.FuncNoParameters;
    var params = try std.ArrayList(*interpret.Expr).initCapacity(interpreter.allocator, args[0].list.items.len);
    errdefer params.deinit();
    for (args[0].list.items) |param| {
        if (param.* == .identifier) {
            try params.append(param);
        } else {
            return error.FuncInvalidParameter;
        }
    }
    var body = try std.ArrayList(*interpret.Expr).initCapacity(interpreter.allocator, args.len - 1);
    for (args[1..]) |expr| try body.append(expr);
    return try interpreter.store(interpret.Expr{ .macro = .{ .params = params, .body = body } });
}

fn @"if"(interpreter: *interpret.Interpreter, args: []*interpret.Expr) !*interpret.Expr {
    if (args.len < 3) return error.IfInvalidArguments;
    const cond = switch ((try interpreter.eval(args[0])).*) {
        .list => |cond| cond.items.len > 0,
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

fn @"while"(interpreter: *interpret.Interpreter, args: []*interpret.Expr) !*interpret.Expr {
    if (args.len < 2) return error.IfInvalidArguments;
    while (switch ((try interpreter.eval(args[0])).*) {
        .list => |cond| cond.items.len > 0,
        else => true,
    }) {
        for (args[1..]) |branch| _ = try interpreter.eval(branch);
    }
    return try interpreter.store(.{ .list = std.ArrayList(*interpret.Expr).init(interpreter.allocator) });
}

fn list(interpreter: *interpret.Interpreter, args: []*interpret.Expr) !*interpret.Expr {
    var exprs = try std.ArrayList(*interpret.Expr).initCapacity(interpreter.allocator, args.len);
    errdefer exprs.deinit();
    for (args) |arg| try exprs.append(arg);
    return try interpreter.store(interpret.Expr{ .list = exprs });
}

fn len(interpreter: *interpret.Interpreter, args: []*interpret.Expr) !*interpret.Expr {
    if (args.len != 1) return error.LenInvalidArguments;
    if (args[0].* != .list) return error.PushNotAList;
    return try interpreter.store(.{ .number = @intToFloat(f64, args[0].list.items.len) });
}

fn at(interpreter: *interpret.Interpreter, args: []*interpret.Expr) !*interpret.Expr {
    if (args.len != 2) return error.AtInvalidArguments;
    if (args[0].* != .list) return error.PushNotAList;
    if (args[1].* != .number) return error.PushNotANumber;
    return args[0].list.items[@floatToInt(usize, args[1].number)];
}

fn push(interpreter: *interpret.Interpreter, args: []*interpret.Expr) !*interpret.Expr {
    if (args.len < 2) return error.PushInvalidArguments;
    if (args[0].* != .list) return error.PushNotAList;
    for (args[1..]) |branch| try args[0].list.append(branch);
    return args[args.len - 1];
}
