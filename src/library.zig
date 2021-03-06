const std = @import("std");

const parse = @import("parse.zig");
const LispTokenizer = parse.LispTokenizer;
const LispParser = parse.LispParser;

const LispCall = interpret.LispCall;
const LispClosure = interpret.LispClosure;
const LispExpr = interpret.LispExpr;
const LispInterpreter = interpret.LispInterpreter;
const LispNativeCall = interpret.LispNativeCall;
const interpret = @import("interpret.zig");

pub fn initCore(allocator: *std.mem.Allocator) !LispInterpreter {
    var core = LispInterpreter.init(allocator, null);
    errdefer core.deinit();
    try core.scope.put("nil", LispExpr{ .nil = {} });
    try core.scope.put("t", LispExpr{ .t = {} });
    try core.scope.put("+", LispExpr{ .native_func = @ptrToInt(Operation(.add)) });
    try core.scope.put("-", LispExpr{ .native_func = @ptrToInt(Operation(.sub)) });
    try core.scope.put("*", LispExpr{ .native_func = @ptrToInt(Operation(.mul)) });
    try core.scope.put("/", LispExpr{ .native_func = @ptrToInt(Operation(.div)) });
    try core.scope.put("=", LispExpr{ .native_func = @ptrToInt(Comparison(.eq)) });
    try core.scope.put("!=", LispExpr{ .native_func = @ptrToInt(Comparison(.neq)) });
    try core.scope.put("<", LispExpr{ .native_func = @ptrToInt(Comparison(.lt)) });
    try core.scope.put("<=", LispExpr{ .native_func = @ptrToInt(Comparison(.lteq)) });
    try core.scope.put(">", LispExpr{ .native_func = @ptrToInt(Comparison(.gt)) });
    try core.scope.put(">=", LispExpr{ .native_func = @ptrToInt(Comparison(.gteq)) });
    try core.scope.put("print", LispExpr{ .native_func = @ptrToInt(print) });
    try core.scope.put("let", LispExpr{ .native_macro = @ptrToInt(let) });
    try core.scope.put("set", LispExpr{ .native_macro = @ptrToInt(set) });
    try core.scope.put("func", LispExpr{ .native_macro = @ptrToInt(func) });
    try core.scope.put("macro", LispExpr{ .native_macro = @ptrToInt(macro) });
    try core.scope.put("if", LispExpr{ .native_macro = @ptrToInt(@"if") });
    try core.scope.put("while", LispExpr{ .native_macro = @ptrToInt(@"while") });
    try core.scope.put("list", LispExpr{ .native_func = @ptrToInt(list) });
    try core.scope.put("len", LispExpr{ .native_func = @ptrToInt(len) });
    try core.scope.put("at", LispExpr{ .native_func = @ptrToInt(at) });
    try core.scope.put("push", LispExpr{ .native_func = @ptrToInt(push) });
    try core.scope.put("pop", LispExpr{ .native_func = @ptrToInt(pop) });
    try core.scope.put("clone", LispExpr{ .native_func = @ptrToInt(clone) });
    return core;
}

const OperationType = enum { add, sub, mul, div };
fn Operation(comptime op: OperationType) LispNativeCall {
    const impl = switch (op) {
        .add, .mul => struct {
            fn inner(interpreter: *LispInterpreter, args: []LispExpr) !LispExpr {
                var acc: isize = switch (op) {
                    .add => 0,
                    .mul => 1,
                    else => unreachable,
                };
                for (args) |arg| {
                    switch (arg) {
                        .number => |num| switch (op) {
                            .add => acc += num,
                            .mul => acc *= num,
                            else => unreachable,
                        },
                        else => return error.OperationNotANumber,
                    }
                }
                return LispExpr{ .number = acc };
            }
        },
        .sub, .div => struct {
            fn inner(interpreter: *LispInterpreter, args: []LispExpr) !LispExpr {
                if (args.len != 2) return error.OperationInvalidArguments;
                const first = switch (args[0]) {
                    .number => |number| number,
                    else => return error.OperationNotANumber,
                };
                const second = switch (args[1]) {
                    .number => |number| number,
                    else => return error.OperationNotANumber,
                };
                return LispExpr{
                    .number = switch (op) {
                        .sub => first - second,
                        .div => @divTrunc(first, second),
                        else => unreachable,
                    },
                };
            }
        },
    };
    return impl.inner;
}

const ComparisonType = enum { eq, neq, gt, gteq, lt, lteq };
fn Comparison(comptime comp: ComparisonType) LispNativeCall {
    const impl = struct {
        fn inner(interpreter: *LispInterpreter, args: []LispExpr) !LispExpr {
            const first = switch (args[0]) {
                .number => |number| number,
                else => return error.ComparisonNotANumber,
            };
            for (args[1..]) |branch| switch (branch) {
                .number => |number| if (!switch (comp) {
                    .eq => first == number,
                    .neq => first != number,
                    .gt => first > number,
                    .gteq => first >= number,
                    .lt => first < number,
                    .lteq => first <= number,
                }) return LispExpr{ .nil = {} },
                else => return error.ComparisonNotANumber,
            };
            return LispExpr{ .t = {} };
        }
    };
    return impl.inner;
}

fn print(interpreter: *LispInterpreter, args: []LispExpr) !LispExpr {
    const stdout = std.io.getStdOut().writer();
    if (args.len == 0) return error.PrintInvalidArguments;
    for (args) |arg| {
        switch (arg) {
            .string => |string| try stdout.print("{s}", .{string.items}),
            else => try stdout.print("{s}", .{arg}),
        }
    }
    return args[args.len - 1];
}

fn let(interpreter: *LispInterpreter, args: []LispExpr) !LispExpr {
    if (args.len != 2) return error.LetInvalidArguments;
    if (args[0] != .identifier) return error.LetNoIdentifier;
    if (interpreter.scope.contains(args[0].identifier.items)) return error.LetIdentifierAlreadyExists;
    const value = try interpreter.eval(args[1]);
    try interpreter.scope.put(args[0].identifier.items, value);
    return value;
}

fn set(interpreter: *LispInterpreter, args: []LispExpr) !LispExpr {
    if (args.len != 2) return error.SetInvalidArguments;
    if (args[0] != .identifier) return error.SetNoIdentifier;
    if (!interpreter.scope.contains(args[0].identifier.items)) return error.SetNoSuchIdentifer;
    const value = try interpreter.eval(args[1]);
    try interpreter.scope.put(args[0].identifier.items, value);
    return value;
}

fn macro(interpreter: *LispInterpreter, args: []LispExpr) !LispExpr {
    if (args.len < 2) return error.MacroInvalidArguments;
    if (args[0] != .list) return error.MacroNoParameters;

    var callable = try interpreter.allocator.create(LispCall);
    errdefer interpreter.allocator.destroy(callable);

    callable.params = try std.ArrayList(LispExpr).initCapacity(interpreter.allocator, args[0].list.items.len);
    errdefer callable.params.deinit();

    for (args[0].list.items) |param| {
        if (param == .identifier) {
            try callable.params.append(param);
        } else {
            return error.MacroInvalidParameter;
        }
    }

    callable.body = try std.ArrayList(LispExpr).initCapacity(interpreter.allocator, args.len - 1);
    errdefer callable.body.deinit();

    for (args[1..]) |expr| try callable.body.append(expr);

    return try interpreter.store(LispExpr{ .macro = callable });
}

fn func(interpreter: *LispInterpreter, args: []LispExpr) !LispExpr {
    if (args.len < 3) return error.FuncInvalidArguments;
    if (args[0] != .list) return error.FuncNoParameters;
    if (args[1] != .list) return error.FuncNoClosure;

    var closure = try interpreter.allocator.create(LispClosure);
    errdefer interpreter.allocator.destroy(closure);

    closure.call.params = try std.ArrayList(LispExpr).initCapacity(interpreter.allocator, args[0].list.items.len);
    errdefer closure.call.params.deinit();

    for (args[0].list.items) |param| {
        if (param != .identifier) return error.FuncInvalidParameter;
        try closure.call.params.append(param);
    }

    closure.interpreter = LispInterpreter.init(interpreter.allocator, null);
    errdefer closure.interpreter.deinit();

    for (args[1].list.items) |captured| {
        if (captured != .identifier) return error.FuncInvalidParameter;
        if (interpreter.get(captured.identifier.items)) |value| {
            try closure.interpreter.scope.put(captured.identifier.items, try closure.interpreter.clone(value));
        } else {
            return error.FuncCaptureNoSuchIdentifier;
        }
    }

    closure.call.body = try std.ArrayList(LispExpr).initCapacity(interpreter.allocator, args.len - 1);
    errdefer closure.call.body.deinit();
    for (args[2..]) |expr| try closure.call.body.append(expr);

    return try interpreter.store(LispExpr{ .func = closure });
}

fn @"if"(interpreter: *LispInterpreter, args: []LispExpr) !LispExpr {
    if (args.len < 3) return error.IfInvalidArguments;
    const cond = try interpreter.eval(args[0]);
    if (cond != .nil) {
        return try interpreter.eval(args[1]);
    } else {
        if (args.len > 3) {
            for (args[2 .. args.len - 1]) |branch| _ = try interpreter.eval(branch);
        }
        return try interpreter.eval(args[args.len - 1]);
    }
}

fn @"while"(interpreter: *LispInterpreter, args: []LispExpr) !LispExpr {
    if (args.len < 2) return error.WhileInvalidArguments;
    while ((try interpreter.eval(args[0])) != .nil) {
        for (args[1..]) |branch| _ = try interpreter.eval(branch);
    }
    return LispExpr{ .nil = {} };
}

fn list(interpreter: *LispInterpreter, args: []LispExpr) !LispExpr {
    var exprs = try interpreter.allocator.create(std.ArrayList(LispExpr));
    errdefer interpreter.allocator.destroy(exprs);
    exprs.* = try std.ArrayList(LispExpr).initCapacity(interpreter.allocator, args.len);
    errdefer exprs.deinit();
    for (args) |arg| try exprs.append(arg);
    return try interpreter.store(LispExpr{ .list = exprs });
}

fn len(interpreter: *LispInterpreter, args: []LispExpr) !LispExpr {
    if (args.len != 1) return error.LenInvalidArguments;
    if (args[0] != .list) return error.PushNotAList;
    return LispExpr{ .number = @intCast(isize, args[0].list.items.len) };
}

fn at(interpreter: *LispInterpreter, args: []LispExpr) !LispExpr {
    if (args.len != 2) return error.AtInvalidArguments;
    if (args[0] != .list) return error.AtNotAList;
    if (args[1] != .number) return error.AtNotANumber;
    const index = args[1].number;
    if (index < 0 or index >= args[0].list.items.len) return error.AtIndexOutOfBounds;
    return args[0].list.items[@intCast(usize, index)];
}

fn push(interpreter: *LispInterpreter, args: []LispExpr) !LispExpr {
    if (args.len < 2) return error.PushInvalidArguments;
    if (args[0] != .list) return error.PushNotAList;
    for (args[1..]) |branch| try args[0].list.append(branch);
    return args[args.len - 1];
}

fn pop(interpreter: *LispInterpreter, args: []LispExpr) !LispExpr {
    if (args.len != 1) return error.PopInvalidArguments;
    if (args[0] != .list) return error.PopNotAList;
    return args[0].list.pop();
}

fn clone(interpreter: *LispInterpreter, args: []LispExpr) !LispExpr {
    if (args.len != 1) return error.CloneInvalidArguments;
    return interpreter.clone(args[0]);
}
