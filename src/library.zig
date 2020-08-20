const std = @import("std");
const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();

const parse = @import("parse.zig");
const Tokenizer = parse.Tokenizer;
const Parser = parse.Parser;

const interpret = @import("interpret.zig");
const Expr = interpret.Expr;
const Call = interpret.Call;
const Interpreter = interpret.Interpreter;

pub fn initCore(allocator: *std.mem.Allocator) !Interpreter {
    var core = Interpreter.init(allocator, null);
    errdefer core.deinit();
    try core.scope.put("nil", Expr{ .nil = undefined });
    try core.scope.put("t", Expr{ .t = undefined });
    try core.scope.put("+", Expr{ .native_func = @ptrToInt(OperationAccumulator(.add).accumulate) });
    try core.scope.put("-", Expr{ .native_func = @ptrToInt(OperationAccumulator(.sub).accumulate) });
    try core.scope.put("*", Expr{ .native_func = @ptrToInt(OperationAccumulator(.mul).accumulate) });
    try core.scope.put("/", Expr{ .native_func = @ptrToInt(OperationAccumulator(.div).accumulate) });
    try core.scope.put("=", Expr{ .native_func = @ptrToInt(ComparisonAccumulator(.eq).accumulate) });
    try core.scope.put("!=", Expr{ .native_func = @ptrToInt(ComparisonAccumulator(.neq).accumulate) });
    try core.scope.put("<", Expr{ .native_func = @ptrToInt(ComparisonAccumulator(.lt).accumulate) });
    try core.scope.put("<=", Expr{ .native_func = @ptrToInt(ComparisonAccumulator(.lteq).accumulate) });
    try core.scope.put(">", Expr{ .native_func = @ptrToInt(ComparisonAccumulator(.gt).accumulate) });
    try core.scope.put(">=", Expr{ .native_func = @ptrToInt(ComparisonAccumulator(.gteq).accumulate) });
    try core.scope.put("print", Expr{ .native_func = @ptrToInt(print) });
    try core.scope.put("let", Expr{ .native_macro = @ptrToInt(let) });
    try core.scope.put("func", Expr{ .native_macro = @ptrToInt(Callable(.func).call) });
    try core.scope.put("macro", Expr{ .native_macro = @ptrToInt(Callable(.macro).call) });
    try core.scope.put("if", Expr{ .native_macro = @ptrToInt(@"if") });
    try core.scope.put("while", Expr{ .native_macro = @ptrToInt(@"while") });
    try core.scope.put("list", Expr{ .native_func = @ptrToInt(list) });
    try core.scope.put("len", Expr{ .native_func = @ptrToInt(len) });
    try core.scope.put("at", Expr{ .native_func = @ptrToInt(at) });
    try core.scope.put("push", Expr{ .native_func = @ptrToInt(push) });
    try core.scope.put("load", Expr{ .native_func = @ptrToInt(load) });
    return core;
}

const Operation = enum { add, sub, mul, div };
fn OperationAccumulator(op: Operation) type {
    return struct {
        fn accumulate(interpreter: *Interpreter, args: []Expr) !Expr {
            var acc: f64 = switch (op) {
                .add, .sub => 0.0,
                .mul, .div => 1.0,
            };
            for (args) |arg| {
                switch (arg) {
                    .number => |num| switch (op) {
                        .add => acc += num,
                        .sub => acc -= num,
                        .mul => acc *= num,
                        .div => acc /= num,
                    },
                    else => return error.AddNotANumber,
                }
            }
            return Expr{ .number = acc };
        }
    };
}

const Comparison = enum { eq, neq, gt, gteq, lt, lteq };
fn ComparisonAccumulator(comp: Comparison) type {
    return struct {
        fn accumulate(interpreter: *Interpreter, args: []Expr) !Expr {
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
                }) return Expr{ .nil = undefined },
                else => return error.ComparisonNotANumber,
            };
            return Expr{ .t = undefined };
        }
    };
}

fn print(interpreter: *Interpreter, args: []Expr) !Expr {
    if (args.len == 0) return error.PrintInvalidArguments;
    for (args) |arg| {
        switch (arg) {
            .string => |string| try stdout.print("{}", .{string.items}),
            else => try stdout.print("{}", .{arg}),
        }
    }
    return args[args.len - 1];
}

fn let(interpreter: *Interpreter, args: []Expr) !Expr {
    if (args.len != 2) return error.LetInvalidArguments;
    switch (args[0]) {
        .identifier => |identifier| {
            const value = try interpreter.eval(args[1]);
            try interpreter.scope.put(identifier.items, value);
            return value;
        },
        else => return error.LetNoIdentifier,
    }
}

const CallableType = enum {
    func,
    macro,
};
fn Callable(callable_type: CallableType) type {
    return struct {
        fn call(interpreter: *Interpreter, args: []Expr) !Expr {
            if (args.len < 2) return error.FuncInvalidArguments;
            if (args[0] != .list) return error.FuncNoParameters;
            var params = try interpreter.allocator.create(std.ArrayList(Expr));
            errdefer interpreter.allocator.destroy(params);
            params.* = try std.ArrayList(Expr).initCapacity(interpreter.allocator, args[0].list.items.len);
            errdefer params.deinit();
            for (args[0].list.items) |param| {
                if (param == .identifier) {
                    try params.append(param);
                } else {
                    return error.FuncInvalidParameter;
                }
            }
            var body = try interpreter.allocator.create(std.ArrayList(Expr));
            errdefer interpreter.allocator.destroy(body);
            body.* = try std.ArrayList(Expr).initCapacity(interpreter.allocator, args.len - 1);
            errdefer body.deinit();
            for (args[1..]) |expr| try body.append(expr);
            const callable = Call{ .params = params, .body = body };
            switch (callable_type) {
                .func => return try interpreter.store(Expr{ .func = callable }),
                .macro => return try interpreter.store(Expr{ .macro = callable }),
            }
        }
    };
}

fn @"if"(interpreter: *Interpreter, args: []Expr) !Expr {
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

fn @"while"(interpreter: *Interpreter, args: []Expr) !Expr {
    if (args.len < 2) return error.WhileInvalidArguments;
    while ((try interpreter.eval(args[0])) != .nil) {
        for (args[1..]) |branch| _ = try interpreter.eval(branch);
    }
    return Expr{ .nil = undefined };
}

fn list(interpreter: *Interpreter, args: []Expr) !Expr {
    var exprs = try interpreter.allocator.create(std.ArrayList(Expr));
    errdefer interpreter.allocator.destroy(exprs);
    exprs.* = try std.ArrayList(Expr).initCapacity(interpreter.allocator, args.len);
    errdefer exprs.deinit();
    for (args) |arg| try exprs.append(arg);
    return try interpreter.store(Expr{ .list = exprs });
}

fn len(interpreter: *Interpreter, args: []Expr) !Expr {
    if (args.len != 1) return error.LenInvalidArguments;
    if (args[0] != .list) return error.PushNotAList;
    return Expr{ .number = @intToFloat(f64, args[0].list.items.len) };
}

fn at(interpreter: *Interpreter, args: []Expr) !Expr {
    if (args.len != 2) return error.AtInvalidArguments;
    if (args[0] != .list) return error.AtNotAList;
    if (args[1] != .number) return error.AtNotANumber;
    const index = @floatToInt(isize, args[1].number);
    if (index < 0 or index >= args[0].list.items.len) return error.AtIndexOutOfBounds;
    return args[0].list.items[@intCast(usize, index)];
}

fn push(interpreter: *Interpreter, args: []Expr) !Expr {
    if (args.len < 2) return error.PushInvalidArguments;
    if (args[0] != .list) return error.PushNotAList;
    for (args[1..]) |branch| try args[0].list.append(branch);
    return args[args.len - 1];
}

fn load(interpreter: *Interpreter, args: []Expr) !Expr {
    if (args.len < 1) return error.LoadInvalidArguments;
    for (args) |branch| {
        if (branch != .string) return error.LoadInvalidPath;
        var file = try std.fs.cwd().openFile(branch.string.items, .{});
        defer file.close();
        const eof = try file.getEndPos();
        var source = std.ArrayList(u8).init(interpreter.allocator);
        defer source.deinit();
        try file.reader().readAllArrayList(&source, eof);
        var tokenizer = Tokenizer.init(source.items);
        var tokens = std.ArrayList(parse.Token).init(interpreter.allocator);
        defer tokens.deinit();
        while (try tokenizer.next()) |token| try tokens.append(token);

        var parser = Parser.init(interpreter, source.items, tokens.items);
        while (try parser.next()) |expr| {
            if (try interpreter.eval(expr)) {} else |err| try stderr.print("{}\n", .{err});
        }
    }
    return Expr{ .nil = undefined };
}
