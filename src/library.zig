const std = @import("std");
const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();
const stdin = std.io.getStdIn().reader();
const parse = @import("parse.zig");
const lisp = @import("lisp.zig");

pub fn core(allocator: *std.mem.Allocator) !lisp.Engine {
    var lisp_engine = lisp.Engine.init(allocator, null);
    errdefer lisp_engine.deinit();
    try lisp_engine.letIdentifier("+", lisp.Expr{ .native_func = @ptrToInt(add) });
    return lisp_engine;
}

fn add(engine: *lisp.Engine, exprs: []lisp.Expr) anyerror!lisp.Expr {
    const lhs = switch (try engine.eval(&exprs[0])) {
        .integer => |integer| integer,
        else => return error.AddNotInteger,
    };
    const rhs = switch (try engine.eval(&exprs[1])) {
        .integer => |integer| integer,
        else => return error.AddNotInteger,
    };
    return lisp.Expr{ .integer = lhs + rhs };
}
