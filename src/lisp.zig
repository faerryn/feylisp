const std = @import("std");
const parse = @import("parse.zig");

pub const Expr = union(ExprTag) {
    list: std.ArrayList(Expr),
    quoted_list: std.ArrayList(Expr),
    identifier: std.ArrayList(u8),
    integer: i64,
    float: f64,
    string: std.ArrayList(u8),

    pub fn deinit(self: Expr) void {
        switch (self) {
            .list, .quoted_list => |list| {
                defer list.deinit();
                for (list.items) |branch| branch.deinit();
            },
            .identifier, .string => |string| {
                string.deinit();
            },
            else => {},
        }
    }
};

pub const ExprTag = enum {
    list,
    quoted_list,
    identifier,
    integer,
    float,
    string,
};

pub const Context = struct {
    allocator: *std.mem.Allocator,
    scope: std.StringHashMap(Expr),
};

pub const Lisp = struct {
    root_context: *Context,

    pub fn init(root_context: *Context) Lisp {}
};
