const std = @import("std");
const parse = @import("parse.zig");

pub const Expr = union(ExprTag) {
    list: std.ArrayList(Expr),
    quoted_list: std.ArrayList(Expr),
    reference: *Expr,
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
    reference,
    identifier,
    integer,
    float,
    string,
};

pub const Lisp = struct {
    allocator: *std.mem.Allocator,
    scope: std.StringHashMap(Expr),
    parent: ?*Lisp,

    pub fn init(allocator: *std.mem.Allocator, parent: ?*Lisp) Lisp {
        return Lisp{
            .allocator = allocator,
            .scope = std.StringHashMap(Expr).init(allocator),
            .parent = parent,
        };
    }

    pub fn deinit(self: *Lisp) void {
        defer self.scope.deinit();
        for (self.scope.items()) |entry| entry.value.deinit();
    }

    pub fn eval(self: *Lisp, expr: *Expr) anyerror!Expr {
        return Expr{ .reference = expr };
        // switch (expr.*) {
        //     .list => |const_list| {
        //         if (const_list.items.len == 0) {
        //             return error.LispEmptyList;
        //         } else {
        //             switch (self.eval(const_list.items[0])) {}
        //         }
        //     },
        //     .identifier => {
        //         var value = self.getIdentifier(identifier);
        //         switch (value.*) {}
        //     },
        // }
    }

    pub fn getIdentifier(self: *Context, identifier: []const u8) ?*Expr {
        if (self.getEntry(identifier)) |entry| {
            return &entry.value;
        } else if (self.parent) |real_parent| {
            return real_parent.get(identifier);
        } else {
            return null;
        }
    }
};
