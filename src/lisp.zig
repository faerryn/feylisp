const std = @import("std");
const parse = @import("parse.zig");

pub const NativeFunc = fn (*Lisp, []Expr) anyerror!Expr;

pub const Expr = union(ExprTag) {
    list: std.ArrayList(Expr),
    quoted_list: std.ArrayList(Expr),
    reference: *Expr,
    identifier: std.ArrayList(u8),
    integer: i64,
    float: f64,
    string: std.ArrayList(u8),
    native_func: usize,

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
    native_func,
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
        switch (expr.*) {
            .list => |const_list| {
                if (const_list.items.len == 0) {
                    return error.LispEmptyList;
                } else {
                    switch (try self.eval(&const_list.items[0])) {
                        .native_func => |address| {
                            const func = @intToPtr(NativeFunc, address);
                            return try func(self, const_list.items[1..]);
                        },
                        .reference => |reference| {
                            switch (reference.*) {
                                .native_func => |address| {
                                    const func = @intToPtr(NativeFunc, address);
                                    return try func(self, const_list.items[1..]);
                                },
                                else => return error.LispNoSuchFunction,
                            }
                        },
                        else => return error.LispNoSuchFunction,
                    }
                }
            },
            .identifier => |identifier| {
                if (self.getIdentifier(identifier.items)) |value| {
                    return Expr{ .reference = value };
                } else {
                    return error.LispNoSuchIdentifier;
                }
            },
            else => {
                return Expr{ .reference = expr };
            },
        }
    }

    pub fn getIdentifier(self: Lisp, identifier: []const u8) ?*Expr {
        if (self.scope.getEntry(identifier)) |entry| {
            return &entry.value;
        } else if (self.parent) |real_parent| {
            return real_parent.getIdentifier(identifier);
        } else {
            return null;
        }
    }

    pub fn letIdentifier(self: *Lisp, identifier: []const u8, expr: Expr) !void {
        if (self.scope.contains(identifier)) {
            return error.LispRedeclareInScope;
        } else {
            try self.scope.put(identifier, expr);
        }
    }
};
