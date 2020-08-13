const std = @import("std");

pub const NativeCall = fn (*Interpreter, []*Expr) anyerror!*Expr;

pub const Expr = union(ExprTag) {
    list: std.ArrayList(*Expr),
    identifier: std.ArrayList(u8),
    string: std.ArrayList(u8),
    number: f64,
    native_func: usize,

    pub fn deinit(self: Expr) void {
        switch (self) {
            .list => |list| list.deinit(),
            .identifier, .string => |list| list.deinit(),
            else => {},
        }
    }

    pub fn format(
        self: Expr,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .list => |list| try writer.print("{}", .{list.items}),
            .identifier, .string => |string| try writer.print("{}", .{string.items}),
            .number => |number| {
                if (@trunc(number) == number) {
                    try writer.print("{}", .{@floatToInt(i64, number)});
                } else {
                    try writer.print("{}", .{number});
                }
            },
            .native_func => |address| try writer.print("@{}", .{address}),
        }
    }
};

pub const ExprTag = enum {
    list,
    identifier,
    string,
    number,
    native_func,
};

pub const Interpreter = struct {
    allocator: *std.mem.Allocator,
    scope: std.StringHashMap(*Expr),
    mem: std.ArrayList(Expr),
    parent: ?*const Interpreter,

    pub fn init(allocator: *std.mem.Allocator, parent: ?*const Interpreter) Interpreter {
        return Interpreter{
            .allocator = allocator,
            .scope = std.StringHashMap(*Expr).init(allocator),
            .mem = std.ArrayList(Expr).init(allocator),
            .parent = parent,
        };
    }

    pub fn deinit(self: *Interpreter) void {
        defer self.scope.deinit();
        defer self.mem.deinit();
        defer for (self.mem.items) |expr| expr.deinit();
    }

    pub fn eval(self: *Interpreter, expr: *Expr) anyerror!*Expr {
        switch (expr.*) {
            .list => |list| {
                if (list.items.len == 0) {
                    return error.InterpreterEmptyList;
                } else {
                    switch ((try self.eval(list.items[0])).*) {
                        .native_func => |address| {
                            const func = @intToPtr(NativeCall, address);
                            var args_list = try std.ArrayList(*Expr).initCapacity(self.allocator, list.items.len - 1);
                            defer args_list.deinit();
                            for (list.items[1..]) |arg| try args_list.append(try self.eval(arg));
                            return try func(self, args_list.items);
                        },
                        else => return error.InterpreterNoSuchFunction,
                    }
                }
            },
            .identifier => |identifier| {
                if (self.get(identifier.items)) |value| {
                    return value;
                } else {
                    return error.InterpreterNoSuchIdentifier;
                }
            },
            else => return expr,
        }
    }

    pub fn get(self: Interpreter, identifier: []const u8) ?*Expr {
        if (self.scope.getEntry(identifier)) |entry| {
            return entry.value;
        } else if (self.parent) |real_parent| {
            return real_parent.get(identifier);
        } else {
            return null;
        }
    }

    pub fn store(self: *Interpreter, expr: Expr) !*Expr {
        try self.mem.append(expr);
        return &self.mem.items[self.mem.items.len - 1];
    }
};
