const std = @import("std");

pub const NativeCall = fn (*Interpreter, []*Expr) anyerror!*Expr;

pub const Expr = union(ExprTag) {
    list: std.ArrayList(*Expr),
    identifier: std.ArrayList(u8),
    string: std.ArrayList(u8),
    number: f64,
    macro: struct {
        params: []*Expr,
        body: []*Expr,
    },
    native_func: usize,
    native_macro: usize,

    pub fn deinit(self: Expr) void {
        switch (self) {
            .list => |list| list.deinit(),
            .identifier, .string => |list| list.deinit(),
            .number, .macro, .native_func, .native_macro => {},
        }
    }

    pub fn format(
        self: Expr,
        fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .list => |list| {
                _ = try writer.write("(");
                if (list.items.len > 0) {
                    for (list.items[0 .. list.items.len - 1]) |expr| try writer.print("{} ", .{expr.*});
                    try writer.print("{}", .{list.items[list.items.len - 1]});
                }
                _ = try writer.write(")");
            },
            .identifier => |identifier| try writer.print("{}", .{identifier.items}),
            .string => |string| {
                _ = try writer.write("\"");
                for (string.items) |c| {
                    switch (c) {
                        '"' => _ = try writer.write("\\\""),
                        '\n' => _ = try writer.write("\\n"),
                        '\t' => _ = try writer.write("\\t"),
                        else => try writer.writeByte(c),
                    }
                }
                _ = try writer.write("\"");
            },
            .number => |number| {
                if (@trunc(number) == number) {
                    try writer.print("{}", .{@floatToInt(i64, number)});
                } else {
                    try writer.print("{}", .{number});
                }
            },
            .macro => |macro| {
                _ = try writer.write("(macro (");
                if (macro.params.len > 0) {
                    for (macro.params[0 .. macro.params.len - 1]) |param| try writer.print("{} ", .{param});
                    try writer.print("{}", .{macro.params[macro.params.len - 1]});
                }
                _ = try writer.write(")");
                if (macro.body.len > 0) {
                    for (macro.body) |expr| try writer.print("\n{}", .{expr});
                }
                _ = try writer.write(")");
            },
            .native_func => |address| try writer.print("func@{}", .{address}),
            .native_macro => |address| try writer.print("macro@{}", .{address}),
        }
    }
};

pub const ExprTag = enum {
    list,
    identifier,
    string,
    number,
    macro,
    native_func,
    native_macro,
};

pub const Interpreter = struct {
    allocator: *std.mem.Allocator,
    scope: std.StringHashMap(*Expr),
    mem: std.ArrayList(Expr),
    parent: ?*Interpreter,

    pub fn init(allocator: *std.mem.Allocator, parent: ?*Interpreter) Interpreter {
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
                if (list.items.len == 0) return error.InterpreterEmptyList;
                switch ((try self.eval(list.items[0])).*) {
                    .native_macro => |address| {
                        const macro = @intToPtr(NativeCall, address);
                        return try macro(self, list.items[1..]);
                    },
                    .native_func => |address| {
                        const func = @intToPtr(NativeCall, address);
                        var args_list = try std.ArrayList(*Expr).initCapacity(self.allocator, list.items.len - 1);
                        defer args_list.deinit();
                        for (list.items[1..]) |arg| try args_list.append(try self.eval(arg));
                        return try func(self, args_list.items);
                    },
                    .macro => |macro| {
                        if (macro.params.len != list.items.len - 1) return error.InterpreterMacroParameterMismatch;
                        if (macro.body.len < 1) return error.InterpreterMacroNoBody;
                        var sub_interpreter = Interpreter.init(self.allocator, self);
                        defer sub_interpreter.deinit();
                        for (macro.params) |param, i| switch (param.*) {
                            .identifier => |identifier| try sub_interpreter.scope.put(identifier.items, list.items[i + 1]),
                            else => return error.InterpreterMacroInvalidParameter,
                        };
                        for (macro.body[0 .. macro.body.len - 1]) |body_expr| _ = try sub_interpreter.eval(body_expr);
                        return self.steal(try sub_interpreter.eval(macro.body[macro.body.len - 1]));
                    },
                    else => return error.InterpreterNoSuchFunction,
                }
            },
            .identifier => |identifier| {
                if (self.get(identifier.items)) |value| return value;
                return error.InterpreterNoSuchIdentifier;
            },
            .number, .string, .macro, .native_func, .native_macro => return expr,
        }
    }

    pub fn get(self: Interpreter, identifier: []const u8) ?*Expr {
        if (self.scope.getEntry(identifier)) |entry| return entry.value;
        if (self.parent) |real_parent| return real_parent.get(identifier);
        return null;
    }

    pub fn store(self: *Interpreter, expr: Expr) !*Expr {
        try self.mem.append(expr);
        return &self.mem.items[self.mem.items.len - 1];
    }

    pub fn steal(self: *Interpreter, expr: *Expr) anyerror!*Expr {
        switch (expr.*) {
            .string => |_| return try self.store(Expr{
                .string = std.ArrayList(u8).fromOwnedSlice(
                    expr.string.allocator,
                    expr.string.toOwnedSlice(),
                ),
            }),
            .number => |_| return try self.store(expr.*),
            .native_func, .native_macro => |_| return try self.store(expr.*),
            .list => |_| {
                var stolen_list = std.ArrayList(*Expr).fromOwnedSlice(
                    expr.list.allocator,
                    expr.list.toOwnedSlice(),
                );
                for (stolen_list.items) |branch| _ = try self.steal(branch);
                return try self.store(Expr{ .list = stolen_list });
            },
            .macro => |_| unreachable, // TODO figure out how you want to handle macros, eh?
            .identifier => |_| unreachable, // You can't evaluate an expression into an identifier
        }
    }
};
