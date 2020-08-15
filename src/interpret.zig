const std = @import("std");

pub const NativeCall = fn (*Interpreter, []*Expr) anyerror!*Expr;
pub const Call = struct {
    params: std.ArrayList(*Expr),
    body: std.ArrayList(*Expr),
};

pub const Expr = union(ExprTag) {
    list: std.ArrayList(*Expr),
    identifier: std.ArrayList(u8),
    string: std.ArrayList(u8),
    number: f64,
    func: Call,
    macro: Call,
    native_func: usize,
    native_macro: usize,

    pub fn deinit(self: Expr) void {
        switch (self) {
            .list => |list| list.deinit(),
            .identifier, .string => |string| string.deinit(),
            .func, .macro => |call| {
                call.params.deinit();
                call.body.deinit();
            },
            .number, .native_func, .native_macro => {},
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
            .number => |number| try writer.print("{d:}", .{number}),
            .func => |call| {
                _ = try writer.write("(func (");
                if (call.params.items.len > 0) {
                    for (call.params.items[0 .. call.params.items.len - 1]) |param| try writer.print("{} ", .{param});
                    try writer.print("{}", .{call.params.items[call.params.items.len - 1]});
                }
                _ = try writer.write(")");
                if (call.body.items.len > 0) {
                    for (call.body.items) |expr| try writer.print("\n{}", .{expr});
                }
                _ = try writer.write(")");
            },
            .macro => |call| {
                _ = try writer.write("(macro (");
                if (call.params.items.len > 0) {
                    for (call.params.items[0 .. call.params.items.len - 1]) |param| try writer.print("{} ", .{param});
                    try writer.print("{}", .{call.params.items[call.params.items.len - 1]});
                }
                _ = try writer.write(")");
                if (call.body.items.len > 0) {
                    for (call.body.items) |expr| try writer.print("\n{}", .{expr});
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
    func,
    macro,
    native_func,
    native_macro,
};

pub const Interpreter = struct {
    allocator: *std.mem.Allocator,
    scope: std.StringHashMap(*Expr),
    memory: std.ArrayList(*Expr),
    parent: ?*Interpreter,

    pub fn init(allocator: *std.mem.Allocator, parent: ?*Interpreter) Interpreter {
        return Interpreter{
            .allocator = allocator,
            .scope = std.StringHashMap(*Expr).init(allocator),
            .memory = std.ArrayList(*Expr).init(allocator),
            .parent = parent,
        };
    }

    pub fn deinit(self: *Interpreter) void {
        self.scope.deinit();
        for (self.memory.items) |branch| {
            branch.deinit();
            self.allocator.destroy(branch);
        }
        self.memory.deinit();
    }

    pub fn eval(self: *Interpreter, expr: *Expr) anyerror!*Expr {
        switch (expr.*) {
            .list => |list| {
                if (list.items.len == 0) return error.InterpreterEmptyList;
                const called = try self.eval(list.items[0]);
                switch (called.*) {
                    .native_macro => |address| return try @intToPtr(NativeCall, address)(self, list.items[1..]),
                    .native_func => |address| {
                        var args_list = try std.ArrayList(*Expr).initCapacity(self.allocator, list.items.len - 1);
                        defer args_list.deinit();
                        for (list.items[1..]) |arg| try args_list.append(try self.eval(arg));
                        return try @intToPtr(NativeCall, address)(self, args_list.items);
                    },
                    .func => |func| {
                        if (func.params.items.len != list.items.len - 1) return error.InterpreterFuncParameterMismatch;
                        if (func.body.items.len < 1) return error.InterpreterFuncNoBody;
                        var sub_interpreter = Interpreter.init(self.allocator, self);
                        defer sub_interpreter.deinit();
                        for (func.params.items) |param, i| switch (param.*) {
                            .identifier => |identifier| try sub_interpreter.scope.put(identifier.items, try sub_interpreter.eval(list.items[i + 1])),
                            else => return error.InterpreterFuncInvalidParameter,
                        };
                        for (func.body.items[0 .. func.body.items.len - 1]) |body_expr| _ = try sub_interpreter.eval(body_expr);
                        return self.steal(try sub_interpreter.eval(func.body.items[func.body.items.len - 1]));
                    },
                    .macro => |macro| {
                        if (macro.params.items.len != list.items.len - 1) return error.InterpreterMacroParameterMismatch;
                        if (macro.body.items.len < 1) return error.InterpreterMacroNoBody;
                        var sub_interpreter = Interpreter.init(self.allocator, self);
                        defer sub_interpreter.deinit();
                        for (macro.params.items) |param, i| switch (param.*) {
                            .identifier => |identifier| try sub_interpreter.scope.put(identifier.items, list.items[i + 1]),
                            else => return error.InterpreterMacroInvalidParameter,
                        };
                        for (macro.body.items[0 .. macro.body.items.len - 1]) |body_expr| _ = try sub_interpreter.eval(body_expr);
                        return self.steal(try sub_interpreter.eval(macro.body.items[macro.body.items.len - 1]));
                    },
                    else => return error.InterpreterNoSuchFunction,
                }
            },
            .identifier => |identifier| {
                if (self.get(identifier.items)) |value| return value;
                return error.InterpreterNoSuchIdentifier;
            },
            .number, .string, .func, .macro, .native_func, .native_macro => return expr,
        }
    }

    pub fn get(self: Interpreter, identifier: []const u8) ?*Expr {
        if (self.scope.getEntry(identifier)) |entry| return entry.value;
        if (self.parent) |real_parent| return real_parent.get(identifier);
        return null;
    }

    pub fn store(self: *Interpreter, expr: Expr) !*Expr {
        var stored = try self.allocator.create(Expr);
        errdefer self.allocator.destroy(stored);
        try self.memory.append(stored);
        stored.* = expr;
        return stored;
    }

    pub fn clone(self: *Interpreter, expr: *Expr) anyerror!*Expr {
        switch (expr.*) {
            .string, .identifier => |string| {
                var string_copy = std.ArrayList(u8).init(self.allocator);
                errdefer string_copy.deinit();
                try string_copy.appendSlice(string.items);
                switch (expr.*) {
                    .string => return try self.store(.{ .string = string_copy }),
                    .identifier => return try self.store(.{ .identifier = string_copy }),
                    else => unreachable,
                }
            },
            .number => |_| return try self.store(expr.*),
            .native_func, .native_macro => |_| return try self.store(expr.*),
            .list => |list| {
                var list_copy = try std.ArrayList(*Expr).initCapacity(self.allocator, list.items.len);
                errdefer list_copy.deinit();
                for (list.items) |branch| try list_copy.append(try self.clone(branch));
                return try self.store(.{ .list = list_copy });
            },
            .func, .macro => |call| {
                var params_copy = try std.ArrayList(*Expr).initCapacity(self.allocator, call.params.items.len);
                errdefer params_copy.deinit();
                for (call.params.items) |branch| try params_copy.append(try self.clone(branch));
                var body_copy = try std.ArrayList(*Expr).initCapacity(self.allocator, call.body.items.len);
                errdefer body_copy.deinit();
                for (call.body.items) |branch| try body_copy.append(try self.clone(branch));
                switch (expr.*) {
                    .func => return try self.store(.{ .func = .{ .params = params_copy, .body = body_copy } }),
                    .macro => return try self.store(.{ .macro = .{ .params = params_copy, .body = body_copy } }),
                    else => unreachable,
                }
            },
        }
    }

    pub fn steal(self: *Interpreter, expr: *Expr) anyerror!*Expr {
        switch (expr.*) {
            .string, .identifier => |_| {
                const string_stolen = std.ArrayList(u8).fromOwnedSlice(expr.string.allocator, expr.string.toOwnedSlice());
                switch (expr.*) {
                    .string => return try self.store(.{ .string = string_stolen }),
                    .identifier => return try self.store(.{ .identifier = string_stolen }),
                    else => unreachable,
                }
            },
            .number => |_| return try self.store(expr.*),
            .native_func, .native_macro => |_| return try self.store(expr.*),
            .list => |_| {
                var list_stolen = std.ArrayList(*Expr).fromOwnedSlice(expr.list.allocator, expr.list.toOwnedSlice());
                errdefer list_stolen.deinit();
                for (list_stolen.items) |branch| _ = try self.steal(branch);
                return try self.store(.{ .list = list_stolen });
            },
            .func, .macro => |_| {
                var params_stolen = std.ArrayList(*Expr).fromOwnedSlice(expr.func.params.allocator, expr.func.params.toOwnedSlice());
                errdefer params_stolen.deinit();
                for (params_stolen.items) |branch| _ = try self.steal(branch);
                var body_stolen = std.ArrayList(*Expr).fromOwnedSlice(expr.func.body.allocator, expr.func.body.toOwnedSlice());
                errdefer body_stolen.deinit();
                for (body_stolen.items) |branch| _ = try self.steal(branch);
                switch (expr.*) {
                    .func => return try self.store(.{ .func = .{ .params = params_stolen, .body = body_stolen } }),
                    .macro => return try self.store(.{ .func = .{ .params = params_stolen, .body = body_stolen } }),
                    else => unreachable,
                }
            },
        }
    }
};
