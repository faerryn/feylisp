const std = @import("std");

pub const NativeCall = fn (*Interpreter, []Expr) anyerror!Expr;
pub const Call = struct {
    params: *std.ArrayList(Expr),
    body: *std.ArrayList(Expr),
};

pub const Expr = union(ExprTag) {
    list: *std.ArrayList(Expr),
    identifier: *std.ArrayList(u8),
    string: *std.ArrayList(u8),
    number: f64,
    boolean: bool,
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
            .number, .boolean, .native_func, .native_macro => {},
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
                    for (list.items[0 .. list.items.len - 1]) |expr| try writer.print("{} ", .{expr});
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
            .boolean => |boolean| try writer.print("{}", .{boolean}),
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
            .native_func => |native_call| try writer.print("func@{}", .{native_call}),
            .native_macro => |native_call| try writer.print("macro@{}", .{native_call}),
        }
    }
};

pub const ExprTag = enum {
    list,
    identifier,
    string,
    number,
    boolean,
    func,
    macro,
    native_func,
    native_macro,
};

pub const Interpreter = struct {
    allocator: *std.mem.Allocator,
    heap: std.ArrayList(Expr),
    scope: std.StringHashMap(Expr),
    parent: ?*Interpreter,

    pub fn init(allocator: *std.mem.Allocator, parent: ?*Interpreter) Interpreter {
        return Interpreter{
            .allocator = allocator,
            .heap = std.ArrayList(Expr).init(allocator),
            .scope = std.StringHashMap(Expr).init(allocator),
            .parent = parent,
        };
    }

    pub fn deinit(self: *Interpreter) void {
        self.scope.deinit();
        for (self.heap.items) |branch| {
            branch.deinit();
            switch (branch) {
                .list => |list| self.allocator.destroy(list),
                .identifier, .string => |list| self.allocator.destroy(list),
                .func, .macro => |call| {
                    self.allocator.destroy(call.params);
                    self.allocator.destroy(call.body);
                },
                .number, .boolean, .native_func, .native_macro => {},
            }
        }
        self.heap.deinit();
    }

    pub fn eval(self: *Interpreter, expr: Expr) anyerror!Expr {
        switch (expr) {
            .list => |list| {
                if (list.items.len == 0) return expr;
                const called = try self.eval(list.items[0]);
                switch (called) {
                    .native_macro => |native_call| return try @intToPtr(NativeCall, native_call)(self, list.items[1..]),
                    .native_func => |native_call| {
                        var args_list = try std.ArrayList(Expr).initCapacity(self.allocator, list.items.len - 1);
                        defer args_list.deinit();
                        for (list.items[1..]) |arg| try args_list.append(try self.eval(arg));
                        return try @intToPtr(NativeCall, native_call)(self, args_list.items);
                    },
                    .func => |func| {
                        if (func.params.items.len != list.items.len - 1) return error.InterpreterFuncParameterMismatch;
                        if (func.body.items.len < 1) return error.InterpreterFuncNoBody;
                        var sub_interpreter = Interpreter.init(self.allocator, self);
                        defer sub_interpreter.deinit();
                        for (func.params.items) |param, i| switch (param) {
                            .identifier => |identifier| try sub_interpreter.scope.put(identifier.items, try sub_interpreter.eval(list.items[i + 1])),
                            else => return error.InterpreterFuncInvalidParameter,
                        };
                        for (func.body.items[0 .. func.body.items.len - 1]) |body_expr| _ = try sub_interpreter.eval(body_expr);
                        return try self.clone(try sub_interpreter.eval(func.body.items[func.body.items.len - 1]));
                    },
                    .macro => |macro| {
                        if (macro.params.items.len != list.items.len - 1) return error.InterpreterMacroParameterMismatch;
                        if (macro.body.items.len < 1) return error.InterpreterMacroNoBody;
                        var sub_interpreter = Interpreter.init(self.allocator, self);
                        defer sub_interpreter.deinit();
                        for (macro.params.items) |param, i| switch (param) {
                            .identifier => |identifier| try sub_interpreter.scope.put(identifier.items, list.items[i + 1]),
                            else => return error.InterpreterMacroInvalidParameter,
                        };
                        for (macro.body.items[0 .. macro.body.items.len - 1]) |body_expr| _ = try sub_interpreter.eval(body_expr);
                        return try self.clone(try sub_interpreter.eval(macro.body.items[macro.body.items.len - 1]));
                    },
                    else => return error.InterpreterNoSuchFunction,
                }
            },
            .identifier => |identifier| {
                if (self.get(identifier.items)) |value| return value;
                return error.InterpreterNoSuchIdentifier;
            },
            .number, .boolean, .string, .func, .macro, .native_func, .native_macro => return expr,
        }
    }

    pub fn get(self: Interpreter, identifier: []const u8) ?Expr {
        if (self.scope.getEntry(identifier)) |entry| return entry.value;
        if (self.parent) |real_parent| return real_parent.get(identifier);
        return null;
    }

    pub fn store(self: *Interpreter, expr: Expr) !Expr {
        switch (expr) {
            .list, .string, .identifier, .func, .macro => try self.heap.append(expr),
            .number, .boolean, .native_func, .native_macro => return error.StoreNotHeapAllocated,
        }
        return expr;
    }

    pub fn clone(self: *Interpreter, expr: Expr) anyerror!Expr {
        switch (expr) {
            .number, .boolean, .native_func, .native_macro => return expr,
            .string => |string| {
                var copy = std.ArrayList(u8).init(self.allocator);
                errdefer copy.deinit();
                try copy.appendSlice(string.items);
                return self.store(Expr{ .string = &copy });
            },
            .identifier => |identifier| {
                var copy = std.ArrayList(u8).init(self.allocator);
                errdefer copy.deinit();
                try copy.appendSlice(identifier.items);
                return self.store(Expr{ .identifier = &copy });
            },
            .list => |list| {
                var copy = try std.ArrayList(Expr).initCapacity(self.allocator, list.items.len);
                errdefer copy.deinit();
                for (list.items) |branch| try copy.append(try self.clone(branch));
                return self.store(Expr{ .list = &copy });
            },
            .func => |func| {
                var params = try std.ArrayList(Expr).initCapacity(self.allocator, func.params.items.len);
                errdefer params.deinit();
                for (func.params.items) |branch| try params.append(try self.clone(branch));
                var body = try std.ArrayList(Expr).initCapacity(self.allocator, func.body.items.len);
                errdefer body.deinit();
                for (func.body.items) |branch| try body.append(try self.clone(branch));
                return self.store(Expr{ .func = .{ .params = &params, .body = &body } });
            },
            .macro => |macro| {
                var params = try std.ArrayList(Expr).initCapacity(self.allocator, macro.params.items.len);
                errdefer params.deinit();
                for (macro.params.items) |branch| try params.append(try self.clone(branch));
                var body = try std.ArrayList(Expr).initCapacity(self.allocator, macro.body.items.len);
                errdefer body.deinit();
                for (macro.body.items) |branch| try body.append(try self.clone(branch));
                return self.store(Expr{ .macro = .{ .params = &params, .body = &body } });
            },
        }
    }
};
