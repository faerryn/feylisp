const std = @import("std");

pub const LispCall = struct {
    params: std.ArrayList(LispExpr),
    body: std.ArrayList(LispExpr),
};

pub const LispClosure = struct {
    call: LispCall,
    interpreter: LispInterpreter,
};

pub const LispNativeCall = fn (*LispInterpreter, []LispExpr) anyerror!LispExpr;

pub const LispExpr = union(enum) {
    list: *std.ArrayList(LispExpr),
    identifier: *std.ArrayList(u8),
    string: *std.ArrayList(u8),
    number: isize,
    t,
    nil,
    func: *LispClosure,
    macro: *LispCall,
    native_func: usize,
    native_macro: usize,

    pub fn deinit(self: LispExpr) void {
        switch (self) {
            .list => |list| list.deinit(),
            .identifier, .string => |string| string.deinit(),
            .macro => |call| {
                call.params.deinit();
                call.body.deinit();
            },
            .func => |closure| {
                closure.call.params.deinit();
                closure.call.body.deinit();
                closure.interpreter.deinit();
            },
            .number, .t, .nil, .native_func, .native_macro => {},
        }
    }

    pub fn format(
        self: LispExpr,
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
            .t => _ = try writer.write("t"),
            .nil => _ = try writer.write("nil"),
            .func => |closure| {
                _ = try writer.write("(func (");
                if (closure.call.params.items.len > 0) 
                    for (closure.call.params.items) |param| try writer.print("{} ", .{param});
                _ = try writer.write(") (");
                if (closure.interpreter.scope.count() > 0) {
                    var it = closure.interpreter.scope.iterator();
                    while (it.next()) |capture| try writer.print("{}={} ", .{capture.key, capture.value});
                }
                _ = try writer.write(")");
                if (closure.call.body.items.len > 0) for (closure.call.body.items) |expr| try writer.print("\n{}", .{expr});
                _ = try writer.write(")");
            },
            .macro => |call| {
                _ = try writer.write("(macro (");
                if (call.params.items.len > 0) for (call.params.items) |param| try writer.print("{} ", .{param});
                _ = try writer.write(")");
                if (call.body.items.len > 0) for (call.body.items) |expr| try writer.print("\n{}", .{expr});
                _ = try writer.write(")");
            },
            .native_func => |native_call| try writer.print("func@{}", .{native_call}),
            .native_macro => |native_call| try writer.print("macro@{}", .{native_call}),
        }
    }
};

pub const LispInterpreter = struct {
    allocator: *std.mem.Allocator,
    heap: std.ArrayList(LispExpr),
    scope: std.StringHashMap(LispExpr),
    parent: ?*LispInterpreter,

    pub fn init(allocator: *std.mem.Allocator, parent: ?*LispInterpreter) LispInterpreter {
        return LispInterpreter{
            .allocator = allocator,
            .heap = std.ArrayList(LispExpr).init(allocator),
            .scope = std.StringHashMap(LispExpr).init(allocator),
            .parent = parent,
        };
    }

    pub fn deinit(self: *LispInterpreter) void {
        self.scope.deinit();
        for (self.heap.items) |branch| {
            branch.deinit();
            switch (branch) {
                .number, .t, .nil, .native_func, .native_macro => {},
                .list => |list| self.allocator.destroy(list),
                .identifier, .string => |list| self.allocator.destroy(list),
                .macro => |call| self.allocator.destroy(call),
                .func => |closure| self.allocator.destroy(closure),
            }
        }
        self.heap.deinit();
    }

    pub fn eval(self: *LispInterpreter, expr: LispExpr) anyerror!LispExpr {
        switch (expr) {
            .number, .t, .nil, .string, .func, .macro, .native_func, .native_macro => return expr,
            .list => |list| {
                if (list.items.len == 0) return expr;
                const called = try self.eval(list.items[0]);
                switch (called) {

                    .native_macro => |native_call| return try @intToPtr(LispNativeCall, native_call)(self, list.items[1..]),

                    .native_func => |native_call| {
                        var args_list = try std.ArrayList(LispExpr).initCapacity(self.allocator, list.items.len - 1);
                        defer args_list.deinit();
                        for (list.items[1..]) |arg| try args_list.append(try self.eval(arg));
                        return try @intToPtr(LispNativeCall, native_call)(self, args_list.items);
                    },

                    .macro => |macro| {
                        if (macro.params.items.len != list.items.len - 1) return error.InterpreterMacroParameterMismatch;
                        if (macro.body.items.len < 1) return error.InterpreterMacroNoBody;

                        var sub_interpreter = LispInterpreter.init(self.allocator, self);
                        defer sub_interpreter.deinit();

                        for (macro.params.items) |param, i| switch (param) {
                            .identifier => |identifier| try sub_interpreter.scope.put(identifier.items, list.items[i + 1]),
                            else => return error.InterpreterMacroInvalidParameter,
                        };

                        for (macro.body.items[0 .. macro.body.items.len - 1]) |body_expr| _ = try sub_interpreter.eval(body_expr);

                        return try self.clone(try sub_interpreter.eval(macro.body.items[macro.body.items.len - 1]));
                    },

                    .func => |func| {
                        if (func.call.params.items.len != list.items.len - 1) return error.InterpreterFuncParameterMismatch;
                        if (func.call.body.items.len < 1) return error.InterpreterFuncNoBody;

                        var sub_interpreter = LispInterpreter.init(self.allocator, self);
                        defer sub_interpreter.deinit();

                        var it = func.interpreter.scope.iterator();
                        while (it.next()) |entry| try sub_interpreter.scope.put(entry.key, try sub_interpreter.clone(entry.value));

                        for (func.call.params.items) |param, i| switch (param) {
                            .identifier => |identifier| try sub_interpreter.scope.put(identifier.items, try sub_interpreter.eval(list.items[i + 1])),
                            else => return error.InterpreterFuncInvalidParameter,
                        };

                        for (func.call.body.items[0 .. func.call.body.items.len - 1]) |body_expr| _ = try sub_interpreter.eval(body_expr);

                        return try self.clone(try sub_interpreter.eval(func.call.body.items[func.call.body.items.len - 1]));
                    },

                    else => return error.InterpreterNoSuchFunction,
                }
            },
            .identifier => |identifier| {
                if (self.get(identifier.items)) |value| return value;
                return error.InterpreterNoSuchIdentifier;
            },
        }
    }

    pub fn get(self: LispInterpreter, identifier: []const u8) ?LispExpr {
        if (self.scope.getEntry(identifier)) |entry| return entry.value;
        if (self.parent) |real_parent| return real_parent.get(identifier);
        return null;
    }

    pub fn store(self: *LispInterpreter, expr: LispExpr) !LispExpr {
        switch (expr) {
            .list, .string, .identifier, .func, .macro => try self.heap.append(expr),
            .number, .t, .nil, .native_func, .native_macro => return error.StoreNotHeapAllocated,
        }
        return expr;
    }

    pub fn clone(self: *LispInterpreter, expr: LispExpr) anyerror!LispExpr {
        switch (expr) {
            .number, .t, .nil, .native_func, .native_macro => return expr,

            .string, .identifier => |string| {
                var copy = try self.allocator.create(std.ArrayList(u8));
                errdefer self.allocator.destroy(copy);
                copy.* = std.ArrayList(u8).init(self.allocator);
                errdefer copy.deinit();
                try copy.appendSlice(string.items);
                switch (expr) {
                    .string => return self.store(LispExpr{ .string = copy }),
                    .identifier => return self.store(LispExpr{ .identifier = copy }),
                    else => unreachable,
                }
            },

            .list => |list| {
                var copy = try self.allocator.create(std.ArrayList(LispExpr));
                errdefer self.allocator.destroy(copy);
                copy.* = try std.ArrayList(LispExpr).initCapacity(self.allocator, list.items.len);
                errdefer copy.deinit();
                for (list.items) |branch| try copy.append(try self.clone(branch));
                return self.store(LispExpr{ .list = copy });
            },

            .macro => |call| {
                var copy = try self.allocator.create(LispCall);
                errdefer self.allocator.destroy(copy);

                copy.params = try std.ArrayList(LispExpr).initCapacity(self.allocator, call.params.items.len);
                errdefer copy.params.deinit();
                for (call.params.items) |branch| try copy.params.append(try self.clone(branch));

                copy.body = try std.ArrayList(LispExpr).initCapacity(self.allocator, call.body.items.len);
                errdefer copy.body.deinit();
                for (call.body.items) |branch| try copy.body.append(try self.clone(branch));

                return self.store(LispExpr{ .macro = copy });
            },

            .func => |closure| {
                var copy = try self.allocator.create(LispClosure);
                errdefer self.allocator.destroy(copy);

                copy.call.params = try std.ArrayList(LispExpr).initCapacity(self.allocator, closure.call.params.items.len);
                errdefer copy.call.params.deinit();
                for (closure.call.params.items) |branch| try copy.call.params.append(try self.clone(branch));

                copy.call.body = try std.ArrayList(LispExpr).initCapacity(self.allocator, closure.call.body.items.len);
                errdefer copy.call.body.deinit();
                for (closure.call.body.items) |branch| try copy.call.body.append(try self.clone(branch));

                copy.interpreter.allocator = self.allocator;
                copy.interpreter.parent = closure.interpreter.parent;

                copy.interpreter.heap = try std.ArrayList(LispExpr).initCapacity(self.allocator, closure.interpreter.heap.items.len);
                errdefer copy.interpreter.heap.deinit();
                for (closure.interpreter.heap.items) |value| try copy.interpreter.heap.append(try self.clone(value));

                copy.interpreter.scope = std.StringHashMap(LispExpr).init(self.allocator);
                errdefer copy.interpreter.scope.deinit();
                var it = closure.interpreter.scope.iterator();
                while (it.next()) |entry| try copy.interpreter.scope.put(entry.key, try self.clone(entry.value));

                return self.store(LispExpr{ .func = copy });
            },
        }
    }
};
