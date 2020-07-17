const lex = @import("lex.zig");

pub const Parse = struct {
    pub const Tree = struct {
        id: Id,

        pub const Id = enum {};
    };
};
