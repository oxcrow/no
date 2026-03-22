const std = @import("std");

pub const dbg = @import("dbg.zig");
pub const kill = @import("kill.zig");
pub const container = @import("core/container.zig");
pub const token = @import("base/token.zig");
pub const lexer = @import("base/lexer.zig");

pub const crash = kill.crash;

pub const Mode = @import("config.zig").Mode;
pub const Compiler = @import("config.zig").Compiler;
pub const Argument = @import("core/args.zig").Argument;
pub const MultiList = container.MultiList;
pub const List = container.List;
pub const Span = container.Span;

pub fn ignore(x: anytype) void {
    _ = x;
}
