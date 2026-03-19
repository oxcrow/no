const std = @import("std");

pub const dbg = @import("dbg.zig");
pub const kill = @import("kill.zig");
pub const dast = @import("core/container.zig");

pub const crash = kill.crash;

pub const Compiler = @import("config.zig").Compiler;
pub const Argument = @import("core/args.zig").Argument;
pub const MultiList = dast.MultiList;
pub const List = dast.List;
pub const Span = dast.Span;

pub fn ignore(x: anytype) void {
    _ = x;
}
