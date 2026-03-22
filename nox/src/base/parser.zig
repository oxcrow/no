const lib = @import("../lib.zig");

pub fn parse(file: []const u8) !void {
    lib.ignore(.{file});
}
