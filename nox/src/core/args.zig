const std = @import("std");

pub const Argument = struct {
    argv: [][:0]u8,

    /// Initialize the argument parser
    ///
    /// Rant: Why do we need a freaking allocator for this? Ask Bill Gates.
    ///
    /// Windows sucks so absolutely much that to even parse command line args,
    /// we need to allocate memory, and extract the arguments as strings.
    /// Thus, to ensure that our code runs correctly on all platforms,
    /// we need to also accept an allocator on all other platforms.
    pub fn init(mem: std.mem.Allocator) !Self {
        const argv = try std.process.argsAlloc(mem);
        return .{
            .argv = argv,
        };
    }

    pub fn getStr(self: Self, index: usize) []const u8 {
        return std.mem.span(self.argv[index].ptr);
    }

    pub fn keyExists(self: Self, key: []const u8) ?bool {
        if (key.len < 3 or key[0] != '-' or key[1] != '-' or key[2] == '-') {
            return null;
        }
        for (self.argv) |arg| {
            const slice = std.mem.span(arg.ptr);
            if (slice.len >= key.len) {
                if (std.mem.eql(u8, slice[0..key.len], key)) {
                    return true;
                }
            }
        }
        return false;
    }

    pub fn shortKeyExists(self: Self, key: []const u8) ?bool {
        if (key.len > 2 or key[0] != '-' or key[1] == '-') {
            return null;
        }
        for (self.argv, 0..) |arg, i| {
            const slice = std.mem.span(arg.ptr);
            if (slice.len >= key.len and i > 0) {
                for (slice) |d| {
                    const c = key[1];
                    if (c == d) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    pub fn getValueOfKey(self: Self, key: []const u8) ?[]const u8 {
        for (self.argv, 0..) |arg, i| {
            const slice = std.mem.span(arg.ptr);
            if (slice.len > key.len) {
                if (std.mem.eql(u8, slice[0..key.len], key)) {
                    if (slice[key.len] == '=') {
                        return self.getStr(i)[(key.len + 1)..];
                    }
                }
            }
        }
        return null;
    }

    pub fn deinitConst(self: Self, mem: std.mem.Allocator) void {
        std.process.argsFree(mem, self.argv);
    }

    const Self = @This();
};
