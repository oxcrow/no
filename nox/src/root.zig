const std = @import("std");

pub fn bye() !void {
    var buffer: [1024]u8 = undefined;
    var writer = std.fs.File.stdout().writer(&buffer);
    const stdout = &writer.interface;

    try stdout.print("", .{});

    try stdout.flush();
}
