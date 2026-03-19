const std = @import("std");
const lib = @import("lib.zig");
const dbg = lib.dbg;

pub const ErrorCode = enum {
    ArgNotFoundCmd,
    ArgNotFoundFile,
    ArgNotFoundKey,
    None,
};

pub fn crash(comptime code: ErrorCode, src: std.builtin.SourceLocation, args: anytype) noreturn {
    fatal(code, src, args) catch unreachable;
    unreachable;
}

fn fatal(comptime code: ErrorCode, src: std.builtin.SourceLocation, args: anytype) !void {
    var buf: [256]u8 = undefined;
    var buf1: [256]u8 = undefined;
    switch (code) {
        .ArgNotFoundCmd => {
            dbg.print("{s}\n", .{try dbg.xdot(&buf, "Perhaps you forgot to mention any command?")});
            dbg.print("{s}\n", .{try dbg.xwhy(&buf, "I need a command to run the compiler.")});
            dbg.print("{s}\n", .{try dbg.xspc(&buf, "")});
            dbg.print("{s}\n", .{try dbg.xtip(&buf, "Use commands such as build, run, etc.")});
            dbg.print("{s}\n", .{try dbg.xspc(&buf, "$ no build <file>.no")});
            dbg.print("{s}\n", .{try dbg.xspc(&buf, "$ no run <file>.no")});
            dbg.print("{s}\n", .{try dbg.xspc(&buf, "")});
            dbg.print("{s}\n", .{try dbg.xtip(&buf, "See $ no --help")});
            dbg.print("{s}\n", .{try dbg.xend(&buf)});
        },
        .ArgNotFoundFile => {
            dbg.print("{s}\n", .{try dbg.xdot(&buf, "Unable to parse file path from arguments.")});
            dbg.print("{s}\n", .{try dbg.xwhy(&buf, "I tried to find a file; but failed.")});
            dbg.print("{s}\n", .{try dbg.xspc(&buf, "")});
            dbg.print("{s}\n", .{try dbg.xtip(&buf, "Use commands such as this.")});
            dbg.print("{s}\n", .{try dbg.xspc(&buf, "$ no build <file>.no")});
            dbg.print("{s}\n", .{try dbg.xspc(&buf, "")});
            dbg.print("{s}\n", .{try dbg.xtip(&buf, "See $ no --help")});
            dbg.print("{s}\n", .{try dbg.xend(&buf)});
        },
        .ArgNotFoundKey => {
            dbg.print("{s}\n", .{try dbg.xdot(&buf, "Unable to parse expected argument key.")});
            dbg.print("{s}\n", .{try dbg.xwhy(&buf, "I tried to find a key; but failed.")});
            dbg.print("{s}\n", .{try dbg.xspc(&buf, "")});
            if (args.len > 0) {
                dbg.print("{s}\n", .{try dbg.xtip(&buf, try dbg.xfmt(&buf1, "I expected to find \"{s}\" in command line arguments.", .{args[0]}))});
                dbg.print("{s}\n", .{try dbg.xspc(&buf, "Perhaps you forgot to include the argument?")});
                dbg.print("{s}\n", .{try dbg.xspc(&buf, "")});
            } else {
                @compileError("lib.crash: Did you forget argument?");
            }
            dbg.print("{s}\n", .{try dbg.xtip(&buf, "See $ no --help")});
            dbg.print("{s}\n", .{try dbg.xend(&buf)});
        },
        else => {
            unreachable;
        },
    }
    try dbg.loc(src);
    try dbg.eco(code);
    dbg.exit(.Failure);
}
