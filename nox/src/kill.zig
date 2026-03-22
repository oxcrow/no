const std = @import("std");
const lib = @import("lib.zig");
const dbg = lib.dbg;

pub const ErrorCode = enum {
    ArgCommandNotFound,
    ArgFileNotFound,
    ArgKeyNotFound,
    SysNotEnoughCPU,
    LexUnknownToken,
    LexCarraigeReturnUsed,
    LexTabsUsedForIndentation,
    LexTabsUsedForAlignment,
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
        .ArgCommandNotFound => {
            dbg.print("{s}\n", .{try dbg.xdot(&buf, "Perhaps you forgot to mention any command?")});
            dbg.print("{s}\n", .{try dbg.xwhy(&buf, "I need a command to run the compiler.")});
            dbg.print("{s}\n", .{try dbg.xspc(&buf, "")});
            dbg.print("{s}\n", .{try dbg.xtip(&buf, "Use commands such as build, run, etc.")});
            dbg.print("{s}\n", .{try dbg.xspc(&buf, "$ no build <file>.no")});
            dbg.print("{s}\n", .{try dbg.xspc(&buf, "$ no run <file>.no")});
            dbg.print("{s}\n", .{try dbg.xspc(&buf, "")});
            dbg.print("{s}\n", .{try dbg.xtip(&buf, "See $ no --help")});
        },
        .ArgFileNotFound => {
            dbg.print("{s}\n", .{try dbg.xdot(&buf, "Unable to parse file path from arguments.")});
            dbg.print("{s}\n", .{try dbg.xwhy(&buf, "I tried to find a file; but failed.")});
            dbg.print("{s}\n", .{try dbg.xspc(&buf, "")});
            dbg.print("{s}\n", .{try dbg.xtip(&buf, "Use commands such as this.")});
            dbg.print("{s}\n", .{try dbg.xspc(&buf, "$ no build <file>.no")});
            dbg.print("{s}\n", .{try dbg.xspc(&buf, "")});
            dbg.print("{s}\n", .{try dbg.xtip(&buf, "See $ no --help")});
        },
        .ArgKeyNotFound => {
            dbg.print("{s}\n", .{try dbg.xdot(&buf, "Unable to parse expected argument key.")});
            dbg.print("{s}\n", .{try dbg.xwhy(&buf, "I tried to find a key; but failed.")});
            dbg.print("{s}\n", .{try dbg.xspc(&buf, "")});
            if (args.len == 1) {
                dbg.print("{s}\n", .{try dbg.xtip(&buf, try dbg.xfmt(&buf1, "I expected to find \"{s}\" in command line arguments.", .{args[0]}))});
                dbg.print("{s}\n", .{try dbg.xspc(&buf, "Perhaps you forgot to include the argument?")});
                dbg.print("{s}\n", .{try dbg.xspc(&buf, "")});
            } else {
                @compileError("lib.crash: Did you forget argument?");
            }
            dbg.print("{s}\n", .{try dbg.xtip(&buf, "See $ no --help")});
        },
        .SysNotEnoughCPU => {
            dbg.print("{s}\n", .{try dbg.xdot(&buf, "Unable to allocate the requested number of threads.")});
            dbg.print("{s}\n", .{try dbg.xwhy(&buf, "I tried to parse threads; but failed.")});
            dbg.print("{s}\n", .{try dbg.xspc(&buf, "")});
            dbg.print("{s}\n", .{try dbg.xtip(&buf, "Available CPUs aren't enough for requested threads.")});
            if (args.len == 2) {
                dbg.print("{s}\n", .{try dbg.xspc(&buf, try dbg.xfmt(&buf1, "+ Number of available CPUs:    {d:4}", .{args[0]}))});
                dbg.print("{s}\n", .{try dbg.xspc(&buf, try dbg.xfmt(&buf1, "+ Number of requested threads: {d:4}", .{args[1]}))});
                dbg.print("{s}\n", .{try dbg.xspc(&buf, "")});
            } else {
                @compileError("lib.crash: Did you forget argument?");
            }
            dbg.print("{s}\n", .{try dbg.xtip(&buf, "Perhaps try to use less threads?")});
        },
        .LexUnknownToken => {
            dbg.print("{s}\n", .{try dbg.xdot(&buf, "Unable to tokenize unknown symbol in the code.")});
            dbg.print("{s}\n", .{try dbg.xwhy(&buf, "I found a symbol that's not in my grammar.")});
            dbg.print("{s}\n", .{try dbg.xspc(&buf, "")});
            if (args.len == 3) {
                dbg.print("{s}\n", .{try dbg.xtip(&buf, try dbg.xfmt(&buf1, "Around (LineNumber: {d}, ColumnNumber: {d})", .{ args[1], args[2] }))});
                dbg.print("{s}", .{try dbg.xspc(&buf, "")});
                try dbg.dump(args[0], args[1], args[2]);
                dbg.print("\n", .{});
                dbg.print("{s}\n", .{try dbg.xspc(&buf, "")});
            } else {
                @compileError("lib.crash: Did you forget argument?");
            }
            dbg.print("{s}\n", .{try dbg.xtip(&buf, "Perhaps try to see here?")});
        },
        .LexCarraigeReturnUsed => {
            dbg.print("{s}\n", .{try dbg.xdot(&buf, "Unable to allow carraige returns for new lines.")});
            dbg.print("{s}\n", .{try dbg.xwhy(&buf, "I found a carraige return symbol (\\r).")});
            dbg.print("{s}\n", .{try dbg.xspc(&buf, "")});
            if (args.len == 3) {
                dbg.print("{s}\n", .{try dbg.xtip(&buf, try dbg.xfmt(&buf1, "Around (LineNumber: {d}, ColumnNumber: {d})", .{ args[1], args[2] }))});
                dbg.print("{s}\n", .{try dbg.xspc(&buf, "")});
            } else {
                @compileError("lib.crash: Did you forget argument?");
            }
            dbg.print("{s}\n", .{try dbg.xtip(&buf, "Perhaps try to see here?")});
        },
        .LexTabsUsedForIndentation => {
            dbg.print("{s}\n", .{try dbg.xdot(&buf, "Unable to allow tabs for indentation.")});
            dbg.print("{s}\n", .{try dbg.xwhy(&buf, "I found a tab symbol (\\t).")});
            dbg.print("{s}\n", .{try dbg.xspc(&buf, "")});
            if (args.len == 3) {
                dbg.print("{s}\n", .{try dbg.xtip(&buf, try dbg.xfmt(&buf1, "Around (LineNumber: {d}, ColumnNumber: {d})", .{ args[1], args[2] }))});
                dbg.print("{s}", .{try dbg.xspc(&buf, "")});
                try dbg.dump(args[0], args[1], args[2]);
                dbg.print("\n", .{});
                dbg.print("{s}\n", .{try dbg.xspc(&buf, "")});
            } else {
                @compileError("lib.crash: Did you forget argument?");
            }
            dbg.print("{s}\n", .{try dbg.xtip(&buf, "Perhaps try to see here?")});
        },
        .LexTabsUsedForAlignment => {
            dbg.print("{s}\n", .{try dbg.xdot(&buf, "Unable to allow tabs for alignment.")});
            dbg.print("{s}\n", .{try dbg.xwhy(&buf, "I found a tab symbol (\\t).")});
            dbg.print("{s}\n", .{try dbg.xspc(&buf, "")});
            if (args.len == 3) {
                dbg.print("{s}\n", .{try dbg.xtip(&buf, try dbg.xfmt(&buf1, "Around (LineNumber: {d}, ColumnNumber: {d})", .{ args[1], args[2] }))});
                dbg.print("{s}\n", .{try dbg.xspc(&buf, "")});
            } else {
                @compileError("lib.crash: Did you forget argument?");
            }
            dbg.print("{s}\n", .{try dbg.xtip(&buf, "Perhaps try to see here?")});
        },
        else => {
            @compileError("lib.crash: Did you forget to handle a case?");
        },
    }
    try dbg.end();
    try dbg.loc(src);
    try dbg.eco(code);
    dbg.exit(.Failure);
}
