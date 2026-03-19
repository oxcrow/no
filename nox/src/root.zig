const std = @import("std");
pub const lib = @import("lib.zig");

pub const Compiler = lib.Compiler;

pub fn header() !void {
    std.debug.print("No: A language for workers of the world.\n", .{});
}

pub fn usage() !void {
    std.debug.print(
        \\
        \\Usage: no <COMMAND> <FILE>.no
        \\
        \\FILE:
        \\    An ASCII file with .no extension.
        \\
        \\COMMAND:
        \\    build [BUILD-OPTIONS]         Build module.
        \\    -h --help                     Print this help message.
        \\
        \\BUILD-OPTIONS:
        \\    --emit=<VALUE>                Set the IR code generator backend. (VALUE: [QBE, LLVM] (QBE is the default)).
        \\    --compile                     Lex, parse, semantic analyze, and emit IR code (This is the default).
        \\    --analyze                     Lex, parse, and semantic analyze; but don't do anything else.
        \\    --parse                       Lex and parse; but don't do anything else.
        \\    --perf                        Instrument code and profile performance.
        \\
        \\
    , .{});
}

test {
    std.testing.refAllDeclsRecursive(@This());
}

pub fn build(mem: std.mem.Allocator, com: Compiler, file: []const u8) !void {
    lib.ignore(.{ mem, com, file });
}

pub fn bye(com: Compiler) !void {
    com.deinit();
}
