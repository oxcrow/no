const std = @import("std");
const nox = @import("nox");
const lib = nox.lib;
const dbg = lib.dbg;

fn dev(mem: std.mem.Allocator) !void {
    const args = try lib.Argument.init(mem);
    defer args.deinitConst(mem);

    if (args.keyExists("--help").? or args.shortKeyExists("-h").?) {
        try nox.header();
        try nox.usage();
        return;
    }

    const com, const file = x: {
        const cmd_word = if (args.argv.len == 1) {
            lib.crash(.ArgCommandNotFound, @src(), .{});
        } else y: {
            const word = std.mem.span(args.argv[1].ptr);
            if (!std.mem.eql(u8, word, "build")) {
                lib.crash(.ArgCommandNotFound, @src(), .{});
            }
            break :y word;
        };

        const file = y: {
            for (args.argv, 0..) |arg, i| {
                if (i > 1) {
                    const word = std.mem.span(arg.ptr);
                    if (word[0] != '-') {
                        break :y word;
                    }
                }
            }
            lib.crash(.ArgFileNotFound, @src(), .{});
        };

        const com = lib.Compiler{
            .cfg = .{
                .cmd = y: {
                    if (std.mem.eql(u8, cmd_word, "build")) {
                        break :y .Build;
                    }
                    unreachable;
                },
                .cmdBuild = .{ .mode = z: {
                    if (args.keyExists("--parse").?) {
                        break :z .Parse;
                    } else if (args.keyExists("--analyze").?) {
                        break :z .Analyse;
                    } else if (args.keyExists("--compile").?) {
                        break :z .Compile;
                    }
                    break :z .Compile;
                }, .emit = z: {
                    const backend = args.getValueOfKey("--emit") orelse {
                        break :z .QBE;
                    };
                    if (std.mem.eql(u8, backend, "QBE")) {
                        break :z .QBE;
                    } else if (std.mem.eql(u8, backend, "LLVM")) {
                        break :z .LLVM;
                    } else {
                        @panic("Unsupported emitter backend!");
                    }
                    break :z .QBE;
                }, .threads = z: {
                    const num_cpus = try std.Thread.getCpuCount();
                    const threads = try std.fmt.parseInt(
                        usize,
                        args.getValueOfKey("--threads") orelse {
                            break :z 1;
                        },
                        10,
                    );
                    if (threads > num_cpus) {
                        lib.crash(.SysNotEnoughCPU, @src(), .{ num_cpus, threads });
                    }
                    std.debug.print("{d}\n", .{threads});
                    break :z threads;
                } },
            },
        };

        break :x .{ com, file };
    };

    try nox.build(mem, com, file);

    try nox.bye(com);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    const mem = gpa.allocator();

    try dev(mem);

    const leaked = gpa.detectLeaks();
    if (leaked) {
        std.debug.print("Has memory leaks!\n", .{});
    }
}
