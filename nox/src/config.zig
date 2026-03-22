const std = @import("std");
const lib = @import("lib.zig");
const dbg = lib.dbg;

pub const Mode = enum { Standard, Liberal };

pub const Compiler = struct {
    cfg: struct {
        mode: Mode = .Standard,
        cmd: enum { Build, Clean, Perf, Run },
        cmdBuild: ?struct {
            mode: enum { Parse, Analyse, Compile } = .Compile,
            emit: enum { QBE, LLVM, X86 } = .QBE,
            perf: bool = false,
            threads: usize = 1,
        },
        limits: struct {
            maxFileSize: usize = 10e6, // 10 MB
            maxMemAllow: usize = 500e6, // 500 MB
        } = .{},
        debug: bool = false,
    },

    pub fn deinit(self: @This()) void {
        lib.ignore(self);
    }
};
