const std = @import("std");
const lib = @import("lib.zig");
const dbg = lib.dbg;

pub const Compiler = struct {
    cfg: struct {
        mode: enum { Standard, Relaxed } = .Standard,
        cmd: enum { Build, Clean, Perf, Run },
        cmdBuild: ?struct {
            mode: enum { Parse, Analyse, Compile } = .Compile,
            emit: enum { QBE, LLVM, X86 } = .QBE,
            perf: bool = false,
        },
        limits: struct {
            maxFileSize: usize = 10e6, // 10 MB
            maxMemAllow: usize = 500e6, // 500 MB
        } = .{},
    },

    pub fn deinit(self: @This()) void {
        lib.ignore(self);
    }
};
