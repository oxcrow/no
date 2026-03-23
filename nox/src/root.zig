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

pub const Database = struct {
    lock: std.Thread.RwLock = .{},
    state: struct {
        files: lib.List(File),
    },

    pub const File = struct {
        text: []const u8,
        index: u32,
        hash: u64,
    };

    pub fn init(mem: std.mem.Allocator) !Self {
        return .{
            .state = .{
                .files = try lib.List(File).initCapacity(mem, 100),
            },
        };
    }

    pub fn deinit(self: *Self, mem: std.mem.Allocator) void {
        for (self.state.files.items()) |file| {
            mem.free(file.text);
        }
        self.state.files.deinit(mem);
    }

    pub fn queryReadFile(self: *Self, file: []const u8) !void {
        const hash = hashString(file);
        const found, const cached = x: {
            self.lock.lockShared();
            self.lock.unlockShared();
            break :x .{ false, null };
        };
        lib.ignore(.{
            &self,  file,  hash, //
            cached, found,
        });
        lib.ignore(.{ &self, file });
    }

    pub fn queryParseFile(self: *Self, file: []const u8) !void {
        lib.ignore(.{ &self, file });
    }

    fn hashString(string: []const u8) u64 {
        const hasher = std.hash.SipHash64(1, 3);
        const hash = hasher.toInt(string, &[_]u8{0} ** 16);
        return hash;
    }

    const Self = @This();
};

pub fn build(mem: std.mem.Allocator, com: Compiler, file: []const u8) !void {
    // Read code into memory
    const code = try std.fs.cwd().readFileAlloc(mem, file, com.cfg.limits.maxFileSize);
    defer mem.free(code);

    // Start thread pool and wait group for parallel data processing
    var threads: std.Thread.Pool = undefined;
    defer threads.deinit();
    try threads.init(.{
        .allocator = mem,
        .n_jobs = @min(com.cfg.cmdBuild.?.threads, try std.Thread.getCpuCount()),
    });
    var wg: std.Thread.WaitGroup = .{};

    // Start compiler database
    var db = try Database.init(mem);
    defer db.deinit(mem);

    // Parse the root file and then parse its dependencies in the same module
    const root = try db.queryReadFile(file);

    // Wait until all work is finished
    wg.wait();

    _ = root;
}

pub fn bye(com: Compiler) !void {
    com.deinit();
}
