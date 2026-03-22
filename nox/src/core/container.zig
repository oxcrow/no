const std = @import("std");
const lib = @import("../lib.zig");
const dbg = lib.dbg;

const ContainerError = error{
    MemoryCapacityLow,
};

pub fn List(comptime T: type) type {
    return struct {
        array: std.ArrayList(T),

        pub fn items(self: Self) []const T {
            return self.array.items;
        }

        pub fn init(allocator: std.mem.Allocator) Self {
            return Self{ .array = std.ArrayList(T).init(allocator) };
        }

        pub fn initCapacity(allocator: std.mem.Allocator, n: usize) !Self {
            return Self{ .array = try std.ArrayList(T).initCapacity(allocator, n) };
        }

        pub fn deinitConst(self: Self, allocator: std.mem.Allocator) void {
            @constCast(&self).deinit(allocator);
        }

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            self.array.deinit(allocator);
        }

        pub fn len(self: Self) usize {
            return self.array.items.len;
        }

        pub fn capacity(self: Self) usize {
            return self.array.capacity;
        }

        pub fn append(self: *Self, allocator: std.mem.Allocator, x: T) !void {
            try self.array.append(allocator, x);
        }

        pub fn appendAssumeCapacity(self: *Self, elem: T) void {
            self.array.appendAssumeCapacity(elem);
        }

        pub fn getLast(self: Self) T {
            return self.array.getLast();
        }

        pub fn at(self: Self, i: usize) T {
            return self.array.items[i];
        }

        pub fn in(self: *Self, i: usize) *T {
            return &self.array.items[i];
        }

        pub fn iterator(self: *const Self) []const T {
            return self.array.items[0..];
        }

        const Self = @This();
    };
}

pub fn MultiList(comptime T: type) type {
    return struct {
        array: std.MultiArrayList(T),

        pub fn items(self: Self) []const T {
            return self.array.items;
        }

        pub fn initCapacity(mem: std.mem.Allocator, n: usize) !Self {
            var array = std.MultiArrayList(T){};
            errdefer array.deinit(mem);
            try array.ensureTotalCapacity(mem, n);
            return Self{ .array = array };
        }

        pub fn deinitConst(self: Self, mem: std.mem.Allocator) void {
            @constCast(&self).deinit(mem);
        }

        pub fn deinit(self: *Self, mem: std.mem.Allocator) void {
            self.array.deinit(mem);
        }

        pub fn len(self: Self) usize {
            return self.array.len;
        }

        pub fn capacity(self: Self) usize {
            return self.array.capacity;
        }

        pub fn append(self: *Self, mem: std.mem.Allocator, x: T) !void {
            try self.array.append(mem, x);
        }

        pub fn appendAssumeCapacity(self: *Self, elem: T) !void {
            if (self.len() < self.capacity()) {
                self.array.appendAssumeCapacity(elem);
            } else {
                return error.MemoryCapacityLow;
            }
        }

        const Self = @This();
    };
}

pub const Span = struct {
    start: u32,
    end: u32,

    pub fn init(start: usize, end: usize) Self {
        return .{
            .start = @truncate(start),
            .end = @truncate(end),
        };
    }

    pub fn len(self: Self) usize {
        return self.end - self.start;
    }

    const Self = @This();
};

test "span" {
    const x = lib.cont.Span.init(0, 3);
    try dbg.ensure(x.len() == 3);
}
