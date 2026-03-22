const std = @import("std");
const lib = @import("lib.zig");

pub const print = std.debug.print;
pub const xfmt = std.fmt.bufPrint;

pub fn xdot(buf: []u8, msg: []const u8) ![]const u8 {
    return try xfmt(buf, "╭───{s}{s}{s}─ {s}", .{ red(), dot(), rst(), msg }); // U+256D ╭
}
pub fn xwhy(buf: []u8, msg: []const u8) ![]const u8 {
    return try xfmt(buf, "{s}   {s}{s} {s}{s}{s}", .{ vbar(), bend(), hbar(), itl(), msg, rst() });
}
pub fn xtip(buf: []u8, msg: []const u8) ![]const u8 {
    return try xfmt(buf, "{s}   {s}  {s}", .{ vbar(), tip(), msg });
}
pub fn xspc(buf: []u8, msg: []const u8) ![]const u8 {
    return try xfmt(buf, "{s}      {s}", .{ vbar(), msg });
}
pub fn xend(buf: []u8) ![]const u8 {
    return try xfmt(buf, "{s}{s}{s}{s}╮", .{ bend(), hbar(), hbar(), hbar() }); // U+256E ╮
}

pub fn dot() []const u8 {
    return "●"; // U+25CF
}
pub fn tip() []const u8 {
    return "○"; // U+25C7
}
pub fn war() []const u8 {
    return "⊗"; // U+2297
}

pub fn vbar() []const u8 {
    return "│"; // U+2502
}
pub fn bend() []const u8 {
    return "╰"; // U+2570
}
pub fn hbar() []const u8 {
    return "─"; // U+2500
}

pub fn end() !void {
    var buf: [256]u8 = undefined;
    print("{s}\n", .{try xend(&buf)});
}

/// Print compiler source code location
pub fn loc(pos: std.builtin.SourceLocation) !void {
    print("{s}{s} This error was raised from compiler source,\n", .{ bend(), hbar() });
    print("   {s}(File: \"{s}\", LineNumber: {d}){s}\n", .{ itl(), pos.file, pos.line, rst() });
    print("   {s}(You could ignore this section){s}\n", .{ itl(), rst() });
}

/// Print error code
/// Instead of printing an integer code like most languages,
/// print the enum's name for improving user/developer experience.
pub fn eco(code: lib.kill.ErrorCode) !void {
    print("   ({s}{s}{s})\n", .{ itl(), @tagName(code), rst() });
}

/// Exit with error code
pub fn exit(code: enum { Failure, Success }) noreturn {
    switch (code) {
        .Failure => std.process.exit(1),
        .Success => std.process.exit(0),
    }
    unreachable;
}

pub fn dump(text: []const u8, line_index: usize, column_index: usize) !void {
    var iline: usize = 1;
    var icol: usize = 1;
    var mark: bool = false;
    for (text) |c| {
        if (c == '\n' and iline == line_index - 1) {
            print("{d:5} ┊ ", .{iline});
        }
        if (iline == line_index and c != '\n') {
            if (icol == column_index - 1) {
                if (c == ' ' or c == '\t') {
                    print("{s}{c}{s}", .{ red(), '+', rst() });
                } else {
                    print("{s}{c}{s}", .{ red(), c, rst() });
                }
                mark = true;
            } else {
                if (mark) {
                    print("{s}{c}", .{ red(), c });
                } else {
                    print("{c}", .{c});
                }
            }
        }
        if (c == '\n') {
            print("{s}", .{rst()});
            iline += 1;
            icol = 1;
        } else {
            icol += 1;
        }
    }
    print("{s}", .{rst()});
}

pub fn ensure(condition: bool) !void {
    try std.testing.expect(condition);
}

fn red() []const u8 {
    return "\x1b[31m";
}
fn rst() []const u8 {
    return "\x1b[0m";
}
fn itl() []const u8 {
    return "\x1b[3m";
}
fn uln() []const u8 {
    return "\x1b[4m";
}
