const std = @import("std");

pub fn part1(input: []const u8) !i32 {
    const alloc = std.heap.page_allocator;
    var sum: i32 = 0;
    var splits = std.mem.split(u8, input, "\n");
    while (splits.next()) |line| {
        var list = std.ArrayList(u8).init(alloc);
        defer list.deinit();
        if (line.len > 0) {
            for (line) |c| {
                if (std.ascii.isDigit(c)) {
                    try list.append(c);
                }
            }
            // std.debug.print("{s} {d}\n", .{ line, line.len });
            // std.debug.print("{s} {d}\n", .{ list.items, list.items.len });
            var slice = [2]u8{ list.items[0], list.items[list.items.len - 1] };
            sum += try std.fmt.parseInt(i32, &slice, 10);
        }
    }
    return sum;
}

pub fn main() !void {
    const stdout = std.io.getStdOut();
    const input = @embedFile("input.txt");

    try stdout.writer().print("part1: {any}\n", .{part1(input)});
}

// const alloc = std.heap.page_allocator;
// var file = try std.fs.cwd().openFile("2023/day1/input.txt", .{});
// defer file.close();
// var buf: [1024]u8 = undefined;
// var buf_reader = std.io.bufferedReader(file.reader());
// var in_stream = buf_reader.reader();
// while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
//     var list = std.ArrayList(u8).init(alloc);
//     defer list.deinit();
//     for (line) |c| {
//         if (std.ascii.isDigit(c)) {
//             try list.append(c);
//         }
//     }
//     var slice = [2]u8{ list.items[0], list.items[list.items.len - 1] };
//     sum += try std.fmt.parseInt(i32, &slice, 10);
// }
// try stdout.writer().print("part1: {d}\n", .{sum});
