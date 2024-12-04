const std = @import("std");
const print = std.debug.print;

pub fn occurences(list: []const i32, target: i32) i32 {
    var count: i32 = 0;
    for (list) |n| {
        if (n == target) count += 1;
    }
    return count;
}

pub fn part1(input: []const u8) !i64 {
    // This is a safe allocator that can prevent double-free, use-after-free and can detect leaks
    // https://zig.guide/standard-library/allocators
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    var iter = std.mem.splitScalar(u8, input, '\n');
    var list1 = std.ArrayList(i32).init(alloc);
    var list2 = std.ArrayList(i32).init(alloc);
    // The deinit() method frees all of the ArrayList's memory
    defer list1.deinit();
    defer list2.deinit();

    while (iter.next()) |line| {
        var numIter = std.mem.splitScalar(u8, line, ' ');

        const num1 = try std.fmt.parseInt(i32, numIter.next().?, 10);
        // Skip spaces
        _ = numIter.next();
        _ = numIter.next();
        const num2 = try std.fmt.parseInt(i32, numIter.next().?, 10);
        try list1.append(num1);
        try list2.append(num2);
    }
    std.mem.sort(i32, list1.items, {}, comptime std.sort.asc(i32));
    std.mem.sort(i32, list2.items, {}, comptime std.sort.asc(i32));
    // print("ArrayList: {any}\n", .{list1.items});
    var sum: i64 = 0;

    for (list1.items, list2.items) |left, right| {
        var distance = left - right;
        if (distance < 0) distance *= -1;
        sum += @as(i64, distance);
    }
    return sum;
}

pub fn part2(input: []const u8) !i64 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    var iter = std.mem.splitScalar(u8, input, '\n');
    var list1 = std.ArrayList(i32).init(alloc);
    var list2 = std.ArrayList(i32).init(alloc);
    defer list1.deinit();
    defer list2.deinit();

    while (iter.next()) |line| {
        var numIter = std.mem.splitScalar(u8, line, ' ');

        const num1 = try std.fmt.parseInt(i32, numIter.next().?, 10);
        // Skip spaces
        _ = numIter.next();
        _ = numIter.next();
        const num2 = try std.fmt.parseInt(i32, numIter.next().?, 10);
        try list1.append(num1);
        try list2.append(num2);
    }
    var sum: i64 = 0;

    for (list1.items) |n| {
        const score = n * occurences(list2.items, n);
        sum += @as(i64, score);
    }
    return sum;
}

pub fn main() !void {
    const input = std.mem.trimRight(u8, @embedFile("input.txt"), "\n");
    // const input =
    //     \\3   4
    //     \\4   3
    //     \\2   5
    //     \\1   3
    //     \\3   9
    //     \\3   3
    // ;

    print("part1: {any}\n", .{part1(input)});
    print("part2: {any}\n", .{part2(input)});
}
