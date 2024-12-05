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

/*

const std = @import("std");

pub fn main(allocator: std.mem.Allocator, input_file: []const u8) !void {
    var in = try std.fs.cwd().openFile(input_file, .{ .mode = .read_only });
    defer in.close();

    const file_contents = try in.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(file_contents);

    var lines = std.mem.tokenizeSequence(u8, file_contents, "\n");
    var left_list = std.ArrayList(u64).init(allocator);
    defer left_list.deinit();
    var right_list = std.ArrayList(u64).init(allocator);
    defer right_list.deinit();

    while (lines.next()) |line| {
        var tokens = std.mem.tokenizeScalar(u8, line, ' ');
        const left_token = tokens.next().?;
        const right_token = tokens.next().?;

        const left_int = try std.fmt.parseInt(u64, left_token, 10);
        const right_int = try std.fmt.parseInt(u64, right_token, 10);

        try left_list.append(left_int);
        try right_list.append(right_int);
    }

    std.mem.sort(u64, left_list.items, {}, std.sort.asc(u64));
    std.mem.sort(u64, right_list.items, {}, std.sort.asc(u64));

    var sum: u64 = 0;
    for (left_list.items, right_list.items) |left, right| {
        const diff = if (left > right) left - right else right - left;
        sum += diff;
    }

    std.debug.print("{d}\n", .{sum});
}


const std = @import("std");

pub fn main(allocator: std.mem.Allocator, input_file: []const u8) !void {
    var in = try std.fs.cwd().openFile(input_file, .{ .mode = .read_only });
    defer in.close();

    const file_contents = try in.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(file_contents);

    var lines = std.mem.tokenizeSequence(u8, file_contents, "\n");
    var left_list = std.ArrayList(u64).init(allocator);
    defer left_list.deinit();
    var right_counts = std.AutoHashMap(u64, u64).init(allocator);
    defer right_counts.deinit();

    while (lines.next()) |line| {
        var tokens = std.mem.tokenizeScalar(u8, line, ' ');
        const left_token = tokens.next().?;
        const right_token = tokens.next().?;

        const left_int = try std.fmt.parseInt(u64, left_token, 10);
        const right_int = try std.fmt.parseInt(u64, right_token, 10);

        try left_list.append(left_int);
        if (right_counts.get(right_int)) |count| {
            try right_counts.put(right_int, count + 1);
        } else {
            try right_counts.put(right_int, 1);
        }
    }

    var sum: u64 = 0;
    for (left_list.items) |left| {
        const count = right_counts.get(left) orelse 0;
        sum += left * count;
    }

    std.debug.print("{d}\n", .{sum});
}

*/