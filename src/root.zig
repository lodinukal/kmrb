const std = @import("std");
const testing = std.testing;

const unit = @import("unit.zig");
const StdFsSourceCollector = @import("source_collector.zig").StdFsSourceCollector;

export fn add(a: i32, b: i32) i32 {
    return a + b;
}

test "basic" {
    const start = std.time.nanoTimestamp();

    var fsc = StdFsSourceCollector.init(std.fs.cwd());
    defer fsc.deinit();

    var build = try unit.Build.init(testing.allocator, fsc.collector());
    defer build.deinit();

    try build.addPackage("test");

    const end = std.time.nanoTimestamp();

    const took = end - start;
    std.debug.print("Took: {}\n", .{took});
}
