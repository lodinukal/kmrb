const std = @import("std");
const testing = std.testing;

const Parser = @import("Parser.zig");

export fn add(a: i32, b: i32) i32 {
    return a + b;
}

test "basic" {
    const t =
        \\package oxy;
        \\
        \\urg: u32 = 0
        \\
        \\@ecs.Component
        \\Health :: struct {
        \\}
        \\
        \\main :: proc() {
        \\  system := ecs.new_system(Health);
        \\  defer ecs.destroy(&system);
        \\  for x in 0..10 {
        \\      print("hello world\n")
        \\  }
        \\  #assert(1 == 1)
        \\}
    ;

    const start = std.time.nanoTimestamp();

    var parser = Parser.init(testing.allocator);
    defer parser.deinit();

    var result = try parser.parse(.{ .code = t, .directory = "test", .name = "waa" });
    defer result.deinit();

    const end = std.time.nanoTimestamp();

    const took = end - start;
    std.debug.print("Took: {}\n", .{took});
}
