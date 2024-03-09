const std = @import("std");

pub const Path = []const u8;

pub const Error = std.fs.File.OpenError || std.fs.File.ReadError || error{
    OutOfMemory,
    StreamTooLong,
};

pub const Kind = enum {
    file,
    directory,
};

pub const SourceCollector = struct {
    get_contents: *const fn (
        self: *const SourceCollector,
        allocator: std.mem.Allocator,
        path: Path,
    ) Error![]const u8,
    get_in_directory: *const fn (
        self: *const SourceCollector,
        allocator: std.mem.Allocator,
        path: Path,
    ) Error!std.ArrayList(Path),
    kind_exists: *const fn (self: *const SourceCollector, path: Path, kind: Kind) bool,
    ptr: *align(8) const anyopaque,

    pub inline fn getContents(self: *const SourceCollector, allocator: std.mem.Allocator, path: Path) Error![]const u8 {
        return self.get_contents(self, allocator, path);
    }

    // free the strings
    pub inline fn getInDirectory(self: *const SourceCollector, allocator: std.mem.Allocator, path: Path) Error!std.ArrayList(Path) {
        return self.get_in_directory(self, allocator, path);
    }

    pub inline fn kindExists(self: *const SourceCollector, path: Path, kind: Kind) bool {
        return self.kind_exists(self, path, kind);
    }
};

pub const StdFsSourceCollector = struct {
    root_directory: std.fs.Dir,

    pub fn init(root_directory: std.fs.Dir) StdFsSourceCollector {
        return StdFsSourceCollector{
            .root_directory = root_directory,
        };
    }

    pub fn deinit(self: *StdFsSourceCollector) void {
        self.root_directory.close();
    }

    pub fn collector(self: *const StdFsSourceCollector) SourceCollector {
        const col = SourceCollector{
            .get_contents = &getContentsImpl,
            .get_in_directory = &getInDirectoryImpl,
            .kind_exists = &kindExistsImpl,
            .ptr = @ptrCast(self),
        };

        return col;
    }

    fn getContentsImpl(self: *const SourceCollector, allocator: std.mem.Allocator, path: Path) Error![]const u8 {
        const this: *const StdFsSourceCollector = @ptrCast(self.ptr);
        return this.getContents(allocator, path);
    }

    pub fn getContents(self: *const StdFsSourceCollector, allocator: std.mem.Allocator, path: Path) Error![]const u8 {
        const file = try self.root_directory.openFile(path, .{});
        return try file.reader().readAllAlloc(allocator, std.math.maxInt(usize));
    }

    fn getInDirectoryImpl(self: *const SourceCollector, allocator: std.mem.Allocator, path: Path) Error!std.ArrayList(Path) {
        const this: *const StdFsSourceCollector = @ptrCast(self.ptr);
        return this.getInDirectory(allocator, path);
    }

    pub fn getInDirectory(self: *const StdFsSourceCollector, allocator: std.mem.Allocator, path: Path) Error!std.ArrayList(Path) {
        var dir = try self.root_directory.openDir(path, .{ .iterate = true });
        defer dir.close();

        var it_count = dir.iterate();
        const count = blk: {
            var got: usize = 0;
            while (try it_count.next()) |entry| {
                // we don't allow for directories in the source collector
                if (entry.kind == .file)
                    got += 1;
            }
            break :blk got;
        };

        var paths = try std.ArrayList(Path).initCapacity(allocator, count);
        var it_names = dir.iterate();
        var index: usize = 0;
        while (try it_names.next()) |entry| {
            if (entry.kind == .file) {
                paths.appendAssumeCapacity(try allocator.dupe(u8, entry.name));
                index += 1;
            }
        }

        return paths;
    }

    fn kindExistsImpl(self: *const SourceCollector, path: Path, kind: Kind) bool {
        const this: *const StdFsSourceCollector = @ptrCast(self.ptr);
        return this.kindExists(path, kind);
    }

    pub fn kindExists(self: *const StdFsSourceCollector, path: Path, kind: Kind) bool {
        switch (kind) {
            .file => {
                const file = self.root_directory.openFile(path, .{}) catch return false;
                defer file.close();
                return true;
            },
            .directory => {
                var dir = self.root_directory.openDir(path, .{}) catch return false;
                defer dir.close();
                return true;
            },
        }
    }
};
