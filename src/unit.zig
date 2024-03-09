const std = @import("std");

const Ast = @import("Ast.zig");
const Parser = @import("Parser.zig");

const SourceCollector = @import("source_collector.zig").SourceCollector;
const SourceCollectorError = @import("source_collector.zig").Error;

pub const Collection = struct {
    name: []const u8,
    path: []const u8,
};

pub const Unit = struct {
    allocator: std.mem.Allocator,

    pathname: []const u8 = "",
    packages: std.ArrayList(Package) = undefined,

    pub fn init(
        allocator: std.mem.Allocator,
        pathname: []const u8,
    ) Unit {
        return .{
            .allocator = allocator,
            .pathname = pathname,
            .packages = std.ArrayList(Package).init(allocator),
        };
    }

    pub fn deinit(self: *Unit) void {
        for (self.packages.items) |*package| {
            package.deinit();
        }
        self.packages.deinit();
    }

    pub fn addPackage(
        self: *Unit,
        pathname: []const u8,
    ) !*Package {
        var found = true;
        _ = self.findPackage(pathname) catch {
            found = false;
        };
        if (found) {
            return error.AlreadyExists;
        }

        var package = try self.packages.addOne();
        try package.init(
            self,
            try self.allocator.dupe(u8, pathname),
        );
        return package;
    }

    pub fn findPackage(
        self: *Unit,
        pathname: []const u8,
    ) !*Package {
        for (self.packages.items) |*package| {
            if (std.mem.eql(u8, package.pathname, pathname)) {
                return package;
            }
        }

        return error.FileNotFound;
    }
};

pub const Package = struct {
    parent_unit: *Unit = undefined,

    pathname: []const u8 = "",
    asts: std.ArrayList(Ast) = undefined,

    pub fn init(
        self: *Package,
        parent_unit: *Unit,
        pathname: []const u8,
    ) !void {
        self.parent_unit = parent_unit;
        self.pathname = pathname;
        self.asts = std.ArrayList(Ast).init(parent_unit.allocator);
    }

    pub fn deinit(self: *Package) void {
        for (self.asts.items) |*t| {
            t.deinit();
        }
        self.asts.deinit();
        self.parent_unit.allocator.free(self.pathname);
    }

    pub fn addAst(self: *Package) !*Ast {
        const ast = try self.asts.addOne();
        ast.* = Ast.init(
            self.parent_unit.allocator,
        );
        return ast;
    }
};

pub const Build = struct {
    allocator: std.mem.Allocator,
    collector: SourceCollector,

    unit: Unit,
    collections: std.ArrayList(Collection),

    work_list: std.ArrayList(Work),

    pub const Work = struct {
        builder: *Build,
        package: *const Package,
        pathname: []const u8,
        ast: *Ast,
        err: bool,
    };

    pub fn init(
        allocator: std.mem.Allocator,
        collector: SourceCollector,
    ) !Build {
        return .{
            .allocator = allocator,
            .collector = collector,
            .unit = Unit.init(allocator, ""),
            .collections = std.ArrayList(Collection).init(allocator),
            .work_list = std.ArrayList(Work).init(allocator),
        };
    }

    pub fn deinit(self: *Build) void {
        self.work_list.deinit();
        self.collections.deinit();
        self.unit.deinit();
    }

    pub fn err(self: *const Build, comptime fmt: []const u8, args: anytype) void {
        _ = self;
        std.debug.print("ERROR: ", .{});
        std.debug.print(fmt, args);
        std.debug.print("\n", .{});
    }

    pub fn addCollection(self: *Build, name: []const u8, path: []const u8) !void {
        if (!self.collector.kindExists(path, .directory)) {
            self.err("Collection path `{s}` does not exist", .{path});
            return error.FileNotFound;
        }

        for (self.collections.items) |c| {
            if (std.mem.eql(u8, c.name, name)) {
                return error.AlreadyExists;
            }
        }

        try self.collections.append(Collection{
            .name = name,
            .path = path,
        });
    }

    pub fn addPackage(self: *Build, path: []const u8) !void {
        const package = try self.unit.addPackage(path);

        var sfa = std.heap.stackFallback(1024, self.allocator);
        const temp = sfa.get();

        const files = try self.collector.getInDirectory(
            temp,
            path,
        );
        defer {
            for (files.items) |name| {
                temp.free(name);
            }
            files.deinit();
        }

        for (files.items) |name| {
            if (!std.mem.endsWith(u8, name, ".rl")) {
                continue;
            }
            // try self.pool.spawn(worker, .{try self.addWork(package, try self.allocator.dupe(u8, name))});
            try tryingWorker(try self.addWork(package, try self.allocator.dupe(u8, name)));
        }
    }

    pub fn findCollection(self: *Build, collection: []const u8) ![]const u8 {
        for (self.collections.items) |c| {
            if (std.mem.eql(u8, c.name, collection)) {
                return c.path;
            }
        }
        self.err("Cannot find collection `{s}`", .{collection});
        return error.FileNotFound;
    }

    pub fn findPackage(self: *Build, pathname: []const u8) !*Package {
        return try self.unit.findPackage(pathname);
    }

    fn addWork(self: *Build, package: *Package, name: []const u8) !*Work {
        const work = try self.work_list.addOne();
        work.builder = self;
        work.err = false;
        work.ast = try package.addAst();
        work.package = package;
        work.pathname = name;
        return work;
    }

    pub const WorkerError = Parser.Error || SourceCollectorError || error{
        InvalidPackageName,
        AlreadyExists,
    };
    fn tryingWorker(work: *Work) WorkerError!void {
        defer work.builder.allocator.free(work.pathname);
        var sfa = std.heap.stackFallback(1024, work.builder.allocator);
        const temp = sfa.get();

        var buf = std.mem.zeroes([512]u8);
        var fba = std.heap.FixedBufferAllocator.init(&buf);
        const proper_source = std.fs.path.join(fba.allocator(), &.{
            work.package.pathname,
            work.pathname,
        }) catch unreachable;

        const source = try work.builder.collector.getContents(
            temp,
            proper_source,
        );
        defer temp.free(source);

        var parser = Parser.init(work.ast);
        defer parser.deinit();
        try parser.parse(.{
            .code = source,
            .pathname = work.pathname,
        });

        if (work.ast.statements.items.len == 0) {
            return;
        }
        const first_stmt = work.ast.getExpression(work.ast.statements.items[0]).statement;
        const package_name: ?[]const u8 = if (first_stmt == .package) first_stmt.package.name else null;
        if (package_name) |name| {
            if (std.mem.eql(u8, name, "_")) {
                work.builder.err("package name cannot be `_`", .{});
                return error.InvalidPackageName;
            } else if (std.mem.eql(u8, name, "intrinsics") or
                std.mem.eql(u8, name, "builtin"))
            {
                work.builder.err(
                    "use of reserved package name `{s}`",
                    .{name},
                );
                return error.InvalidPackageName;
            }
        }

        for (work.ast.imports.items) |import| {
            const import_statement = work.ast.getExpression(import).statement.import;
            if (std.mem.eql(u8, import_statement.collection orelse "", "core")) {
                if (std.mem.eql(u8, import_statement.pathname, "intrinsics") or
                    std.mem.eql(u8, import_statement.pathname, "builtin"))
                {
                    continue;
                }
            }

            buf = std.mem.zeroes([512]u8);
            fba = std.heap.FixedBufferAllocator.init(&buf);
            const resolved_source = try work.builder.resolveFullPathname(
                fba.allocator(),
                work.package.pathname,
                import_statement.collection,
                import_statement.pathname,
            );
            try work.builder.addPackage(resolved_source);
        }
    }

    pub fn resolveFullPathname(
        self: *Build,
        allocator: std.mem.Allocator,
        base: []const u8,
        collection: ?[]const u8,
        pathname: []const u8,
    ) ![]const u8 {
        const look_path = if (collection) |collection_name|
            self.findCollection(collection_name) catch {
                self.err("Cannot find collection `{s}`", .{collection_name});
                return error.FileNotFound;
            }
        else
            base;

        const resolved_source = try std.mem.join(allocator, "/", &.{
            look_path,
            pathname,
        });

        return resolved_source;
    }
};
