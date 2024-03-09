const std = @import("std");

const Validation = @This();
const Ast = @import("Ast.zig");
const lexemes = @import("lexemes.zig");
const unit = @import("unit.zig");

allocator: std.mem.Allocator,
scopes: std.ArrayListUnmanaged(Scope) = .{},

pub fn init(allocator: std.mem.Allocator) Validation {
    return Validation{
        .allocator = allocator,
        .scopes = std.ArrayListUnmanaged(Scope){},
    };
}

pub fn deinit(self: *Validation) void {
    for (self.scopes.items) |*scope| {
        scope.deinit(self.allocator);
    }
}

pub fn getScope(self: *const Validation, index: ScopeIndex) *Scope {
    return &self.scopes.items[@intFromEnum(index)];
}

pub fn newScope(self: *Validation, tag: Scope.Tag) !ScopeIndex {
    const index: ScopeIndex = @enumFromInt(self.scopes.items.len);
    try self.scopes.append(self.allocator, Scope{
        .passes = self,
        .tag = tag,
        .index = index,
    });
    return index;
}

pub const SourcedIndex = struct {
    ast: *Ast = undefined,
    index: Ast.Index = .none,
};

pub const ScopeIndex = enum(usize) { none = std.math.maxInt(usize), _ };

pub const Scope = struct {
    tag: Tag,
    validation: *Validation,
    index: ScopeIndex,

    /// symbols that are in this scope
    symbols: std.StringArrayHashMapUnmanaged(ScopeSymbol) = .{},
    /// imports can be placed anywhere and are scoped, so we need to keep track of them
    used_imports: std.ArrayListUnmanaged(Using) = .{},
    /// scopes don't change position, so after each pass we can reset this scope_index
    /// and continue from there
    scope_water_level: usize = 0,
    children: std.ArrayListUnmanaged(ScopeIndex) = .{},

    pub const ScopeSymbol = union(enum) {
        package: ScopeIndex,
        symbol: SourcedIndex,
    };

    pub const Using = struct {
        ast: *Ast,
        scope_index: ScopeIndex,
    };

    pub const Tag = union(enum) {
        /// can see all symbols no matter the order in the same package
        package: PackageScope,
        /// can see symbols in the parent scope and only symbols that are declared before
        /// the current statement
        proc: void,
    };

    pub const PackageScope = struct {
        package: *const unit.Package,
    };

    pub fn putUsing(self: *Scope, allocator: std.mem.Allocator, using: Using) !void {
        try self.used_imports.append(allocator, using);
    }

    pub fn putSymbol(
        self: *Scope,
        allocator: std.mem.Allocator,
        name: []const u8,
        symbol: ScopeSymbol,
    ) !void {
        try self.symbols.put(allocator, name, symbol);
    }

    pub fn findSymbol(self: Scope, name: []const u8, from: *Ast) ?*ScopeSymbol {
        if (self.symbols.getPtr(name)) |symbol| {
            return symbol;
        }
        for (self.used_imports.items) |used_import_scope_index| {
            const scope = self.validation.getScope(used_import_scope_index.scope_index);
            // `using`s don't propagate across files (a file has its own ast)
            if (used_import_scope_index.ast != from) {
                continue;
            }
            if (scope.findSymbol(name, from)) |symbol| {
                return symbol;
            }
        }
        return null;
    }

    pub fn nextChild(self: *Scope, allocator: std.mem.Allocator, tag: Tag) !ScopeIndex {
        // this might get invalidated if we append to the array
        // so we have to store the id before we append
        const self_index = self.index;
        const validation = self.validation;
        var moving_self = self;
        if (self.children.items.len < self.scope_water_level + 1) {
            const new = try self.validation.newScope(tag);
            // we're invalidated from here on
            moving_self = validation.getScope(self_index);
            try moving_self.children.append(allocator, new);
        }
        moving_self.scope_water_level += 1;
        return moving_self.children.items[moving_self.scope_water_level - 1];
    }

    /// returns the index of the child at the given index
    pub fn getChild(self: *Scope, index: ScopeIndex) ?usize {
        return std.mem.indexOfScalar(ScopeIndex, self.children.items, index);
    }

    pub fn reset(self: *Scope) void {
        self.scope_water_level = 0;
        for (self.children.items) |child| {
            self.validation.getScope(child).reset();
        }
    }

    pub fn deinit(self: *Scope, allocator: std.mem.Allocator) void {
        self.symbols.deinit(allocator);
        self.used_imports.deinit(allocator);
        self.children.deinit(allocator);
        switch (self.tag) {
            .package => {},
            .proc => {},
        }
    }
};
