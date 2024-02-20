const std = @import("std");

const Lexer = @import("Lexer.zig");

const Index = enum(usize) { none = std.math.maxInt(usize), _ };
const ManyIndex = enum(usize) { none = std.math.maxInt(usize), _ };
const Expression = union(Tag) {
    const Tag = enum {
        declaration,
        identifier,
        literal,
        binary,
        unary,

        many,
    };

    declaration: Declaration,
    identifier: []const u8,
    literal: []const u8,
    binary: void,
    unary: void,

    many: usize, // comes right after the current expression
};
fn expressionInit(comptime tag: Expression.Tag) fn (e: anytype) Expression {
    return struct {
        pub fn init(e: anytype) Expression {
            return @unionInit(Expression, @tagName(tag), e);
        }
    }.init;
}

const Declaration = struct {
    pub const Type = enum {
        variable,
        constant,
    };

    identifier: []const u8,
    type_identifier: ?Index,
    type: Type,
    value: Index,
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    expressions: std.ArrayList(Expression),
    lexer: Lexer.Lexer,

    pub fn init(allocator: std.mem.Allocator, source: Lexer.Source) Parser {
        return Parser{
            .allocator = allocator,
            .expressions = std.ArrayList(Expression).init(std.testing.allocator),
            .lexer = Lexer.init(source),
        };
    }

    pub fn deinit(self: *Parser) void {
        self.expressions.deinit();
    }

    pub fn getExpression(self: *Parser, index: Index) Expression {
        if (index == .none) @panic("Invalid index");
        return self.expressions.items[@intFromEnum(index)];
    }

    // WILL be invalidated by resizing the array
    pub fn mutateExpression(self: *Parser, index: Index) *Expression {
        if (index == .none) @panic("Invalid index");
        return &self.expressions.items[@intFromEnum(index)];
    }

    pub fn appendExpression(self: *Parser, expression: Expression) !Index {
        const index = self.expressions.items.len;
        try self.expressions.append(expression);
        return @enumFromInt(index);
    }

    pub fn appendManyExpression(self: *Parser, items: []const Expression) !ManyIndex {
        const index = self.expressions.items.len;
        try self.expressions.append(.{
            .many = items.len,
        });
        try self.expressions.appendSlice(items);
        return @enumFromInt(index);
    }

    pub fn getManyExpression(self: *Parser, index: ManyIndex) []const Expression {
        if (index == .none) @panic("Invalid index");
        const start = @intFromEnum(index);
        const end = start + self.expressions.items[start].many;
        return self.expressions.items[start + 1 .. end];
    }

    pub fn mutateManyExpression(self: *Parser, index: ManyIndex) []Expression {
        if (index == .none) @panic("Invalid index");
        const start = @intFromEnum(index);
        const end = start + self.expressions.items[start].many;
        return self.expressions.items[start + 1 .. end];
    }

    fn expectKeyword(self: *Parser, keyword: Lexer.Keyword) !Lexer.Token {
        const token = try self.expectToken();
        if (token.kw != keyword) return error.UnexpectedKeyword;
        return token;
    }

    fn expectToken(self: *Parser, expected: Lexer.Token.Kind) !Lexer.Token {
        const token = self.nextToken() orelse @panic("unexpected EOF");
        if (token.kind != expected) return error.UnexpectedToken;
        return token;
    }

    fn nextToken(self: *Parser) ?Lexer.Token {
        return self.lexer.next();
    }
};

test {
    const input =
        \\xamasee : shimmy : HEHEHEHA
        \\zuul := 42
    ;
    _ = input;
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    // const result = try sequence.parse(arena.allocator(), input);
    // for (result.value) |r| {
    //     std.debug.print("{}\n", .{r});
    // }
}
