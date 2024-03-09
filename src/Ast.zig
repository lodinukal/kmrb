const std = @import("std");

const Ast = @This();
const Lexer = @import("Lexer.zig");
const lexemes = @import("lexemes.zig");

allocator: std.mem.Allocator,
statements: std.ArrayListUnmanaged(Index),
expressions: std.ArrayListUnmanaged(Expression),
imports: std.ArrayListUnmanaged(Index),
tokens: std.ArrayListUnmanaged(Lexer.Token),

pub fn init(allocator: std.mem.Allocator) Ast {
    return Ast{
        .allocator = allocator,
        .statements = std.ArrayListUnmanaged(Index){},
        .expressions = std.ArrayListUnmanaged(Expression){},
        .imports = std.ArrayListUnmanaged(Index){},
        .tokens = std.ArrayListUnmanaged(Lexer.Token){},
    };
}

pub fn deinit(self: *Ast) void {
    self.statements.deinit(self.allocator);
    self.expressions.deinit(self.allocator);
    self.imports.deinit(self.allocator);
    self.tokens.deinit(self.allocator);
}

pub fn recordToken(self: *Ast, token: Lexer.Token) !void {
    try self.tokens.append(self.allocator, token);
}

pub fn addImport(self: *Ast, index: Index) !void {
    try self.imports.append(self.allocator, index);
}

pub fn getExpression(self: *Ast, index: Index) Expression {
    if (index == .none) @panic("Invalid index");
    return self.expressions.items[@intFromEnum(index)];
}

// WILL be invalidated by resizing the array
pub fn mutateExpression(self: *Ast, index: Index) *Expression {
    if (index == .none) @panic("Invalid index");
    return &self.expressions.items[@intFromEnum(index)];
}

pub fn appendExpression(self: *Ast, expression: Expression) !Index {
    const index = self.expressions.items.len;
    try self.expressions.append(self.allocator, expression);
    return @enumFromInt(index);
}

pub fn appendManyExpression(self: *Ast, items: []const Expression) !ManyIndex {
    const index = self.expressions.items.len;
    try self.expressions.append(self.allocator, .{
        .many = items.len,
    });
    try self.expressions.appendSlice(self.allocator, items);
    return @enumFromInt(index);
}

pub fn getManyExpression(self: *Ast, index: ManyIndex) []const Expression {
    if (index == .none) return &.{};
    const start = @intFromEnum(index);
    const count = self.expressions.items[start].many;
    return self.expressions.items[start + 1 .. start + count + 1];
}

pub fn iterateManyExpression(self: *Ast, index: ManyIndex) ManyIterator(false) {
    return ManyIterator(false){
        .ast = self,
        .index = index,
        .current = @intFromEnum(index) + 1,
        .end = @intFromEnum(index) + self.expressions.items[@intFromEnum(index)].many + 1,
    };
}

pub fn mutateManyExpression(self: *Ast, index: ManyIndex) []Expression {
    if (index == .none) return &.{};
    const start = @intFromEnum(index);
    const count = self.expressions.items[start].many;
    return self.expressions.items[start + 1 .. start + count + 1];
}

pub fn iterateManyExpressionMut(self: *Ast, index: ManyIndex) ManyIterator(true) {
    return ManyIterator(true){
        .ast = self,
        .index = index,
        .current = @intFromEnum(index) + 1,
        .end = @intFromEnum(index) + self.expressions.items[@intFromEnum(index)].many + 1,
    };
}

pub fn ManyIterator(comptime mutable: bool) type {
    return struct {
        const Self = @This();
        ast: *Ast,
        index: ManyIndex,
        current: usize,
        end: usize,

        const Give = if (mutable) *Expression else Expression;

        pub const Result = struct {
            expression: Give,
            index: Index,
        };

        pub fn next(self: *Self) ?Result {
            if (self.current == self.end) return null;
            const result = self.ast.expressions.items[self.current];
            defer self.current += 1;
            return Result{
                .expression = result,
                .index = @enumFromInt(self.current),
            };
        }

        pub fn nextMut(self: *Self) ?Result {
            if (comptime !mutable) @compileError("Iterator is not mutable");
            if (self.current == self.end) return null;
            const result = &self.ast.expressions.items[self.current];
            defer self.current += 1;
            return Result{
                .expression = result,
                .index = @enumFromInt(self.current),
            };
        }

        pub fn size(self: *Self) usize {
            return self.end - self.current;
        }
    };
}

pub const Index = enum(usize) { none = std.math.maxInt(usize), _ };
pub const ManyIndex = enum(usize) { none = std.math.maxInt(usize), _ };
pub const Expression = union(enum) {
    const Tag = std.meta.Tag(Expression);

    identifier: Identifier,
    typ: Typ,
    selector: Selector,
    unwrap: Unwrap,
    unary: Unary,
    compound_literal: CompoundLiteral,
    procedure: Procedure,
    call: Call,
    literal: Literal,
    procedure_group: ManyIndex,
    undefined: void,
    index: IndexSelector,
    cast: Cast,
    ternary: Ternary,
    binary: Binary,
    statement: Statement,
    compile_time: Index,
    context: void,

    many: usize, // comes right after the current expression
    ref: Index, // points to another Index
    field: Field,
    case_clause: CaseClause,

    pub const Identifier = struct {
        contents: []const u8,
        polymorphic: bool,
    };

    pub const Typ = union(enum) {
        builtin: BuiltinType,
        identifier: Index,
        procedure: ProcedureType,
        pointer: PointerType,
        multipointer: MultiPointerType,
        slice: SliceType,
        array: ArrayType,
        dynamic_array: DynamicArrayType,
        bitset: BitsetType,
        typeid: TypeidType,
        map: MapType,
        matrix: MatrixType,
        distinct: DistinctType,
        enumeration: EnumType,
        expression: ExpressionType,
        structure: StructType,
        @"union": UnionType,
        poly: PolyType,

        pub const BuiltinType = struct {
            identifier: []const u8,
            kind: BuiltinTypeKind,
            size: u16,
            alignof: u16,
            endianness: Endianness,
        };

        pub const ProcedureType = struct {
            kind: DefinitionType,
            flags: ProcedureFlags,
            convention: CallingConvention,
            params: ManyIndex,
            results: ManyIndex,
        };

        pub const PointerType = struct {
            typ: Index,
            is_const: bool,
        };

        pub const MultiPointerType = struct {
            typ: Index,
            is_const: bool,
        };

        pub const SliceType = struct {
            typ: Index,
            is_const: bool,
        };

        pub const ArrayType = struct {
            typ: Index,
            count: ?Index,
        };

        pub const DynamicArrayType = struct {
            typ: Index,
        };

        pub const BitsetType = struct {
            typ: ?Index,
            expression: Index,
        };

        pub const TypeidType = struct {
            specialisation: ?Index,
        };

        pub const MapType = struct {
            key: Index,
            value: Index,
        };

        pub const MatrixType = struct {
            typ: Index,
            rows: Index,
            columns: Index,
        };

        pub const DistinctType = struct {
            typ: Index,
        };

        pub const EnumType = struct {
            typ: ?Index,
            fields: ManyIndex,
        };

        /// used for referring to a type by anonymous declaration
        pub const ExpressionType = struct {
            expression: Index,
        };

        pub const StructType = struct {
            kind: DefinitionType,
            flags: StructFlags,
            alignment: ?Index,
            fields: ManyIndex,
            where_clauses: ?ManyIndex,

            parameters: ?ManyIndex,
        };

        pub const UnionType = struct {
            kind: DefinitionType,
            flags: UnionFlags,
            alignment: ?Index,
            variants: ManyIndex,
            where_clauses: ?ManyIndex,

            parameters: ?ManyIndex,
        };

        pub const PolyType = struct {
            typ: Index,
            specialisation: ?Index,
        };
    };

    pub const Selector = struct {
        operand: ?Index,
        field: Index,
    };

    pub const Unwrap = struct {
        operand: Index,
        access: ?Index,
    };

    pub const Unary = struct {
        operator: lexemes.OperatorKind,
        operand: Index,
    };

    pub const CompoundLiteral = struct {
        typ: ?Index,
        fields: ManyIndex,
    };

    pub const Procedure = struct {
        typ: Index,
        where_clauses: ?ManyIndex,
        body: ?Index,
    };

    pub const Call = struct {
        operand: Index,
        arguments: ManyIndex,
    };

    pub const Literal = struct {
        kind: lexemes.LiteralKind,
        value: []const u8,
    };

    pub const IndexSelector = struct {
        operand: Index,
        lhs: ?Index,
        rhs: ?Index,
    };

    pub const Cast = struct {
        operation: lexemes.KeywordKind,
        typ: Index,
        expression: Index,
    };

    pub const Ternary = struct {
        operation: lexemes.KeywordKind,
        on_true: Index,
        condition: Index,
        on_false: Index,

        pub inline fn isRuntime(self: Ternary) bool {
            return self.operation == .@"if";
        }
    };

    pub const Binary = struct {
        operation: lexemes.OperatorKind,
        /// ManyIndex to support .in
        lhs: ManyIndex,
        rhs: Index,
    };

    pub const Statement = union(enum) {
        empty,
        block: Block,
        declaration: Declaration,
        assignment: Assignment,
        expression: Index,
        import: Import,
        ifs: If,
        when: When,
        fors: For,
        switchs: Switch,
        defers: Defer,
        returns: Return,
        foreign_block: ForeignBlock,
        foreign_import: ForeignImport,
        branch: Branch,
        package: Package,

        pub const Block = struct {
            flags: BlockFlags,
            statements: ManyIndex,
            // identifier
            label: ?Index,
        };

        pub const Declaration = struct {
            pub const Kind = enum {
                variable,
                constant,
            };

            identifiers: ManyIndex,
            typ: ?Index,
            values: ManyIndex,
            kind: Kind,
            attributes: ?ManyIndex,
        };

        pub const Assignment = struct {
            assignment: lexemes.AssignmentKind,
            lhs: ManyIndex,
            rhs: ManyIndex,
        };

        pub const Import = struct {
            name: []const u8,
            collection: ?[]const u8,
            pathname: []const u8,
            using: bool,
            location: Lexer.Location,
        };

        pub const If = struct {
            /// stmt
            init: ?Index = null,
            /// expression
            condition: Index,
            /// stmt
            body: Index,
            /// stmt
            elif: ?Index = null,
            /// identifier
            label: ?Index,
        };

        pub const When = struct {
            /// expression
            condition: Index,
            /// stmt
            body: Index,
            /// stmt
            elif: ?Index = null,
        };

        pub const For = struct {
            /// stmt
            init: ?Index,
            /// expression
            condition: ?Index,
            /// stmt
            body: Index,
            /// stmt
            post: ?Index = null,
            /// identifier
            label: ?Index,
        };

        pub const Switch = struct {
            /// stmt
            init: ?Index,
            /// expression
            condition: ?Index,
            /// many caseclause
            clauses: ManyIndex, // std.ArrayList(CaseClauseIndex),
            /// identifier
            label: ?Index,
        };

        pub const Defer = struct {
            statement: Index,
        };

        pub const Return = struct {
            expression: ManyIndex,
        };

        pub const ForeignBlock = struct {
            /// identifier
            name: ?Index = null,
            /// statement
            body: Index,
            /// many field
            attributes: ManyIndex, // std.ArrayList(FieldIndex),
        };

        pub const ForeignImport = struct {
            name: []const u8,
            sources: ManyIndex, // std.ArrayList(IdentifierIndex),
            /// many field
            attributes: ManyIndex, // std.ArrayList(FieldIndex),
        };

        pub const Branch = struct {
            branch: lexemes.KeywordKind,
            /// identifier
            label: ?Index,
        };

        pub const Package = struct {
            name: []const u8,
            location: Lexer.Location,
        };
    };

    pub const Field = struct {
        name: ?Index = null,
        typ: ?Index = null,
        value: ?Index = null,
        tag: ?[]const u8 = null,
        flags: FieldFlags = .{},
        attributes: ?ManyIndex = null,
    };

    pub const CaseClause = struct {
        expression: ?ManyIndex,
        statements: ManyIndex,
    };
};
pub fn expressionInit(comptime tag: Expression.Tag) fn (e: anytype) Expression {
    return struct {
        pub fn init(e: anytype) Expression {
            return @unionInit(Expression, @tagName(tag), e);
        }
    }.init;
}

pub const BuiltinTypeKind = enum(u8) {
    sint = 0, // i8,i{16,32,64,128}(le|be)
    uint = 1, // u8,u{16,32,64,128}(le|be)
    float = 2, // f{16,32,64}(le|be)
    bool = 3, // b{8,16,32,64}
    string = 4, // string
    cstring = 5, // cstring
    pointer = 6, // rawptr
    uintptr = 7, // uintptr
    _,
};

pub const DefinitionType = enum(u8) {
    concrete,
    generic,
};

pub const Endianness = enum(u8) {
    na,
    little,
    big,
};

pub const BlockFlags = packed struct {
    bounds_check: bool = false,
    type_assert: bool = false,
};

pub const ProcedureFlags = packed struct {
    diverging: bool = false,
    optional_ok: bool = false,
    optional_allocation_error: bool = false,
    bounds_check: bool = false,
    type_assert: bool = false,
    force_inline: bool = false,
};

pub const StructFlags = packed struct {
    @"packed": bool = false,
    uncopyable: bool = false,
    @"union": bool = false,
};

pub const UnionFlags = packed struct {
    no_nil: bool = false,
    shared_nil: bool = false,
    maybe: bool = false,
};

pub const FieldFlags = packed struct {
    using: bool = false,
    any_int: bool = false,
    c_vararg: bool = false,
    no_alias: bool = false,
    subtype: bool = false,
    @"const": bool = false,
};

pub const CallingConvention = enum(u8) {
    invalid,
    rl,
    contextless,
    cdecl,
    stdcall,
    fastcall,
    none,
    naked,
    win64,
    sysv,
    system,
};
