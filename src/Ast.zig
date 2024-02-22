const std = @import("std");

const Ast = @This();
const lexemes = @import("lexemes.zig");

allocator: std.mem.Allocator,
expressions: std.ArrayList(Expression),

pub fn init(allocator: std.mem.Allocator) Ast {
    return Ast{
        .allocator = allocator,
        .expressions = std.ArrayList(Expression).init(std.testing.allocator),
    };
}

pub fn deinit(self: *Ast) void {
    self.expressions.deinit();
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
    try self.expressions.append(expression);
    return @enumFromInt(index);
}

pub fn appendManyExpression(self: *Ast, items: []const Expression) !ManyIndex {
    const index = self.expressions.items.len;
    try self.expressions.append(.{
        .many = items.len,
    });
    try self.expressions.appendSlice(items);
    return @enumFromInt(index);
}

pub fn getManyExpression(self: *Ast, index: ManyIndex) []const Expression {
    if (index == .none) return &.{};
    const start = @intFromEnum(index);
    const end = start + self.expressions.items[start].many;
    return self.expressions.items[start + 1 .. end];
}

pub fn iterateManyExpression(self: *Ast, index: ManyIndex) ManyIterator(false) {
    return ManyIterator(false){
        .ast = self,
        .index = index,
        .current = @intFromEnum(index) + 1,
        .end = @intFromEnum(index) + self.expressions.items[@intFromEnum(index)].many,
    };
}

pub fn mutateManyExpression(self: *Ast, index: ManyIndex) []Expression {
    if (index == .none) return &.{};
    const start = @intFromEnum(index);
    const end = start + self.expressions.items[start].many;
    return self.expressions.items[start + 1 .. end];
}

pub fn iterateManyExpressionMut(self: *Ast, index: ManyIndex) ManyIterator(true) {
    return ManyIterator(true){
        .ast = self,
        .index = index,
        .current = @intFromEnum(index) + 1,
        .end = @intFromEnum(index) + self.expressions.items[@intFromEnum(index)].many,
    };
}

pub fn ManyIterator(comptime mutable: bool) type {
    return struct {
        ast: *Ast,
        index: ManyIndex,
        current: usize,
        end: usize,

        pub fn next(self: *ManyIterator) ?Expression {
            if (self.current == self.end) return null;
            const result = self.ast.expressions.items[self.current];
            self.current += 1;
            return result;
        }

        pub fn nextMut(self: *ManyIterator) ?*Expression {
            if (comptime !mutable) @compileError("Iterator is not mutable");
            if (self.current == self.end) return null;
            const result = &self.ast.expressions.items[self.current];
            self.current += 1;
            return result;
        }
    };
}

pub const Index = enum(usize) { none = std.math.maxInt(usize), _ };
pub const ManyIndex = enum(usize) { none = std.math.maxInt(usize), _ };
pub const Expression = union(Tag) {
    const Tag = enum {
        identifier,
        typ,
        selector,
        unwrap,
        unary,
        compound_literal,
        block,
        call,
        literal,
        procedure_group,
        undefined,
        index,
        cast,
        ternary,
        binary,
        statement,
        context,

        many,
        ref,
        field,
        case_clause,
    };

    identifier: Identifier,
    typ: Typ,
    selector: Selector,
    unwrap: Unwrap,
    unary: Unary,
    compound_literal: CompoundLiteral,
    block: Block,
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
            endianess: Endianess,
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
            where_clauses: ?Index,

            parameters: ?ManyIndex,
        };

        pub const UnionType = struct {
            kind: DefinitionType,
            flags: UnionFlags,
            alignment: ?Index,
            variants: ManyIndex,
            where_clauses: ?Index,

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

    pub const Block = struct {
        flags: BlockFlags,
        statements: ManyIndex,
        // identifier
        label: ?Index,
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
        lhs: Index,
        rhs: Index,
    };

    pub const Statement = union(enum) {
        empty,
        declaration: Declaration,

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
    };

    pub const Field = struct {
        name: ?Index,
        typ: ?Index,
        value: ?Index,
        tag: ?[]const u8,
        flags: FieldFlags,
        attributes: ?ManyIndex,
    };

    pub const CaseClause = struct {
        expression: ?Index,
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

pub const Endianess = enum(u8) {
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
