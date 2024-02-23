const std = @import("std");

const Parser = @This();
const Lexer = @import("Lexer.zig");
const Ast = @import("Ast.zig");
const lexemes = @import("lexemes.zig");

allocator: std.mem.Allocator,
ast: Ast,
lexer: Lexer,
peek_list: [10]Lexer.Token = .{Lexer.Token.invalid} ** 10,

this_token: Lexer.Token = undefined,
last_token: Lexer.Token = undefined,

this_procedure: ?Ast.Index = null,

trace_depth: i32 = 0,
expression_depth: i32 = 0,

allow_newline: bool = false,
allow_type: bool = false,
allow_in: bool = false,

pub const Error = error{
    Parse,
};

pub fn init(allocator: std.mem.Allocator, source: Lexer.Source) Parser {
    return Parser{
        .allocator = allocator,
        .ast = Ast.init(allocator),
        .lexer = Lexer.init(source),
    };
}

pub fn deinit(self: *Parser) void {
    self.ast.deinit();
}

fn err(self: *const Parser, comptime msg: []const u8, args: anytype) void {
    std.log.err("{}:{}:" ++ msg, .{
        self.this_token.location.line,
        self.this_token.location.column,
    } ++ args);
}

fn acceptedOperator(self: *Parser, op: lexemes.OperatorKind) bool {
    if (isOperator(self.this_token, op)) {
        _ = try self.advance();
        return true;
    }
    return false;
}

fn acceptedAssignment(self: *Parser, op: lexemes.AssignmentKind) bool {
    if (isAssignment(self.this_token, op)) {
        _ = try self.advance();
        return true;
    }
    return false;
}

fn acceptedKind(self: *Parser, kind: lexemes.TokenKind) bool {
    if (isKind(self.this_token, kind)) {
        _ = try self.advance();
        return true;
    }
    return false;
}

fn acceptedKeyword(self: *Parser, kind: lexemes.KeywordKind) bool {
    if (isKeyword(self.this_token, kind)) {
        _ = try self.advance();
        return true;
    }
    return false;
}

fn peek(self: *Parser) !Lexer.Token {
    var token = try self.lexer.peek();
    while (isKind(token, .comment)) {
        token = try self.lexer.peek();
    }
    return token;
}

fn acceptedSeparator(self: *Parser) bool {
    const token = self.this_token;
    if (self.acceptedOperator(.comma)) return true;
    if (isKind(token, .semicolon)) {
        if (isNewline(token)) {
            const pk = self.peek() catch return false;
            if (isKind(pk, .rbrace) or isOperator(pk, .rparen)) {
                _ = try self.advance();
                return true;
            }
        }
        self.err("Expected comma", .{});
        return false;
    }
    return false;
}

fn acceptedControlStatementSeparator(self: *Parser) bool {
    const token = self.peek() catch return false;
    if (!isKind(token, .lbrace)) {
        return self.acceptedKind(.semicolon);
    }
    if (std.mem.eql(u8, self.this_token.string, ";")) {
        return self.acceptedKind(.semicolon);
    }
    return false;
}

fn ignoreNewline(self: *Parser) bool {
    return self.expression_depth > 0;
}

fn advancePossibleNewline(self: *Parser) !bool {
    if (isNewline(self.this_token)) {
        _ = try self.advance();
        return true;
    }
    return false;
}

fn advancePossibleNewlineWithin(self: *Parser) !bool {
    const token = self.this_token;
    if (!isNewline(token)) return false;
    const next = try self.peek();
    if (token.location.line + 1 < next.location.line) return false;
    if (!isKind(next, .lbrace) and !isKeyword(next, .@"else") and !isKeyword(next, .where)) return false;
    _ = try self.advance();
    return true;
}

fn advance(self: *Parser) !Lexer.Token {
    const last = self.this_token;
    self.last_token = last;
    self.this_token = self.lexer.next();
    while (isKind(self.this_token, .comment)) {
        self.this_token = self.lexer.next();
    }
    if (isKind(self.this_token, .semicolon)) {
        if (self.ignoreNewline() and std.mem.eql(u8, self.this_token.string, "\n")) {
            _ = try self.advance();
        }
    }
    return last;
}

fn expectKind(self: *Parser, kind: lexemes.TokenKind) !Lexer.Token {
    const token = self.this_token;
    if (!isKind(token, kind)) {
        self.err("Expected token of kind `{s}`, got `{s}`", .{ @tagName(kind), @tagName(token.un) });
        return error.Parse;
    }
    return try self.advance();
}

fn expectOperator(self: *Parser, kind: lexemes.OperatorKind) !Lexer.Token {
    const token = self.this_token;
    if ((isOperator(token, .in) or isOperator(token, .not_in)) and
        (self.expression_depth >= 0 or self.allow_in))
    {
        // empty
    } else if (!isOperator(token, kind)) {
        self.err("Expected operator `{s}`, got `{s}`", .{ lexemes.operatorName(kind), @tagName(token.un) });
        return error.Parse;
    }
    return try self.advance();
}

fn expectKeyword(self: *Parser, kind: lexemes.KeywordKind) !Lexer.Token {
    const token = self.this_token;
    if (!isKeyword(token, kind)) {
        self.err("Expected keyword `{s}`, got `{s}`", .{ @tagName(kind), @tagName(token.un) });
        return error.Parse;
    }
    return try self.advance();
}

fn expectAssignment(self: *Parser, kind: lexemes.AssignmentKind) !Lexer.Token {
    const token = self.this_token;
    if (!isAssignment(token, kind)) {
        self.err("Expected assignment `{s}`, got `{s}`", .{
            lexemes.assignment_tokens.get(kind),
            @tagName(token.un),
        });
        return error.Parse;
    }
    return try self.advance();
}

fn expectLiteral(self: *Parser, kind: lexemes.LiteralKind) !Lexer.Token {
    const token = self.this_token;
    if (!isLiteral(token, kind)) {
        self.err("Expected literal `{s}`, got `{s}`", .{ @tagName(kind), @tagName(token.un) });
        return error.Parse;
    }
    return try self.advance();
}

fn expectSemicolon(self: *Parser) !void {
    if (self.acceptedKind(.semicolon)) return;

    const token = self.this_token;
    if (isKind(token, .rbrace) or isOperator(token, .rparen)) {
        if (token.location.line == self.last_token.location.line) return;
    }

    if (isKind(self.last_token, .semicolon)) return;
    if (isKind(self.this_token, .eof)) return;

    if (token.location.line == self.last_token.location.line) {
        self.err("Expected semicolon, got `{s}`", .{@tagName(token.un)});
        return error.Parse;
    }
}

fn parseIdentifier(self: *Parser, poly: bool) !Ast.Index {
    try self.record();

    var token = self.this_token;
    if (isKind(token, .identifier)) {
        _ = try self.advance();
    } else if (isKind(token, .@"const")) {
        _ = try self.advance();
        if (!self.acceptedKind(.identifier)) {
            self.err("Expected identifier after `$`, got `{s}`", .{@tagName(self.this_token.un)});
            return error.Parse;
        }
        token = self.this_token;
    } else {
        self.err("Expected identifier or `$`, got `{s}`", .{@tagName(token.un)});
        return error.Parse;
    }

    return try self.ast.appendExpression(.{ .identifier = .{
        .contents = token.string,
        .poly = poly,
    } });
}

const builtin_types = .{
    .{ .bool, "bool", 1, .little },
    .{ .bool, "b8", 1, .little },
    .{ .bool, "b16", 2, .little },
    .{ .bool, "b32", 4, .little },
    .{ .bool, "b64", 8, .little },

    .{ .sint, "i8", 1, .little },
    .{ .sint, "i16", 2, .little },
    .{ .sint, "i32", 4, .little },
    .{ .sint, "i64", 8, .little },

    .{ .sint, "i16le", 2, .little },
    .{ .sint, "i32le", 4, .little },
    .{ .sint, "i64le", 8, .little },

    .{ .sint, "i16be", 2, .big },
    .{ .sint, "i32be", 4, .big },
    .{ .sint, "i64be", 8, .big },

    .{ .uint, "u8", 1, .little },
    .{ .uint, "u16", 2, .little },
    .{ .uint, "u32", 4, .little },
    .{ .uint, "u64", 8, .little },

    .{ .uint, "u16le", 2, .little },
    .{ .uint, "u32le", 4, .little },
    .{ .uint, "u64le", 8, .little },

    .{ .float, "f16", 2, .little },
    .{ .float, "f32", 4, .little },
    .{ .float, "f64", 8, .little },

    .{ .float, "f16le", 2, .little },
    .{ .float, "f32le", 4, .little },
    .{ .float, "f64le", 8, .little },

    .{ .float, "f16be", 2, .big },
    .{ .float, "f32be", 4, .big },
    .{ .float, "f64be", 8, .big },

    .{ .string, "string", 0, .little },
    .{ .cstring, "cstring", 0, .little },
    .{ .pointer, "rawptr", 0, .little },
    .{ .uintptr, "uintptr", 0, .little },
};

fn builtinType(self: *Parser, identifier: Ast.Expression.Identifier) !?Ast.Index {
    inline for (builtin_types) |bt| {
        const kind: Ast.BuiltinTypeKind = bt.@"0";
        const name: []const u8 = bt.@"1";
        const size: u32 = bt.@"2";
        const endianness: Ast.Endianess = bt.@"3";

        if (std.mem.eql(u8, name, identifier.contents)) {
            return try self.ast.appendExpression(.{ .typ = .{ .builtin = .{
                .identifier = identifier.contents,
                .kind = kind,
                .size = size,
                .alignof = size,
                .endianness = endianness,
            } } });
        }
    }

    // TODO: Custom builtin types
    // for (self.context.custom_builtin_types orelse &.{}) |bt| {
    //     if (std.mem.eql(u8, bt.identifier, identifier.contents)) {
    //         return try self.ast.appendExpression(.{
    //             .typ = .{ .builtin = .{
    //                 .identifier = identifier.contents,
    //                 .kind = bt.kind,
    //                 .size = bt.size,
    //                 .alignof = bt.size,
    //                 .endianness = bt.endianness,
    //             } },
    //         });
    //     }
    // }

    return null;
}

fn parseOperand(self: *Parser, lhs: bool) !?Ast.Index {
    switch (self.this_token.un) {
        .identifier => {
            const ident = try self.parseIdentifierExpression();
            const expr = self.ast.getExpression(ident);
            if (try self.builtinType(expr.identifier)) |ty| {
                return ty;
            }
            return ident;
        },
        .literal => return try self.parseLiteralExpression(),
        .lbrace => {
            if (!lhs) {
                return try self.parseCompoundLiteralExpression(null);
            }
        },
        .directive => return try self.parseDirectivePrefix(lhs),
        .@"const" => return try self.parsePolyTypeExpression(),
        .undefined => return try self.parseUndefinedExpression(),
        .keyword => |kw| switch (kw) {
            .distinct => return try self.parseDistinctTypeExpression(),
            .proc => {
                _ = try self.expectKeyword(.proc);
                // proc group or proc
                if (isKind(self.this_token, .lbrace)) {
                    return try self.parseProcedureGroupExpression();
                }
                return try self.parseProcedure();
            },
            .bit_set => return try self.parseBitSetTypeExpression(),
            .typeid => return try self.parseTypeidTypeExpression(),
            .map => return try self.parseMapTypeExpression(),
            .matrix => return try self.parseMatrixTypeExpression(),
            .@"struct" => return try self.parseStructTypeExpression(),
            .@"union" => return try self.parseUnionTypeExpression(),
            .@"enum" => return try self.parseEnumTypeExpression(),
            .context => {
                _ = try self.expectKeyword(.context);
                return try self.ast.appendExpression(.context);
            },
            else => {},
        },
        .operator => |op| switch (op) {
            .lparen => {
                _ = try self.expectOperator(.lparen);
                if (isOperator(self.last_token, .rparen)) {
                    self.err("Expected expression, got `()`", .{});
                    return error.Parse;
                }
                const depth = self.expression_depth;
                const allow_newline = self.allow_newline;
                if (depth < 0) self.allow_newline = false;
                self.expression_depth = @max(depth, 0) + 1;
                const operand = try self.parseExpression(false);
                _ = try self.expectOperator(.rparen);
                self.expression_depth = depth;
                self.allow_newline = allow_newline;
                return operand;
            },
            .pointer => return try self.parsePointerTypeExpression(),
            .lbracket => {
                _ = try self.expectOperator(.lbracket);
                if (isOperator(self.this_token, .pointer)) {
                    return try self.parseMultiPointerTypeExpression();
                } else if (isOperator(self.this_token, .question)) {
                    return try self.parseArrayTypeExpression(false);
                } else if (self.acceptedKeyword(.dynamic)) {
                    return try self.parseDynamicArrayTypeExpression();
                } else if (!isOperator(self.this_token, .rbracket)) {
                    return try self.parseArrayTypeExpression(true);
                } else if (self.acceptedOperator(.rbracket)) {
                    return try self.parseSliceTypeExpression();
                }
            },
            else => {},
        },
        else => {},
    }
    return null;
}

fn parseExpression(self: *Parser, lhs: bool) !Ast.Index {
    return self.parseBinaryExpression(lhs, 1) catch return error.Parse;
}

fn parseAtomExpression(self: *Parser, opt_operand: ?Ast.Index, lhs: bool) !?Ast.Index {
    if (opt_operand == null) {
        if (self.allow_type) {
            return null;
        }
        self.err("Expected operand, got `{s}`", .{@tagName(self.this_token.un)});
    }
    var operand = opt_operand.?;

    var still_lhs = lhs;
    while (true) {
        switch (self.this_token.un) {
            .operator => |op| switch (op) {
                .lparen => return try self.parseCallExpression(operand),
                .period => {
                    _ = try self.expectOperator(.period);
                    switch (self.this_token.un) {
                        .identifier => {
                            operand = try self.ast.appendExpression(.{ .selector = .{
                                .operand = operand,
                                .field = try self.parseIdentifier(false),
                            } });
                        },
                        .operator => |in_op| switch (in_op) {
                            .lparen => {
                                _ = try self.expectOperator(.lparen);
                                const access = try self.parseIdentifier(false);
                                _ = try self.expectOperator(.rparen);
                                operand = try self.ast.appendExpression(.{ .unwrap = .{
                                    .operand = operand,
                                    .access = access,
                                } });
                            },
                            .question => {
                                _ = try self.expectOperator(.question);
                                operand = try self.ast.appendExpression(.{ .unwrap = .{
                                    .operand = operand,
                                    .access = null,
                                } });
                            },
                            else => {},
                        },
                        else => {
                            self.err("Expected selector in selector expression", .{});
                            return error.Parse;
                        },
                    }
                },
                .arrow => {
                    _ = try self.expectOperator(.arrow);
                    operand = try self.ast.appendExpression(.{ .selector = .{
                        .operand = operand,
                        .field = try self.parseIdentifier(false),
                    } });
                },
                .lbracket => {
                    operand = try self.parseIndexExpression(operand);
                },
                .pointer, .or_return, .or_else => |in_op| {
                    _ = try self.expectOperator(in_op);
                    operand = try self.ast.appendExpression(.{ .unary = .{
                        .operand = operand,
                        .operator = in_op,
                    } });
                },
                else => return operand,
            },
            .lbrace => {
                if (!still_lhs and self.expression_depth >= 0) {
                    const ty = try self.ast.appendExpression(.{ .typ = .{
                        .expression = operand,
                    } });
                    operand = try self.parseCompoundLiteralExpression(ty);
                } else {
                    return operand;
                }
            },
            else => return operand,
        }
        still_lhs = false;
    }

    return operand;
}

fn parseTypeOrIdentifier(self: *Parser) !?Ast.Index {
    const depth = self.expression_depth;
    const allow_type = self.allow_type;
    self.expression_depth = -1;
    self.allow_type = true;

    const operand = try self.parseOperand(true);
    const expression = self.parseAtomExpression(operand, true);

    self.expression_depth = depth;
    self.allow_type = allow_type;

    if (expression) |ex| {
        if (ex) |inner_ex| {
            const got_inner_ex = self.ast.getExpression(inner_ex);
            return switch (got_inner_ex) {
                .typ => got_inner_ex,
                else => try self.ast.appendExpression(.{ .typ = .{ .expression = .{
                    .expression = inner_ex,
                } } }),
            };
        }
    } else |_| {
        // ignore error
    }

    return null;
}

fn parseType(self: *Parser) Error!Ast.Index {
    const typ = self.parseTypeOrIdentifier() catch return error.Parse;
    if (typ == null) {
        self.err("Expected type, got `{s}`", .{@tagName(self.this_token.un)});
        return error.Parse;
    }
    return typ.?;
}

fn parseCompoundLiteralExpression(self: *Parser, typ: ?Ast.Index) !Ast.Index {
    var buf = std.mem.zeroes([512]u8);
    var fba = std.heap.FixedBufferAllocator.init(&buf);

    var fields = std.ArrayList(Ast.Expression).init(fba.allocator());
    const depth = self.expression_depth;
    self.expression_depth = 0;

    _ = try self.expectKind(.lbrace);
    while (!isKind(self.this_token, .rbrace) and !isKind(self.this_token, .eof)) {
        const element = self.parseValue() catch return error.Parse;
        const name = self.evaluateIdentifierExpression(element);
        if (isAssignment(self.this_token, .eq)) {
            _ = try self.expectAssignment(.eq);
            const value = self.parseValue() catch return error.Parse;
            try fields.append(.{ .field = .{
                .name = name,
                .value = value,
            } });
        } else {
            try fields.append(.{ .field = .{
                .name = name,
            } });
        }
        if (!self.acceptedSeparator()) break;
    }

    _ = try self.expectClosing(.rbrace);
    self.expression_depth = depth;

    return try self.ast.appendExpression(.{ .compound_literal = .{
        .typ = typ,
        .fields = try self.ast.appendManyExpression(fields.items),
    } });
}

fn parseVariableNameOrType(self: *Parser) Error!Ast.Index {
    if (isOperator(self.this_token, .ellipsis)) {
        _ = try self.advance();
        if (try self.parseTypeOrIdentifier()) |ty| {
            return ty;
        }
        self.err("Expected type after `...`, got `{s}`", .{@tagName(self.this_token.un)});
        return error.Parse;
    } else if (isKeyword(self.this_token, .typeid)) {
        _ = try self.expectKeyword(.typeid);
        var spec: ?Ast.Index = null;
        if (self.acceptedOperator(.quo)) {
            spec = try self.parseType();
        }
        return try self.ast.appendExpression(.{ .typ = .{ .typeid = .{
            .specialisation = spec,
        } } });
    }

    return try self.parseType();
}

fn evaluateIdentifierExpression(self: *Parser, expression: Ast.Index) ?Ast.Index {
    return switch (self.ast.getExpression(expression)) {
        .identifier => expression,
        .typ => self.evaluateIdentifierType(expression),
        .selector => |s| blk: {
            if (s.operand == null) break :blk s.field;
            break :blk null;
        },
        else => null,
    };
}

fn evaluateIdentifierType(self: *Parser, ty: Ast.Index) ?Ast.Index {
    return switch (self.ast.getExpression(ty).typ) {
        .expression => |ex| self.evaluateIdentifierExpression(ex.expression),
        .poly => |ply| blk: {
            if (ply.specialisation == null) break :blk self.evaluateIdentifierType(ply.typ);
            break :blk null;
        },
        else => null,
    };
}

fn parseFieldFlags(self: *Parser) !Ast.FieldFlags {
    var flags = Ast.FieldFlags{};
    if (isKeyword(self.this_token, .using)) {
        _ = try self.advance();
        flags.using = true;
    }

    while (isKind(self.this_token, .directive)) {
        switch (self.this_token.un.directive) {
            .any_int => flags.any_int = true,
            .c_vararg => flags.c_vararg = true,
            .no_alias => flags.no_alias = true,
            .subtype => flags.subtype = true,
            .@"const" => flags.@"const" = true,
            else => {
                self.err("Unknown field flag `{s}`", .{@tagName(self.this_token.un.directive)});
                return error.Parse;
            },
        }
        _ = try self.advance();
    }

    return flags;
}

fn parseContinue(self: *Parser, is_struct: bool) bool {
    return (if (is_struct) !isKind(self.this_token, .rbrace) else !isOperator(self.this_token, .colon)) and
        !isKind(self.this_token, .eof);
}

fn parseFieldList(self: *Parser, is_struct: bool) !Ast.ManyIndex {
    var buf = std.mem.zeroes([1024]u8);
    var fba = std.heap.FixedBufferAllocator.init(&buf);

    var fields = std.ArrayList(Ast.Expression).init(fba.allocator());
    if (isOperator(self.this_token, .rparen)) return .none;

    const allow_newline = self.allow_newline;
    self.allow_newline = true;

    var type_list = std.ArrayList(Ast.Index).init(fba.allocator());
    var attributes = std.ArrayList(Ast.ManyIndex).init(fba.allocator());
    var field_flags = std.ArrayList(Ast.FieldFlags).init(fba.allocator());

    while (self.parseContinue(is_struct)) {
        var f_type: ?Ast.Index = null;
        const start_type_index = type_list.items.len;
        while (self.parseContinue(is_struct)) {
            // check for attributes
            try attributes.append(try self.parseSomeAttributes());

            const flags = try self.parseFieldFlags();
            const name_or_type = try self.parseVariableNameOrType();
            try type_list.append(name_or_type);
            try field_flags.append(flags);
            if (!self.acceptedSeparator()) break;
        }
        const end_type_index = type_list.items.len;

        if (isOperator(self.this_token, .colon)) {
            _ = try self.expectOperator(.colon);
            if (!isAssignment(self.this_token, .eq)) {
                f_type = try self.parseVariableNameOrType();
            }
        }

        var default_value: ?Ast.Index = null;
        if (self.acceptedAssignment(.eq)) {
            default_value = try self.parseExpression(true);
        }

        var tag: ?[]const u8 = null;
        if (f_type != null and default_value == null and isLiteral(self.this_token, .string)) {
            tag = self.this_token.string;
            _ = try self.advance();
        }

        for (start_type_index..end_type_index) |i| {
            const name = type_list.items[i];
            const flags = field_flags.items[i];
            const ident = self.evaluateIdentifierType(name);
            if (ident == null) {
                self.err("Expected identifier, got `{s}`", .{@tagName(self.ast.getExpression(name))});
                return error.Parse;
            }
            try fields.append(.{ .field = .{
                .name = ident,
                .typ = f_type,
                .value = default_value,
                .tag = tag,
                .flags = flags,
                .attributes = attributes.items[i],
            } });
        }

        if (!self.acceptedSeparator()) {
            break;
        }
    }
    self.allow_newline = allow_newline;

    return try self.ast.appendManyExpression(fields.items);
}

fn parseProcedureResults(self: *Parser, no_return: *bool) !Ast.ManyIndex {
    if (!self.acceptedOperator(.arrow)) {
        return .none;
    }

    if (self.acceptedOperator(.not)) {
        no_return.* = true;
        return .none;
    }

    // not a tuple
    if (!isOperator(self.this_token, .lparen)) {
        const attributes = try self.parseSomeAttributes();
        const ty = try self.parseType();
        try self.record();
        const ident = try self.ast.appendExpression(.{ .identifier = .{
            .contents = "_unnamed",
            .polymorphic = false,
        } });
        return try self.ast.appendManyExpression(&.{Ast.Expression{ .field = .{
            .name = ident,
            .typ = ty,
            .attributes = attributes,
        } }});
    }

    // we have a tuple
    _ = try self.expectOperator(.lparen);

    const fields = try self.parseFieldList(false);
    _ = try self.advancePossibleNewline();
    _ = try self.expectOperator(.rparen);

    return fields;
}

fn parseProcedureType(self: *Parser) !Ast.Index {
    var cc = Ast.CallingConvention.rl;
    if (isLiteral(self.this_token, .string)) {
        const token = try self.expectLiteral(.string);
        cc = std.meta.stringToEnum(Ast.CallingConvention, token.string) orelse {
            self.err("Unknown calling convention `{s}`", .{token.string});
            return error.Parse;
        };
    }

    _ = try self.expectOperator(.lparen);
    const params = try self.parseFieldList(false);
    _ = try self.advancePossibleNewline();
    _ = try self.expectOperator(.rparen);

    var is_generic = false;
    const params_list = self.ast.getManyExpression(params);
    for (params_list) |param| {
        if (param.field.name) |n| {
            if (self.ast.getExpression(n).identifier.polymorphic) {
                is_generic = true;
                break;
            }
        }
    }

    var diverging = false;
    const results = try self.parseProcedureResults(&diverging);

    const flags = Ast.ProcedureFlags{
        .bounds_check = true,
        .type_assert = true,
        .diverging = diverging,
    };

    return self.ast.appendExpression(.{ .typ = .{ .procedure = .{
        .convention = cc,
        .params = params,
        .results = results,
        .flags = flags,
        .kind = if (is_generic) .generic else .concrete,
    } } });
}

fn parseBody(self: *Parser, block_flags: Ast.BlockFlags) !Ast.Index {
    const depth = self.expression_depth;
    const allow_newline = self.allow_newline;
    self.expression_depth = 0;
    self.allow_newline = true;

    _ = try self.expectKind(.lbrace);
    const statements = try self.parseStatementList(block_flags);
    _ = try self.expectKind(.rbrace);

    self.allow_newline = allow_newline;
    self.expression_depth = depth;

    return try self.ast.appendExpression(.{ .block = .{
        .flags = block_flags,
        .statements = statements,
        .label = null,
    } });
}

fn parseRhsTupleExpression(self: *Parser) !Ast.ManyIndex {
    return try self.parseTupleExpression(false);
}

fn parseProcedure(self: *Parser) !Ast.Index {
    const typ = try self.parseProcedureType();
    _ = try self.advancePossibleNewline();

    var where_clauses: ?Ast.ManyIndex = null;
    if (isKeyword(self.this_token, .where)) {
        _ = try self.expectKeyword(.where);
        const depth = self.expression_depth;
        self.expression_depth = -1;
        where_clauses = try self.parseRhsTupleExpression();
        self.expression_depth = depth;
    }

    var flags = self.ast.getExpression(typ).typ.procedure.flags;

    while (isKind(self.this_token, .directive)) {
        const token = try self.expectKind(.directive);
        switch (token.un.directive) {
            .optional_ok => flags.optional_ok = true,
            .optional_allocator_error => flags.optional_allocation_error = true,
            .no_bounds_check => flags.bounds_check = false,
            .bounds_check => flags.bounds_check = true,
            .no_type_assert => flags.type_assert = false,
            .type_assert => flags.type_assert = true,
            else => {
                self.err("Unknown procedure flag `{s}`", .{@tagName(token.un.directive)});
                return error.Parse;
            },
        }
    }

    self.ast.mutateExpression(typ).typ.procedure.flags = flags;

    if (self.acceptedKind(.undefined)) {
        if (where_clauses) |_| {
            self.err("Procedures with `where` clauses need bodies", .{});
            return error.Parse;
        }
        return try self.ast.appendExpression(.{ .procedure = .{
            .typ = typ,
            .where_clauses = where_clauses,
            .body = null,
        } });
    } else if (isKind(self.this_token, .lbrace)) {
        const block_flags = Ast.BlockFlags{
            .bounds_check = flags.bounds_check,
            .type_assert = flags.type_assert,
        };
        self.this_procedure = typ;
        const body = try self.parseBody(block_flags);
        return try self.ast.appendExpression(.{ .procedure = .{
            .typ = typ,
            .where_clauses = where_clauses,
            .body = body,
        } });
    }

    // no body or equivalent, must be a type
    return try self.ast.appendExpression(.{
        .typ = .{ .expression = .{
            .expression = typ,
        } },
    });
}

fn parseCallExpression(self: *Parser, operand: Ast.Index) !Ast.Index {
    var buf = std.mem.zeroes([512]u8);
    var fba = std.heap.FixedBufferAllocator.init(&buf);

    var arguments = std.ArrayList(Ast.Expression).init(fba.allocator());
    const depth = self.expression_depth;
    const allow_newline = self.allow_newline;
    self.expression_depth = 0;
    self.allow_newline = true;

    _ = try self.expectOperator(.lparen);

    var seen_ellipsis = false;
    while (!isOperator(self.this_token, .rparen) and !isKind(self.this_token, .eof)) {
        if (isOperator(self.this_token, .comma)) {
            self.err("Expected expression, got `,`", .{});
            return error.Parse;
        } else if (isAssignment(self.this_token, .eq)) {
            self.err("Expected expression, got `=`", .{});
            return error.Parse;
        }

        var has_ellipsis = false;
        if (isOperator(self.this_token, .ellipsis)) {
            has_ellipsis = true;
            _ = try self.expectOperator(.ellipsis);
        }

        const argument = try self.parseExpression(false);
        if (isAssignment(self.this_token, .eq)) {
            _ = try self.expectAssignment(.eq);
            if (has_ellipsis) {
                self.err("Cannot apply `..`", .{});
                return error.Parse;
            }
            if (self.evaluateIdentifierExpression(argument)) |name| {
                try arguments.append(.{ .field = .{
                    .name = name,
                    .value = try self.parseValue(),
                } });
            } else {
                self.err("Expected identifier, got `{s}`", .{@tagName(self.ast.getExpression(argument).*)});
            }
        } else {
            if (seen_ellipsis) {
                self.err("Positional arguments not allowed after `..`", .{});
                return error.Parse;
            }
            try arguments.append(.{ .field = .{
                .value = argument,
            } });
        }

        if (has_ellipsis) {
            seen_ellipsis = true;
        }

        if (!self.acceptedSeparator()) break;
    }

    _ = try self.expectOperator(.rparen);

    self.allow_newline = allow_newline;
    self.expression_depth = depth;

    return try self.ast.appendExpression(.{ .call = .{
        .operand = operand,
        .arguments = try self.ast.appendManyExpression(arguments.items),
    } });
}

fn parseStatement(self: *Parser, block_flags: Ast.BlockFlags) Error!?Ast.Index {
    const token = self.this_token;
    return switch (token.un) {
        .eof => {
            self.err("Unexpected end of file", .{});
            return error.Parse;
        },
        .literal => |lit| switch (lit) {
            inline else => return try self.parseBasicSimpleStatement(block_flags),
        },
        .keyword => |kw| switch (kw) {
            .context, .proc => return try self.parseBasicSimpleStatement(block_flags),
            .foreign => return try self.parseForignDeclarationStatement(),
            .@"if" => return try self.parseIfStatement(block_flags),
            .when => return try self.parseWhenStatement(),
            .import => return try self.parseImportStatement(false),
            .@"for" => return try self.parseForStatement(block_flags),
            .@"switch" => return try self.parseSwitchStatement(block_flags),
            .@"defer" => return try self.parseDeferStatement(block_flags),
            .@"return" => return try self.parseReturnStatement(),
            .@"break",
            .@"continue",
            .fallthrough,
            => |in_kw| return try self.parseBranchStatement(in_kw),
            .using => return try self.parseUsingStatement(),
            .package => return try self.parsePackageStatement(),
            else => {
                self.err("Unexpected keyword `{s}`", .{@tagName(kw)});
                return error.Parse;
            },
        },
        .identifier => return try self.parseBasicSimpleStatement(block_flags),
        .operator => |op| switch (op) {
            .lparen,
            .pointer,
            .add,
            .sub,
            .xor,
            .not,
            .@"and",
            => return try self.parseBasicSimpleStatement(block_flags),
            else => {
                self.err("Unexpected operator `{s}`", .{@tagName(op)});
                return error.Parse;
            },
        },
        .attribute => return try self.parseAttributesForStatement(block_flags),
        .directive => return try self.parseDirectiveForStatement(block_flags),
        .lbrace => return try self.parseBlockStatement(block_flags, false),
        .semicolon => return try self.parseEmptyStatement(),
        else => {
            self.err("Unexpected token `{s}` in statement", .{@tagName(token.un)});
            return error.Parse;
        },
    };
}

fn parseUnaryExpression(self: *Parser, lhs: bool) !Ast.Index {
    switch (self.this_token.un) {
        .operator => |op| switch (op) {
            .transmute, .auto_cast, .cast => return try self.parseCastExpression(lhs),
            .add, .sub, .xor, .@"and", .not => return try self.parseUnaryStemExpression(lhs),
            .period => return try self.parseImplicitSelectorExpression(),
            else => {},
        },
        else => {},
    }

    const operand = try self.parseOperand(lhs);
    return (try self.parseAtomExpression(operand, lhs)).?;
}

fn parseDirectiveCallExpression(self: *Parser, name: []const u8) !Ast.Index {
    try self.record();
    const intrinsics = try self.ast.appendExpression(.{ .identifier = .{
        .contents = "intrinsics",
        .polymorphic = false,
    } });
    const directive = try self.ast.appendExpression(.{ .identifier = .{
        .contents = name,
        .polymorphic = false,
    } });
    const selector = try self.ast.appendExpression(.{ .selector = .{
        .operand = intrinsics,
        .field = directive,
    } });
    return try self.parseCallExpression(selector);
}

fn parseDirectiveForStatement(self: *Parser, block_flags: Ast.BlockFlags) !Ast.Index {
    const token = try self.expectKind(.directive);
    const stmt: ?Ast.Index = switch (token.un.directive) {
        .bounds_check => try self.parseStatement(blk: {
            var flags = block_flags;
            flags.bounds_check = true;
            break :blk flags;
        }),
        .no_bounds_check => try self.parseStatement(blk: {
            var flags = block_flags;
            flags.bounds_check = false;
            break :blk flags;
        }),
        .type_assert => try self.parseStatement(blk: {
            var flags = block_flags;
            flags.type_assert = true;
            break :blk flags;
        }),
        .no_type_assert => try self.parseStatement(blk: {
            var flags = block_flags;
            flags.type_assert = false;
            break :blk flags;
        }),
        .partial => try self.parseStatement(blk: {
            // TODO: partial
            // var flags = block_flags;
            // flags.partial = true;
            break :blk block_flags;
        }),
        .assert => try self.parseDirectiveCallExpression("assert"),
        .panic => try self.parseDirectiveCallExpression("panic"),
        .load => try self.parseDirectiveCallExpression("load"),
        .unroll => try self.parseStatement(blk: {
            // TODO: unroll
            // var flags = block_flags;
            // flags.partial = true;
            break :blk block_flags;
        }),
        .force_inline, .force_no_inline => try self.parseStatement(blk: {
            // TODO: force_inline, no_inline
            // var flags = block_flags;
            // flags.partial = true;
            break :blk block_flags;
        }),
        else => {
            self.err("Unknown statement directive `{s}`", .{@tagName(token.un.directive)});
            return error.Parse;
        },
    };

    return stmt orelse {
        self.err("Expected statement after directive `{s}`", .{@tagName(token.un.directive)});
        return error.Parse;
    };
}

fn parseValue(self: *Parser) Error!Ast.Index {
    if (isKind(self.this_token, .lbrace)) {
        return self.parseCompoundLiteralExpression(null) catch return error.Parse;
    }
    return self.parseExpression(false) catch return error.Parse;
}

fn parseAttributesStatementTail(
    self: *Parser,
    block_flags: Ast.BlockFlags,
    attributes: Ast.ManyIndex,
) !Ast.Index {
    _ = try self.advancePossibleNewline();
    const stmt = (try self.parseStatement(block_flags)) orelse {
        self.err("Attributes must be followed by statements", .{});
        return error.Parse;
    };
    switch (self.ast.mutateExpression(stmt).*) {
        inline .declaration,
        .foreign_block,
        .foreign_import,
        => |*b| b.attributes = attributes,
        else => {
            self.err("Attributes can only be applied to declarations", .{});
            return error.Parse;
        },
    }

    return stmt;
}

fn parseAttributesForStatement(self: *Parser, block_flags: Ast.BlockFlags) !Ast.Index {
    return try self.parseAttributesStatementTail(block_flags, try self.parseAttributes());
}

fn parseAttributes(self: *Parser) !Ast.ManyIndex {
    _ = try self.expectKind(.attribute);
    var buf = std.mem.zeroes([512]u8);
    var fba = std.heap.FixedBufferAllocator.init(&buf);

    var attributes = std.ArrayList(Ast.Expression).init(fba.allocator());
    if (isKind(self.this_token, .identifier)) {
        const ident = try self.parseIdentifier(false);
        const value: ?Ast.Index = blk: {
            if (isOperator(self.this_token, .lparen)) {
                _ = try self.expectOperator(.lparen);
                const found = try self.parseValue();
                _ = try self.expectOperator(.rparen);
                break :blk found;
            }
            break :blk null;
        };
        try attributes.append(.{
            .field = .{
                .name = ident,
                .value = value,
            },
        });
    } else {
        _ = try self.expectOperator(.lparen);
        self.expression_depth += 1;
        while (!isOperator(self.this_token, .rparen) and !isKind(self.this_token, .eof)) {
            const ident = try self.parseIdentifier(false);
            const value: ?Ast.Index = blk: {
                if (isAssignment(self.this_token, .eq)) {
                    _ = try self.expectAssignment(.eq);
                    break :blk try self.parseValue();
                }
                break :blk null;
            };
            try attributes.append(.{ .field = .{
                .name = ident,
                .value = value,
            } });
            if (!self.acceptedSeparator()) break;
        }
        self.expression_depth -= 1;
        _ = try self.expectOperator(.rparen);
    }
    return try self.ast.appendManyExpression(attributes.items);
}

fn parseSomeAttributes(self: *Parser) !Ast.ManyIndex {
    if (!isKind(self.this_token, .attribute)) return .none;
    return try self.parseAttributes();
}

fn parseIdentifierExpression(self: *Parser) !Ast.Index {
    return try self.parseIdentifier(false);
}

fn parseLiteralExpression(self: *Parser) !Ast.Index {
    const token = try self.advance();
    if (token.un != .literal) {
        self.err("Expected literal, got `{s}`", .{@tagName(token.un)});
        return error.Parse;
    }
    return try self.ast.appendExpression(.{ .literal = .{
        .kind = token.un.literal,
        .value = token.string,
    } });
}

fn parseBitSetTypeExpression(self: *Parser) !Ast.Index {
    _ = try self.expectKeyword(.bit_set);
    _ = try self.expectOperator(.lbracket);
    const expression = try self.parseExpression(false);
    const underlying_ty: ?Ast.Index = blk: {
        if (self.acceptedKind(.semicolon)) {
            break :blk try self.parseType();
        }
        break :blk null;
    };
    _ = try self.expectOperator(.rbracket);
    return try self.ast.appendExpression(.{ .typ = .{ .bitset = .{
        .expression = expression,
        .typ = underlying_ty,
    } } });
}

fn parseTypeidTypeExpression(self: *Parser) !Ast.Index {
    _ = try self.expectKeyword(.typeid);
    return try self.ast.appendExpression(.{ .typ = .{ .typeid = .{
        .specialisation = null,
    } } });
}

fn parseMapTypeExpression(self: *Parser) !Ast.Index {
    _ = try self.expectKeyword(.map);
    _ = try self.expectOperator(.lbracket);
    const key_expression = try self.parseExpression(true);
    _ = try self.expectOperator(.rbracket);
    const value = try self.parseType();
    const key = try self.ast.appendExpression(.{ .typ = .{
        .expression = key_expression,
    } });
    return try self.ast.appendExpression(.{ .typ = .{ .map = .{
        .key = key,
        .value = value,
    } } });
}

fn parseMatrixTypeExpression(self: *Parser) !Ast.Index {
    _ = try self.expectKeyword(.matrix);
    _ = try self.expectOperator(.lbracket);
    const rows_expression = try self.parseExpression(true);
    _ = try self.expectOperator(.comma);
    const columns_expression = try self.parseExpression(true);
    _ = try self.expectOperator(.rbracket);

    const base_ty = try self.parseType();
    return try self.ast.appendExpression(.{ .typ = .{ .matrix = .{
        .rows = rows_expression,
        .columns = columns_expression,
        .typ = base_ty,
    } } });
}

fn parsePointerTypeExpression(self: *Parser) !Ast.Index {
    const is_const = self.acceptedKeyword(.@"const");
    _ = try self.expectOperator(.pointer);
    const typ = try self.parseType();
    return try self.ast.appendExpression(.{ .typ = .{ .pointer = .{
        .is_const = is_const,
        .typ = typ,
    } } });
}

fn parseMultiPointerTypeExpression(self: *Parser) !Ast.Index {
    _ = try self.expectOperator(.pointer);
    const is_const = self.acceptedKeyword(.@"const");
    _ = try self.expectOperator(.rbracket);

    const ty = try self.parseType();
    return try self.ast.appendExpression(.{ .typ = .{ .multipointer = .{
        .is_const = is_const,
        .typ = ty,
    } } });
}

fn parseArrayTypeExpression(self: *Parser, parse_count: bool) !Ast.Index {
    const count: ?Ast.Index = blk: {
        if (parse_count) {
            self.expression_depth += 1;
            const c = try self.parseExpression(false);
            self.expression_depth -= 1;
            break :blk c;
        }
        _ = try self.expectOperator(.question);
        break :blk null;
    };
    _ = try self.expectOperator(.rbracket);

    const ty = try self.parseType();
    return try self.ast.appendExpression(.{ .typ = .{ .array = .{
        .count = count,
        .typ = ty,
    } } });
}

fn parseDynamicArrayTypeExpression(self: *Parser) !Ast.Index {
    _ = try self.expectOperator(.rbracket);
    const ty = try self.parseType();
    return try self.ast.appendExpression(.{ .typ = .{ .dynamic_array = .{
        .typ = ty,
    } } });
}

fn parseSliceTypeExpression(self: *Parser) !Ast.Index {
    const is_const = self.acceptedKeyword(.@"const");
    const ty = try self.parseType();
    return try self.ast.appendExpression(.{ .typ = .{ .slice = .{
        .is_const = is_const,
        .typ = ty,
    } } });
}

fn parseDistinctTypeExpression(self: *Parser) !Ast.Index {
    _ = try self.expectKeyword(.distinct);
    const ty = try self.parseType();
    return try self.ast.appendExpression(.{ .typ = .{ .distinct = .{
        .typ = ty,
    } } });
}

fn parseProcedureGroupExpression(self: *Parser) !Ast.Index {
    var buf = std.mem.zeroes([512]u8);
    var fba = std.heap.FixedBufferAllocator.init(&buf);

    var expressions = std.ArrayList(Ast.Index).init(fba.allocator());
    _ = try self.expectKind(.lbrace);
    while (!isKind(self.this_token, .rbrace) and !isKind(self.this_token, .eof)) {
        try expressions.append(try self.ast.appendExpression(.{ .ref = try self.parseExpression(false) }));
        if (!self.acceptedSeparator()) break;
    }
    _ = try self.expectKind(.rbrace);

    return try self.ast.appendExpression(.{ .procedure_group = try self.ast.appendManyExpression(expressions.items) });
}

fn parseStructTypeExpression(self: *Parser) !Ast.Index {
    _ = try self.expectKeyword(.@"struct");
    var parameters: ?Ast.ManyIndex = null;
    // parapoly
    if (self.acceptedOperator(.lparen)) {
        parameters = try self.parseFieldList(false);
        _ = try self.expectOperator(.rparen);
    }

    var alignment: ?Ast.Index = null;
    var flags = Ast.StructFlags{};
    while (self.acceptedKind(.directive)) {
        switch (self.last_token.un.directive) {
            .@"packed" => flags.@"packed" = true,
            .@"align" => {
                const depth = self.expression_depth;
                self.expression_depth = -1;
                defer self.expression_depth = depth;
                _ = try self.expectOperator(.lparen);
                alignment = try self.parseExpression(true);
                _ = try self.expectOperator(.rparen);
            },
            .raw_union => flags.@"union" = true,
            .no_copy => flags.uncopyable = true,
            else => |d| {
                self.err("Unknown struct flag `{s}`", .{@tagName(d)});
                return error.Parse;
            },
        }
    }

    _ = try self.advancePossibleNewline();

    const where_clauses: ?Ast.Index = blk: {
        if (self.acceptedKeyword(.where)) {
            const depth = self.expression_depth;
            self.expression_depth = -1;
            defer self.expression_depth = depth;
            break :blk try self.parseRhsTupleExpression();
        }
        break :blk null;
    };

    _ = try self.advancePossibleNewline();

    _ = try self.expectKind(.lbrace);
    const fields = try self.parseFieldList(true);
    _ = try self.expectKind(.rbrace);

    return try self.ast.appendExpression(.{ .typ = .{ .structure = .{
        .alignment = alignment,
        .flags = flags,
        .fields = fields,
        .where_clauses = where_clauses,
        .parameters = parameters,
    } } });
}

fn parseUnionTypeExpression(self: *Parser) !Ast.Index {
    _ = try self.expectKeyword(.@"union");

    var parameters: ?Ast.ManyIndex = null;
    // parapoly
    if (self.acceptedOperator(.lparen)) {
        parameters = try self.parseFieldList(false);
        _ = try self.expectOperator(.rparen);
    }

    var flags = Ast.UnionFlags{};
    var alignment: ?Ast.Index = null;
    while (self.acceptedKind(.directive)) {
        switch (self.last_token.un.directive) {
            .@"align" => {
                const depth = self.expression_depth;
                self.expression_depth = -1;
                defer self.expression_depth = depth;
                _ = try self.expectOperator(.lparen);
                alignment = try self.parseExpression(true);
                _ = try self.expectOperator(.rparen);
            },
            .no_nil => flags.no_nil = true,
            .shared_nil => flags.shared_nil = true,
            .maybe => flags.maybe = true,
            else => |d| {
                self.err("Unknown union flag `{s}`", .{@tagName(d)});
                return error.Parse;
            },
        }
    }

    _ = try self.advancePossibleNewline();

    const where_clauses: ?Ast.Index = blk: {
        if (self.acceptedKeyword(.where)) {
            const depth = self.expression_depth;
            self.expression_depth = -1;
            const c = try self.parseRhsTupleExpression();
            self.expression_depth = depth;
            break :blk c;
        }
        break :blk null;
    };

    _ = try self.advancePossibleNewline();

    _ = try self.expectKind(.lbrace);
    const variants = try self.parseFieldList(true);
    _ = try self.expectKind(.rbrace);

    return try self.ast.appendExpression(.{ .typ = .{ .@"union" = .{
        .alignment = alignment,
        .flags = flags,
        .variants = variants,
        .where_clauses = where_clauses,
        .parameters = parameters,
    } } });
}

fn parseEnumTypeExpression(self: *Parser) !Ast.Index {
    _ = try self.expectKeyword(.@"enum");
    const base_ty: ?Ast.Index = blk: {
        if (!isKind(self.this_token, .lbrace)) {
            break :blk try self.parseType();
        }
        break :blk null;
    };

    _ = try self.advancePossibleNewline();

    var buf = std.mem.zeroes([512]u8);
    var fba = std.heap.FixedBufferAllocator.init(&buf);

    var fields = std.ArrayList(Ast.Expression).init(fba.allocator());
    _ = try self.expectKind(.lbrace);
    while (!isKind(self.this_token, .rbrace) and !isKind(self.this_token, .eof)) {
        const name = try self.parseValue();
        const got_name = self.ast.getExpression(name);
        if (got_name != .identifier) {
            self.err("Expected identifier for enumerator, got `{s}`", .{@tagName(got_name)});
            return error.Parse;
        }

        var value: ?Ast.Index = null;
        if (isAssignment(self.this_token, .eq)) {
            _ = try self.expectAssignment(.eq);
            value = try self.parseValue();
        }

        try fields.append(.{ .field = .{
            .name = got_name,
            .value = value,
        } });
        if (!self.acceptedSeparator()) break;
    }
    _ = try self.expectKind(.rbrace);

    return try self.ast.appendExpression(.{ .typ = .{ .enumeration = .{
        .typ = base_ty,
        .fields = try self.ast.appendManyExpression(fields.items),
    } } });
}

fn parsePolyTypeExpression(self: *Parser) !Ast.Index {
    _ = try self.expectKind(.@"const");
    const ident = try self.parseIdentifier(true);

    var specialisation: ?Ast.Index = null;
    if (self.acceptedOperator(.quo)) {
        specialisation = try self.parseType();
    }

    return try self.ast.appendExpression(.{ .typ = .{ .poly = .{
        .typ = ident,
        .specialisation = specialisation,
    } } });
}

fn parseUndefinedExpression(self: *Parser) !Ast.Index {
    _ = try self.expectKind(.undefined);
    return try self.ast.appendExpression(.undefined);
}

fn parseDirectivePrefix(self: *Parser, lhs: bool) !Ast.Index {
    switch ((try self.expectKind(.directive)).un.directive) {
        .type => {
            return try self.parseType();
        },
        .simd => {
            const ty = try self.parseType();
            const got_ty = self.ast.getExpression(ty);
            if (got_ty.typ != .array) {
                self.err("Expected array type for `simd`, got `{s}`", .{@tagName(got_ty.typ)});
                return error.Parse;
            }
            // TODO: simd
            return ty;
        },
        .soa, .sparse => {
            return try self.parseType();
        },
        .partial => {
            return try self.parseExpression(lhs);
        },
        .no_bounds_check, .bounds_check, .type_assert, .no_type_assert => {
            return try self.parseExpression(lhs);
        },
        .force_no_inline, .force_inline => {
            return try self.parseExpression(lhs);
        },
        .config => {
            return self.parseDirectiveCallExpression("config");
        },
        .defined => {
            return self.parseDirectiveCallExpression("defined");
        },
        .load => {
            return self.parseDirectiveCallExpression("load");
        },
        .caller_location => {
            const intrinsics = try self.ast.appendExpression(.{ .identifier = .{
                .contents = "intrinsics",
                .polymorphic = false,
            } });
            const caller_location = try self.ast.appendExpression(.{ .identifier = .{
                .contents = "caller_location",
                .polymorphic = false,
            } });
            return try self.ast.appendExpression(.{ .selector = .{
                .operand = intrinsics,
                .field = caller_location,
            } });
        },
        .procedure => {
            const intrinsics = try self.ast.appendExpression(.{ .identifier = .{
                .contents = "intrinsics",
                .polymorphic = false,
            } });
            const procedure = try self.ast.appendExpression(.{ .identifier = .{
                .contents = "procedure",
                .polymorphic = false,
            } });
            return try self.ast.appendExpression(.{ .selector = .{
                .operand = intrinsics,
                .field = procedure,
            } });
        },
        else => {
            self.err("Unknown directive `{s}`", .{@tagName(self.last_token.un.directive)});
            return error.Parse;
        },
    }
}

fn expectClosing(self: *Parser, kind: lexemes.TokenKind) !Lexer.Token {
    const token = self.this_token;
    if (!isKind(token, kind) and isKind(token, .semicolon) and
        (std.mem.eql(u8, token.string, "\n") or isKind(token, .eof)))
    {
        if (self.allow_newline) {
            self.err("Expected `,` before newline", .{});
            return error.Parse;
        }
        _ = try self.advance();
    }
    return self.expectKind(kind);
}

fn parseIndexExpression(self: *Parser, operand: Ast.Index) !Ast.Index {
    var lhs: ?Ast.Index = null;
    var rhs: ?Ast.Index = null;

    self.expression_depth += 1;

    _ = try self.expectOperator(.lbracket);

    if (!isOperator(self.this_token, .ellipsis) and !isOperator(self.this_token, .rangefull) and !isOperator(self.this_token, .rangehalf) and !isOperator(self.this_token, .colon)) {
        lhs = try self.parseExpression(false);
    }

    if (isOperator(self.this_token, .ellipsis) or isOperator(self.this_token, .rangefull) or isOperator(self.this_token, .rangehalf)) {
        self.err("Expected `:` in index expression, not {s}", .{@tagName(self.this_token.un)});
        return error.ParseIndexExpression;
    }

    var interval: ?Lexer.Token = null;
    if (isOperator(self.this_token, .comma) or isOperator(self.this_token, .colon)) {
        interval = try self.advance();
        if (!isOperator(self.this_token, .rbracket) and !isKind(self.this_token, .eof)) {
            rhs = try self.parseExpression(false);
        }
    }

    _ = try self.expectOperator(.rbracket);
    self.expression_depth -= 1;

    // if (interval) |ivl| {
    //     if (isOperator(ivl, .comma))
    //     {

    //     } else {

    //     }
    // } else {
    //     return
    // }
    return try self.ast.appendExpression(.{ .index = .{
        .operand = operand,
        .lhs = lhs,
        .rhs = rhs,
    } });
}

fn parseCastExpression(self: *Parser, lhs: bool) Error!Ast.Index {
    const token = try self.advance();

    var ty: ?Ast.Index = null;
    if (!isOperator(token, .auto_cast)) {
        _ = try self.expectOperator(.lparen);
        ty = try self.parseType();
        _ = try self.expectOperator(.rparen);
    }

    const operand = self.parseUnaryExpression(lhs) catch return error.Parse;
    if (token.un != .operator) {
        self.err("Expected operator, got `{s}`", .{@tagName(token.un)});
        return error.Parse;
    }

    return try self.ast.appendExpression(.{ .cast = .{
        .operation = token.un.keyword,
        .typ = ty orelse {
            self.err("Expected type in cast expression", .{});
            return error.Parse;
        },
        .expression = operand,
    } });
}

fn parseUnaryStemExpression(self: *Parser, lhs: bool) Error!Ast.Index {
    const token = try self.advance();
    const operand = try self.parseUnaryExpression(lhs);
    if (token.un != .operator) {
        self.err("Expected operator, got `{s}`", .{@tagName(token.un)});
        return error.Parse;
    }
    return try self.ast.appendExpression(.{ .unary = .{
        .operator = token.un.operator,
        .operand = operand,
    } });
}

fn parseImplicitSelectorExpression(self: *Parser) !Ast.Index {
    _ = try self.expectOperator(.period);
    const ident = try self.parseIdentifier(false);
    return try self.ast.appendExpression(.{ .selector = .{
        .operand = null,
        .field = ident,
    } });
}

fn parseTernaryExpression(self: *Parser, expression: Ast.Index, lhs: bool) !Ast.Index {
    var condition: ?Ast.Index = null;
    var on_true: ?Ast.Index = null;
    var kind: lexemes.KeywordKind = .@"if";
    if (self.acceptedOperator(.question)) {
        condition = expression;
        on_true = try self.parseExpression(lhs);
        _ = try self.expectOperator(.colon);
    } else if (self.acceptedKeyword(.@"if") or self.acceptedKeyword(.when)) {
        kind = self.last_token.un.keyword;
        condition = try self.parseExpression(lhs);
        on_true = expression;
        _ = try self.expectKeyword(.@"else");
    }

    const on_false = try self.parseExpression(lhs);
    return try self.ast.appendExpression(.{ .ternary = .{
        .operation = kind,
        .condition = condition orelse {
            self.err("Expected expression on the right-hand side of ternary", .{});
            return error.Parse;
        },
        .on_true = on_true orelse {
            self.err("Expected expression on the left-hand side of ternary", .{});
            return error.Parse;
        },
        .on_false = on_false,
    } });
}

pub const precedence = blk: {
    var list = std.EnumArray(lexemes.OperatorKind, i32).initFill(0);
    var ops = lexemes.operators;
    var it = ops.iterator();
    while (it.next()) |op| {
        list.set(op.key, op.value.precedence);
    }
    break :blk list;
};

fn parseBinaryExpression(self: *Parser, lhs: bool, prec: i32) Error!Ast.Index {
    var expr = try self.parseUnaryExpression(lhs);
    var still_lhs = lhs;
    while (true) {
        const token = self.this_token;
        const last = self.last_token;
        var op_prec: i32 = 0;
        if (isKind(token, .operator)) {
            op_prec = precedence.get(token.un.operator);
            if (isOperator(token, .in) or isOperator(token, .not_in)) {
                if (self.expression_depth < 0 and !self.allow_in) {
                    op_prec = 0;
                }
            }
        } else if (isKeyword(token, .@"if") or isKeyword(token, .when)) {
            op_prec = 1;
        }
        if (op_prec < prec) break;

        if (isKeyword(token, .@"if") or isKeyword(token, .when)) {
            if (last.location.line < token.location.line) {
                return expr;
            }
        }

        if (isOperator(token, .question) or isKeyword(token, .@"if") or isKeyword(token, .when)) {
            expr = try self.parseTernaryExpression(expr, still_lhs);
        } else {
            _ = try self.expectKind(.operator);
            const rhs = self.parseBinaryExpression(false, op_prec + 1) catch {
                self.err("Expected expression on the right-hand side", .{});
                return error.Parse;
            };
            expr = try self.ast.appendExpression(.{ .binary = .{
                .operator = token.un.operator,
                .lhs = expr,
                .rhs = rhs,
            } });
        }

        still_lhs = false;
    }

    return expr;
}

fn parseTupleExpression(self: *Parser, lhs: bool) !Ast.ManyIndex {
    const allow_newline = self.allow_newline;
    self.allow_newline = true;

    var buf = std.mem.zeroes([512]u8);
    var fba = std.heap.FixedBufferAllocator.init(&buf);

    var expressions = std.ArrayList(Ast.Expression).init(fba.allocator());
    while (true) {
        try expressions.append(.{ .ref = try self.parseExpression(lhs) });
        if (!isOperator(self.this_token, .comma) or isKind(self.this_token, .eof)) break;
        _ = try self.advance();
    }

    self.allow_newline = allow_newline;
    return try self.ast.appendManyExpression(expressions.items);
}

fn parseLhsTupleExpression(self: *Parser) !Ast.ManyIndex {
    return try self.parseTupleExpression(true);
}

fn parseStatementList(self: *Parser, block_flags: Ast.BlockFlags) !Ast.ManyIndex {
    var buf = std.mem.zeroes([512]u8);
    var fba = std.heap.FixedBufferAllocator.init(&buf);

    var statements = std.ArrayList(Ast.Expression).init(fba.allocator());

    while (!isKeyword(self.this_token, .case) and
        !isKind(self.this_token, .rbrace) and
        !isKind(self.this_token, .eof))
    {
        if (try self.parseStatement(block_flags)) |statement| {
            if (self.ast.getExpression(statement).statement != .empty) {
                try statements.append(.{ .ref = statement });
            }
        }
    }

    return try self.ast.appendManyExpression(statements.items);
}

fn parseBlockStatement(self: *Parser, block_flags: Ast.BlockFlags, when: bool) !Ast.Index {
    _ = try self.advancePossibleNewlineWithin();

    if (!when and self.this_procedure == null) {
        self.err("Blocks can only be used in procedures", .{});
        return error.Parse;
    }

    return try self.parseBody(block_flags);
}

fn parseDeclarationStatementTail(self: *Parser, names: Ast.ManyIndex) !Ast.Index {
    var values: ?Ast.ManyIndex = null;
    const ty = try self.parseTypeOrIdentifier();
    const token = self.this_token;
    var constant = false;

    const names_list = self.ast.getManyExpression(names);
    const n_names = names_list.len;
    if (isAssignment(token, .eq) or isOperator(token, .colon)) {
        const seperator = try self.advance();
        constant = isOperator(seperator, .colon);
        _ = try self.advancePossibleNewline();
        values = try self.parseRhsTupleExpression();
        const values_list = self.ast.getManyExpression(values.?);
        const n_values = values_list.len;
        if (n_values > n_names) {
            self.err("Too many values for declaration, expected `{}`, got `{}`", .{ n_names, n_values });
            return error.Parse;
        } else if (n_values < n_names and constant) {
            self.err("Too few values for constant declarations, expected `{}`, got `{}`", .{ n_names, n_values });
            return error.Parse;
        } else if (n_values == 0) {
            self.err("Expected expression for declaration", .{});
            return error.Parse;
        }
    }

    const n_values = self.ast.getManyExpression(values orelse .none).len;
    if (ty == null) {
        if (constant and n_values == 0) {
            self.err("Expected type for constant declaration", .{});
            return error.Parse;
        } else if (n_values == 0 and n_names > 0) {
            self.err("Expected constant value", .{});
            return error.Parse;
        }
    }

    if (self.expression_depth >= 0) {
        if (isKind(self.this_token, .rbrace) and self.this_token.location.line == self.last_token.location.line) {
            // nothing
        } else {
            try self.expectSemicolon();
        }
    }

    if (self.this_procedure == null and n_values > 0 and n_names != n_values) {
        self.err("Expected `{}` values on right-hand side, got `{}`", .{ n_names, n_values });
        return error.Parse;
    }

    if (ty != null and n_values == 0) {
        var buf = std.mem.zeroes([512]u8);
        var fba = std.heap.FixedBufferAllocator.init(&buf);

        var expressions = std.ArrayList(Ast.Expression).init(fba.allocator());
        for (0..n_names) |_| {
            // zero initialisation
            try expressions.append(.{ .compound_literal = .{
                .typ = ty,
                .fields = .none,
            } });
        }
        values = try self.ast.appendManyExpression(expressions.items);
    }

    return try self.ast.appendExpression(.{ .statement = .{ .declaration = .{
        .identifiers = names,
        .typ = ty,
        .values = values orelse unreachable,
    } } });
}

fn parseDeclarationStatement(self: *Parser, lhs: Ast.ManyIndex) !Ast.Index {
    var buf = std.mem.zeroes([512]u8);
    var fba = std.heap.FixedBufferAllocator.init(&buf);
    var names = std.ArrayList(Ast.Expression).init(fba.allocator());
    var it = self.ast.iterateManyExpression(lhs);
    while (it.next()) |expr| {
        if (self.evaluateIdentifierExpression(expr.index)) |ident| {
            try names.append(.{ .ref = ident });
        } else {
            self.err("Expected identifier, got `{s}`", .{
                @tagName(self.ast.getExpression(expr.index)),
            });
            return error.Parse;
        }
    }

    return try self.parseDeclarationStatementTail(
        try self.ast.appendManyExpression(names.items),
    );
}

fn parseAssignmentStatement(self: *Parser, lhs: Ast.Index) !Ast.Index {
    if (self.this_procedure == null) {
        self.err("Assignment statements can only be used in procedures", .{});
        return error.Parse;
    }

    if (self.this_token.un != .assignment) {
        self.err("Expected assignment operator, got `{s}`", .{@tagName(self.this_token.un)});
        return error.Parse;
    }
    const kind = self.this_token.un.assignment;

    _ = try self.advance();
    const rhs = try self.parseRhsTupleExpression();
    const expression_items = self.ast.getManyExpression(rhs);
    if (expression_items.len == 0) {
        self.err("Missing right-hand side of assignment", .{});
        return error.Parse;
    }

    return try self.ast.appendExpression(.{ .statement = .{ .assignment = .{
        .lhs = lhs,
        .rhs = rhs,
        .kind = kind,
    } } });
}

fn parseSimpleStatement(self: *Parser, block_flags: Ast.BlockFlags, allow_in: bool, allow_label: bool) !Ast.Index {
    const lhs = try self.parseLhsTupleExpression();

    switch (self.this_token.un) {
        .assignment => return try self.parseAssignmentStatement(lhs),
        .operator => |op| switch (op) {
            .in => {
                if (allow_in) {
                    const prev_allow_in = self.allow_in;
                    defer self.allow_in = prev_allow_in;
                    self.allow_in = false;
                    _ = try self.expectOperator(.in);
                    const rhs = try self.parseExpression(true);
                    const expression = try self.ast.appendExpression(.{ .binary = .{
                        .operation = .in,
                        .lhs = lhs,
                        .rhs = rhs,
                    } });
                    return expression;
                }
            },
            .colon => {
                _ = try self.advance();

                var lhs_items = self.ast.iterateManyExpression(lhs);
                if (allow_label and lhs_items.size() == 1) {
                    const token = self.this_token;
                    if (isKind(token, .lbrace) or
                        isKeyword(token, .@"if") or
                        isKeyword(token, .@"for") or
                        isKeyword(token, .@"switch"))
                    {
                        const name = lhs_items.next() orelse unreachable;
                        const label: Ast.Expression = name.expression;
                        const label_index = name.index;
                        if (label != .identifier) {
                            self.err("Expected identifier for label, got `{s}`", .{@tagName(label)});
                            return error.Parse;
                        }
                        const statement = (try self.parseStatement(block_flags)) orelse {
                            self.err("Expected statement after label `{s}`", .{label.identifier.contents});
                            return error.Parse;
                        };
                        const got_statement = self.ast.mutateExpression(statement);
                        switch (got_statement.*) {
                            inline .block,
                            .@"if",
                            .@"for",
                            .@"switch",
                            => |*s| s.label = label_index,
                            else => {
                                self.err("Cannot apply block `{s}` to `{s}`", .{ label.identifier.contents, @tagName(got_statement.*) });
                                return error.Parse;
                            },
                        }
                        return statement;
                    }

                    return try self.parseDeclarationStatement(lhs, false);
                }
            },
            else => {},
        },
        else => {},
    }

    const got_expressions = self.ast.getManyExpression(lhs);
    if (got_expressions.len == 0 or got_expressions.len > 1) {
        self.err("Expected single expression for declaration, got `{}`", .{got_expressions.len});
        return error.Parse;
    }

    return try self.ast.appendExpression(.{ .statement = .{ .expression = got_expressions[0] } });
}

fn parseImportStatement(self: *Parser, is_using: bool) !Ast.Index {
    try self.ast.recordToken(try self.expectKeyword(.import));

    const token: ?Lexer.Token = blk: {
        if (isKind(self.this_token, .identifier)) {
            break :blk try self.advance();
        }
        break :blk null;
    };

    const path = try self.expectLiteral(.string);

    try self.expectSemicolon();

    const name = if (token) |t| t.string else blk: {
        var found: []const u8 = "";
        var it = std.mem.tokenizeAny(u8, path.string, "/:");
        while (it.next()) |set| {
            found = set;
        }
        break :blk found;
    };
    const next = path.string;

    const stmt = blk: {
        if (std.mem.indexOfScalar(u8, next, ':')) |idx| {
            const collection = next[0..idx];
            const pathname = next[idx + 1 ..];
            break :blk try self.ast.appendExpression(.{ .statement = .{ .import = .{
                .name = name,
                .path = pathname,
                .collection = collection,
                .using = is_using,
                .location = self.this_token.location,
            } } });
        } else {
            break :blk try self.ast.appendExpression(.{ .statement = .{ .import = .{
                .name = name,
                .path = next,
                .collection = null,
                .using = is_using,
                .location = self.this_token.location,
            } } });
        }
    };
    try self.ast.addImport(stmt);
    return stmt;
}

fn wrapStatementBlock(self: *Parser, block_flags: Ast.BlockFlags, statement: Ast.Index) !Ast.Index {
    const got_statement = self.ast.getExpression(statement);
    if (got_statement == .block or got_statement == .empty) {
        self.err("Expected regular statement, got `{s}`", .{@tagName(got_statement)});
        return error.Parse;
    }
    return try self.ast.appendExpression(.{ .block = .{
        .flags = block_flags,
        .label = null,
        .statements = try self.ast.appendManyExpression(&.{Ast.Expression{ .ref = statement }}),
    } });
}

fn unwrapStatementExpression(self: *Parser, statement: ?Ast.Index) !?Ast.Index {
    if (statement) |stmt| {
        const got_stmt = self.ast.getExpression(stmt);
        if (got_stmt != .statement) {
            self.err("Expected expression statement, got `{s}`", .{@tagName(got_stmt)});
            return error.Parse;
        }
        return got_stmt.statement.expression;
    } else {
        return null;
    }
}

fn parseDoBody(self: *Parser, block_flags: Ast.BlockFlags) !Ast.Index {
    const depth = self.expression_depth;
    const allow_newline = self.allow_newline;
    self.expression_depth = 0;
    self.allow_newline = false;
    defer {
        self.expression_depth = depth;
        self.allow_newline = allow_newline;
    }

    const statement = try self.parseStatement(block_flags) orelse {
        self.err("Expected body", .{});
        return error.Parse;
    };
    const body = try self.wrapStatementBlock(block_flags, statement);

    return body;
}

fn parseIfStatement(self: *Parser, block_flags: Ast.BlockFlags) !Ast.Index {
    _ = try self.expectKeyword(.@"if");

    const depth = self.expression_depth;
    const allow_in = self.allow_in;
    self.expression_depth = -1;
    self.allow_in = true;

    var init_stmt: ?Ast.Index = try self.parseSimpleStatement(block_flags, false, false);
    var condition: ?Ast.Index = null;
    if (self.acceptedControlStatementSeparator()) {
        condition = try self.parseExpression(false);
    } else {
        condition = try self.unwrapStatementExpression(init_stmt);
        init_stmt = null;
    }

    self.expression_depth = depth;
    self.allow_in = allow_in;

    if (condition == null) {
        self.err("Expected condition for `if` statement", .{});
        return error.Parse;
    }

    var body: ?Ast.Index = null;
    if (self.acceptedKeyword(.do)) {
        body = try self.parseDoBody(block_flags);
    } else {
        body = try self.parseBlockStatement(block_flags, false);
    }

    _ = try self.advancePossibleNewlineWithin();

    var elif: ?Ast.Index = null;
    if (isKeyword(self.this_token, .@"else")) {
        _ = try self.expectKeyword(.@"else");
        if (isKeyword(self.this_token, .@"if")) {
            elif = try self.ast.appendExpression(.{ .block = .{
                .flags = block_flags,
                .label = null,
                .statements = try self.ast.appendManyExpression(&.{Ast.Expression{ .ref = try self.parseIfStatement(block_flags) }}),
            } });
        } else if (isKind(self.this_token, .lbrace)) {
            elif = try self.parseBlockStatement(block_flags, false);
        } else if (isKeyword(self.this_token, .do)) {
            _ = try self.expectKeyword(.do);
            elif = try self.parseDoBody(block_flags);
        } else {
            self.err("Expected block on `else` statement", .{});
            return error.Parse;
        }
    }

    return try self.ast.appendExpression(.{
        .statement = .{ .ifs = .{
            .init = init_stmt,
            .condition = condition,
            .body = body,
            .elif = elif,
            .label = null,
        } },
    });
}

fn parseWhenStatement(self: *Parser) !Ast.Index {
    _ = try self.expectKeyword(.when);

    const expression_depth = self.expression_depth;
    self.expression_depth = -1;
    const condition = self.parseExpression(false);
    self.expression_depth = expression_depth;

    if (condition) |_| {
        // nothing
    } else |_| {
        self.err("Expected condition in `when` statement", .{});
        return error.Parse;
    }

    var body: ?Ast.Index = null;
    const flags = Ast.BlockFlags{};
    if (self.acceptedKeyword(.do)) {
        body = try self.parseDoBody(flags);
    } else {
        body = try self.parseBlockStatement(flags, true);
    }

    _ = try self.advancePossibleNewlineWithin();

    var elif: ?Ast.Index = null;
    if (isKeyword(self.this_token, .@"else")) {
        _ = try self.expectKeyword(.@"else");
        if (isKeyword(self.this_token, .when)) {
            elif = try self.ast.appendExpression(.{ .block = .{
                .flags = flags,
                .label = null,
                .statements = try self.ast.appendManyExpression(&.{Ast.Expression{ .ref = try self.parseWhenStatement() }}),
            } });
        } else if (isKeyword(self.this_token, .do)) {
            _ = try self.expectKeyword(.do);
            elif = try self.parseDoBody(flags);
        } else if (isKind(self.this_token, .lbrace)) {
            elif = try self.parseBlockStatement(flags, true);
        } else {
            self.err("Expected block on `else` statement", .{});
            return error.Parse;
        }
    }

    return try self.ast.appendExpression(.{ .statement = .{ .when = .{
        .condition = try condition,
        .body = body,
        .elif = elif,
    } } });
}

fn parseForStatement(self: *Parser, block_flags: Ast.BlockFlags) !Ast.Index {
    var init_stmt: ?Ast.Index = null;
    var condition: ?Ast.Index = null;
    var body: ?Ast.Index = null;
    var post_stmt: ?Ast.Index = null;

    _ = try self.expectKeyword(.@"for");

    var range = false;

    const token = self.this_token;
    if (!isKind(token, .lbrace) and !isKeyword(token, .do)) {
        const depth = self.expression_depth;
        self.expression_depth = -1;

        if (isOperator(token, .in)) {
            self.err("Use for `for in` not allowed. use `for _ in`", .{});
            return error.Parse;
        }

        if (!isKind(token, .semicolon)) {
            const statement = try self.parseSimpleStatement(block_flags, true, false);
            const got_statement = self.ast.getExpression(statement);
            if (got_statement.statement == .expression) {
                condition = got_statement.statement.expression;
                const got_condition = self.ast.getExpression(condition.?);
                if (got_condition == .binary and got_condition.binary.operation == .in) {
                    range = true;
                }
            } else {
                init_stmt = statement;
                condition = null;
            }
        }

        if (!range and self.acceptedControlStatementSeparator()) {
            const in_token = self.this_token;
            if (isKind(in_token, .lbrace) or isKeyword(in_token, .do)) {
                self.err("Expected `;`", .{});
                return error.Parse;
            } else {
                if (!isKind(token, .semicolon)) {
                    condition = try self.unwrapStatementExpression(
                        try self.parseSimpleStatement(block_flags, false, false),
                    );
                }
                try self.expectSemicolon();
                if (!isKind(self.this_token, .lbrace) and !isKeyword(self.this_token, .do)) {
                    post_stmt = try self.parseSimpleStatement(block_flags, false, false);
                }
            }
        }

        self.expression_depth = depth;
    }

    if (self.acceptedKeyword(.do)) {
        body = try self.parseDoBody(block_flags);
    } else {
        body = try self.parseBlockStatement(block_flags, false);
    }

    return try self.ast.appendExpression(.{
        .statement = .{ .fors = .{
            .init = init_stmt,
            .condition = condition,
            .body = body,
            .post = post_stmt,
            .label = null,
        } },
    });
}

fn parseCaseClause(self: *Parser, block_flags: Ast.BlockFlags, is_type: bool) !Ast.Index {
    _ = try self.expectKeyword(.case);

    const allow_in = self.allow_in;
    self.allow_in = !is_type;

    var list: ?Ast.ManyIndex = null;
    if (!isOperator(self.this_token, .colon)) {
        list = try self.parseRhsTupleExpression();
    }
    self.allow_in = allow_in;

    _ = try self.expectOperator(.colon);

    return try self.ast.appendExpression(.{ .case_clause = .{
        .expression = list,
        .statements = try self.parseStatementList(block_flags),
    } });
}

fn parseSwitchStatement(self: *Parser, block_flags: Ast.BlockFlags) !Ast.Index {
    _ = try self.expectKeyword(.@"switch");

    var init_stmt: ?Ast.Index = null;
    var condition: ?Ast.Index = null;
    if (!isKind(self.this_token, .lbrace)) {
        const depth = self.expression_depth;
        self.expression_depth = -1;

        const statement = try self.parseSimpleStatement(block_flags, true, false);
        const got_statement = self.ast.getExpression(statement).statement;
        if (got_statement == .expression) {
            condition = got_statement.expression;
        } else {
            init_stmt = statement;
            if (self.acceptedControlStatementSeparator()) {
                condition = try self.parseExpression(false);
            } else {
                try self.expectSemicolon();
            }
        }
        self.expression_depth = depth;
    }

    const is_type_switch = if (condition) |c| blk: {
        const got_c = self.ast.getExpression(c);
        break :blk (got_c == .binary and got_c.binary.operation == .in);
    } else false;
    var buf = std.mem.zeroes([512]u8);
    var fba = std.heap.FixedBufferAllocator.init(&buf);
    var clauses = std.ArrayList(Ast.Expression).init(fba.allocator());
    _ = try self.advancePossibleNewline();
    _ = try self.expectKind(.lbrace);
    while (isKeyword(self.this_token, .case)) {
        try clauses.append(Ast.Expression{
            .ref = try self.parseCaseClause(block_flags, is_type_switch),
        });
    }
    _ = try self.expectKind(.rbrace);

    return try self.ast.appendExpression(.{ .statement = .{ .switchs = .{
        .init = init_stmt,
        .condition = condition,
        .label = null,
        .clauses = try self.ast.appendManyExpression(clauses.items),
    } } });
}

fn parseDeferStatement(self: *Parser, block_flags: Ast.BlockFlags) !Ast.Index {
    _ = try self.expectKeyword(.@"defer");
    return try self.ast.newDeferStatement((try self.parseStatement(block_flags)) orelse {
        self.err("Expected statement after `defer`", .{});
        return error.Parse;
    });
}

fn parseReturnStatement(self: *Parser) !Ast.StatementIndex {
    _ = try self.expectKeyword(.@"return");

    if (self.expression_depth > 0) {
        self.err("Cannot return from expression", .{});
        return error.ParseReturnStatement;
    }

    var buf = std.mem.zeroes([512]u8);
    var fba = std.heap.FixedBufferAllocator.init(&buf);

    var results = std.ArrayList(Ast.AnyIndex).init(fba.allocator());
    while (!isKind(self.this_token, .semicolon) and !isKind(self.this_token, .rbrace) and !isKind(self.this_token, .eof)) {
        try results.append(Ast.AnyIndex.from(try self.parseExpression(false)));
        if (!isOperator(self.this_token, .comma) or isKind(self.this_token, .eof)) break;
        _ = try self.advance();
    }

    try self.expectSemicolon();

    return try self.ast.newReturnStatement(try self.ast.newTupleExpression(
        try self.ast.createRef(results.items),
    ));
}

fn parseBasicSimpleStatement(self: *Parser, block_flags: Ast.BlockFlags) !Ast.StatementIndex {
    const statement = try self.parseSimpleStatement(block_flags, false, true);
    try self.expectSemicolon();
    return statement;
}

fn parseForeignBlockStatement(self: *Parser) !Ast.StatementIndex {
    var name: ?Ast.IdentifierIndex = null;
    if (isKind(self.this_token, .identifier)) {
        name = try self.parseIdentifier(false);
    }

    var buf = std.mem.zeroes([512]u8);
    var fba = std.heap.FixedBufferAllocator.init(&buf);

    var statements = std.ArrayList(Ast.AnyIndex).init(fba.allocator());
    _ = try self.advancePossibleNewlineWithin();
    _ = try self.expectKind(.lbrace);
    while (!isKind(self.this_token, .rbrace) and !isKind(self.this_token, .eof)) {
        try statements.append(Ast.AnyIndex.from((try self.parseStatement(Ast.BlockFlags{})) orelse {
            self.err("Expected statement in foreign block", .{});
            return error.ParseForeignBlockStatement;
        }));
    }
    _ = try self.expectKind(.rbrace);

    const block = try self.ast.newBlockStatement(
        Ast.BlockFlags{},
        try self.ast.createRef(statements.items),
        null,
    );
    const statement = try self.ast.newForeignBlockStatement(name, block, Ast.RefIndex.none);
    _ = try self.expectSemicolon();

    return statement;
}

fn parseForiegnImportStatement(self: *Parser) !Ast.StatementIndex {
    _ = try self.expectKeyword(.import);

    var name: ?lexer.Token = null;
    if (isKind(self.this_token, .identifier)) {
        name = try self.advance();
    }

    var sources = std.ArrayList([]const u8).init(self.allocator);
    if (self.acceptedKind(.lbrace)) {
        while (!isKind(self.this_token, .rbrace) and !isKind(self.this_token, .eof)) {
            try sources.append((try self.expectLiteral(.string)).string);
            if (!self.acceptedSeparator()) break;
        }
        _ = try self.expectClosing(.rbrace);
    } else {
        try sources.append((try self.expectLiteral(.string)).string);
    }

    return try self.ast.newForeignImportStatement(
        if (name) |n| n.string else "",
        sources,
        Ast.RefIndex.none,
    );
}

fn parseForignDeclarationStatement(self: *Parser) !?Ast.StatementIndex {
    _ = try self.expectKeyword(.foreign);
    if (isKind(self.this_token, .identifier) or isKind(self.this_token, .lbrace)) {
        return try self.parseForeignBlockStatement();
    } else if (isKeyword(self.this_token, .import)) {
        return try self.parseForiegnImportStatement();
    }

    return null;
}

fn parseBranchStatement(self: *Parser, kind: lexemes.KeywordKind) !Ast.StatementIndex {
    _ = try self.advance();

    var label: ?Ast.IdentifierIndex = null;
    if (isKind(self.this_token, .identifier)) {
        label = try self.parseIdentifier(false);
    }

    return try self.ast.newBranchStatement(kind, label);
}

fn parseUsingStatement(self: *Parser) !Ast.Index {
    _ = try self.expectKeyword(.using);

    if (isKeyword(self.this_token, .import)) {
        return try self.parseImportStatement(true);
    }

    self.err("Expected `import` after `using`", .{});
    return error.Parse;
}

fn parsePackageStatement(self: *Parser) !Ast.StatementIndex {
    _ = try self.expectKeyword(.package);
    const package = try self.expectKind(.identifier);
    try self.ast.recordToken(package);
    return try self.ast.newPackageStatement(package.string);
}

fn parseEmptyStatement(self: *Parser) !Ast.StatementIndex {
    const statement = try self.ast.newEmptyStatement();
    _ = try self.expectSemicolon();
    return statement;
}

fn record(self: *Parser) !void {
    try self.ast.recordToken(self.this_token);
}

// utils
fn isKind(token: Lexer.Token, kind: lexemes.TokenKind) bool {
    return token.un == kind;
}

fn isOperator(token: Lexer.Token, kind: lexemes.OperatorKind) bool {
    return token.un == .operator and token.un.operator == kind;
}

fn isKeyword(token: Lexer.Token, kind: lexemes.KeywordKind) bool {
    return token.un == .keyword and token.un.keyword == kind;
}

fn isAssignment(token: Lexer.Token, kind: lexemes.AssignmentKind) bool {
    return token.un == .assignment and token.un.assignment == kind;
}

fn isLiteral(token: Lexer.Token, kind: lexemes.LiteralKind) bool {
    return token.un == .literal and token.un.literal == kind;
}

fn isNewline(token: Lexer.Token) bool {
    return token.un == .semicolon and std.mem.eql(u8, token.string, "\n");
}

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
