const std = @import("std");

pub const Position = struct {
    line: u32 = 0,
    column: u32 = 0,
    pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        return writer.print("{}:{}", .{ value.line, value.column });
    }
};

pub const Range = struct {
    start: Position = .{},
    end: Position = .{},

    pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        return writer.print("{} -> {}", .{ value.start, value.end });
    }
};

pub const Keyword = enum {
    function,
    @"struct",
    @"enum",
    @"union",
    @"if",
    @"else",
    @"for",
    @"break",
    @"continue",
    @"return",
    @"defer",
};

pub const Token = struct {
    pub const Kind = enum {
        unknown,
        eof,

        string,
        integer,
        float,
        identifier,
        keyword,

        // operators
        @"=",
        @"==",
        @"!=",
        @"+",
        @"-",
        @"*",
        @"/",
        @"%",
        @"<",
        @">",
        @"<=",
        @">=",
        @"&&",
        @"||",
        @"!",
        @"&",
        @"|",
        @"^",
        @"<<",
        @">>",
        @"&^",

        // applied operators
        @"+=",
        @"-=",
        @"*=",
        @"/=",
        @"%=",
        @"<<=",
        @">>=",
        @"&=",
        @"|=",
        @"^=",
        @"&^=",

        // symbols
        @"(",
        @")",
        @"{",
        @"}",
        @"[",
        @"]",
        @",",
        @";",
        @":",
        @".",
        @"..",
        @"...",
        @"->",
        @"?",
        @"@",
        @"$",
    };

    kind: Kind = .unknown,
    range: Range = .{},
    kw: ?Keyword = null,
};

pub const Source = struct {
    path: []const u8,
    content: []const u8,

    pub fn str(self: *Source, range: Range) []const u8 {
        return self.content[range.start.column..range.end.column];
    }
};

const Lexer = @This();

source: Source,
current_position: Position = .{ .line = 1, .column = 0 },
current_index: u32 = 0,
current_token: Token = .{},

work_token: Token = .{},

pub fn init(source: Source) Lexer {
    return Lexer{
        .source = source,
    };
}

pub fn next(self: *Lexer) ?Token {
    if (self.current_token.kind == .eof) {
        return null;
    }

    const c = self.peek();
    if (c == 0) {
        self.work_token.kind = .eof;
    } else if (std.ascii.isAlphabetic(c)) {
        self.identifier();
        // check if its a keyword
        if (std.meta.stringToEnum(Keyword, self.str(self.work_token.range))) |k| {
            self.work_token.kw = k;
            self.work_token.kind = .keyword;
        }
    } else if (std.ascii.isDigit(c)) {
        // could be integer or float
        // self.literal();
        self.number();
    } else {
        self.symbol();
    }

    self.current_token = self.work_token;
    defer self.work_token = .{};
    return if (self.current_token.kind == .eof) null else self.current_token;
}

pub fn str(self: *Lexer, range: Range) []const u8 {
    return self.source.str(range);
}

fn identifier(self: *Lexer) void {
    const start = self.current_position;
    while (true) {
        const c = self.peek();
        if (c == 0) break;
        if (!std.ascii.isAlphanumeric(c) and c != '_') {
            break;
        }
        _ = self.eat();
    }
    const end = self.current_position;
    self.work_token.kind = .identifier;
    self.work_token.range = .{ .start = start, .end = end };
}

fn symbol(self: *Lexer) void {
    const start = self.current_position;
    switch (self.eat()) {
        '=' => {
            if (self.peek() == '=') {
                _ = self.eat();
                self.work_token.kind = .@"==";
            } else {
                self.work_token.kind = .@"=";
            }
        },
        '+' => {
            if (self.peek() == '=') {
                _ = self.eat();
                self.work_token.kind = .@"+=";
            } else {
                self.work_token.kind = .@"+";
            }
        },
        '-' => {
            switch (self.peek()) {
                '=' => {
                    _ = self.eat();
                    self.work_token.kind = .@"-=";
                },
                '>' => {
                    _ = self.eat();
                    self.work_token.kind = .@"->";
                },
                else => {
                    self.work_token.kind = .@"-";
                },
            }
        },
        '*' => {
            if (self.peek() == '=') {
                _ = self.eat();
                self.work_token.kind = .@"*=";
            } else {
                self.work_token.kind = .@"*";
            }
        },
        '/' => {
            if (self.peek() == '=') {
                _ = self.eat();
                self.work_token.kind = .@"/=";
            } else {
                self.work_token.kind = .@"/";
            }
        },
        '%' => {
            if (self.peek() == '=') {
                _ = self.eat();
                self.work_token.kind = .@"%=";
            } else {
                self.work_token.kind = .@"%";
            }
        },
        '<' => {
            if (self.peek() == '=') {
                _ = self.eat();
                self.work_token.kind = .@"<=";
            } else if (self.peek() == '<') {
                _ = self.eat();
                if (self.peek() == '=') {
                    _ = self.eat();
                    self.work_token.kind = .@"<<=";
                } else {
                    self.work_token.kind = .@"<<";
                }
            } else {
                self.work_token.kind = .@"<";
            }
        },
        '>' => {
            if (self.peek() == '=') {
                _ = self.eat();
                self.work_token.kind = .@">=";
            } else if (self.peek() == '>') {
                _ = self.eat();
                if (self.peek() == '=') {
                    _ = self.eat();
                    self.work_token.kind = .@">>=";
                } else {
                    self.work_token.kind = .@">>";
                }
            } else {
                self.work_token.kind = .@">";
            }
        },
        '&' => {
            if (self.peek() == '=') {
                _ = self.eat();
                self.work_token.kind = .@"&=";
            } else if (self.peek() == '&') {
                _ = self.eat();
                self.work_token.kind = .@"&&";
            } else {
                self.work_token.kind = .@"&";
            }
        },
        '|' => {
            if (self.peek() == '=') {
                _ = self.eat();
                self.work_token.kind = .@"|=";
            } else if (self.peek() == '|') {
                _ = self.eat();
                self.work_token.kind = .@"||";
            } else {
                self.work_token.kind = .@"|";
            }
        },
        '^' => {
            if (self.peek() == '=') {
                _ = self.eat();
                self.work_token.kind = .@"^=";
            } else {
                self.work_token.kind = .@"^";
            }
        },
        '!' => {
            if (self.peek() == '=') {
                _ = self.eat();
                self.work_token.kind = .@"!=";
            } else {
                self.work_token.kind = .@"!";
            }
        },
        '(' => self.work_token.kind = .@"(",
        ')' => self.work_token.kind = .@")",
        '{' => self.work_token.kind = .@"{",
        '}' => self.work_token.kind = .@"}",
        '[' => self.work_token.kind = .@"[",
        ']' => self.work_token.kind = .@"]",
        ',' => self.work_token.kind = .@",",
        ';' => self.work_token.kind = .@";",
        ':' => self.work_token.kind = .@":",
        '.' => {
            if (self.peek() == '.') {
                _ = self.eat();
                if (self.peek() == '.') {
                    _ = self.eat();
                    self.work_token.kind = .@"...";
                } else {
                    self.work_token.kind = .@"..";
                }
            } else {
                self.work_token.kind = .@".";
            }
        },
        '?' => self.work_token.kind = .@"?",
        '@' => self.work_token.kind = .@"@",
        '$' => self.work_token.kind = .@"$",
        else => |unhandled| std.debug.panic("unhandled: {}", .{unhandled}),
    }
    const end = self.current_position;
    self.work_token.range = .{ .start = start, .end = end };
}

fn number(self: *Lexer) void {
    const start = self.current_position;
    var is_float = false;
    while (true) {
        const c = self.peek();
        if (c == 0) break;
        if (!std.ascii.isDigit(c) and c != '.') break;
        if (c == '.') {
            is_float = true;
        }
        _ = self.eat();
    }
    const end = self.current_position;
    self.work_token.kind = if (is_float) .float else .integer;
    self.work_token.range = .{ .start = start, .end = end };
}

pub fn whitespace(self: *Lexer) void {
    // peek and eat whitespace
    while (true) {
        const c = self.peek();
        if (c == 0) {
            // we have eof
            self.work_token.kind = .eof;
            break;
        }
        if (std.ascii.isWhitespace(c)) {
            _ = self.eat();
        } else {
            break;
        }
    }
}

fn eat(self: *Lexer) u8 {
    if (self.current_index >= self.source.content.len) {
        self.work_token.kind = .eof;
        return 0;
    }

    const c = self.source.content[self.current_index];
    self.current_index += 1;

    if (c == '\n') {
        self.current_position.line += 1;
        self.current_position.column = 1;
    } else {
        self.current_position.column += 1;
    }

    return c;
}

fn peek(self: *Lexer) u8 {
    if (self.current_index >= self.source.content.len) return 0;
    return self.source.content[self.current_index];
}
