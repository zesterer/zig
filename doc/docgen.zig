const std = @import("std");
const io = std.io;
const os = std.os;
const mem = std.mem;

pub fn main() -> %void {
    // TODO use a more general purpose allocator here
    var inc_allocator = %%std.heap.IncrementingAllocator.init(5 * 1024 * 1024);
    defer inc_allocator.deinit();
    const allocator = &inc_allocator.allocator;

    var args_it = os.args();

    if (!args_it.skip()) @panic("expected self arg");

    const in_file_name = %%(args_it.next(allocator) ?? @panic("expected input arg"));
    defer allocator.free(in_file_name);

    const out_file_name = %%(args_it.next(allocator) ?? @panic("expected output arg"));
    defer allocator.free(out_file_name);

    var in_file = %%io.File.openRead(in_file_name, allocator);
    defer in_file.close();

    var out_file = %%io.File.openWrite(out_file_name, allocator);
    defer out_file.close();

    var file_in_stream = io.FileInStream.init(&in_file);
    var buffered_in_stream = io.BufferedInStream.init(&file_in_stream.stream);

    var file_out_stream = io.FileOutStream.init(&out_file);
    var buffered_out_stream = io.BufferedOutStream.init(&file_out_stream.stream);

    gen(allocator, &buffered_in_stream.stream, &buffered_out_stream.stream);
    %%buffered_out_stream.flush();

}

const Context = struct {
    line: usize,
    column: usize,
};

fn gen(allocator: &mem.Allocator, in: &io.InStream, out: &const io.OutStream) {
    var toc_buffer = std.Buffer.init(allocator);
    defer toc_buffer.deinit();

    var buf_out_stream = io.BufferOutStream.init(&toc_buffer);
    const toc_stream = &buf_out_stream.stream;
    %%toc_stream.print("<ul>\n");

    var state = State.Start;
    var context = Context {
        .line = 0,
        .column = 0,
    };
    %%toc_stream.print("</ul>\n");
}

const TokenId = enum {
    Content,
    Tag,
};

const Token = struct {
    id: TokenId,
    line: usize,
    column: usize,
    start: usize,
    end: usize,
};

const TokenizeContext = struct {
    allocator: &mem.Allocator,
    file_name: []const u8,
    tokens: std.ArrayList(Token),
    line: usize,
    column: usize,
    pos: usize,
};

const State = enum {
    Start,
    LessThan,
    BeginTagName,
    TagName,
    TagArgBeginQuote,
};

fn tokenize(context: &TokenizeContext, in: &io.InStream) {
    var state = State.Start;
    beginToken(context, TokenId.Content);
    while (true) {
        const byte = in.readByte() %% |err| {
            if (err == error.EndOfStream) {
                break;
            }
            std.debug.panic("{}", err)
        };
        switch (state) {
            State.Start => switch (byte) {
                '<' => {
                    endToken(context);
                    beginToken(context, TokenId.Tag);
                    state = State.LessThan;
                },
                else => {},
            },
            State.LessThan => switch (byte) {
                '%' => {
                    state = State.BeginTagName;
                },
                else => {
                    cancelToken(context);
                    state = State.Start;
                },
            },
            State.BeginTagName => switch (byte) {
                'a'...'z' => {
                    state = State.TagName,
                },
                else => {
                    reportError(context, "unrecognized tag character: '{}'", byte);
                },
            },
            State.TagName => switch (byte) {
                'a'...'z' => {},
                ':' => {
                    state = State.TagArgBeginQuote,
                },
                '>' => {
                    endToken(context);
                    state = State.Start;
                },
                else => {
                    reportError(context, "unexpected character: '{}'", byte);
                },
            },
            State.TagArgBeginQuote => switch (byte) {
                '"' => {
                    state = State.TagArg;
                },
                else => {
                    reportError(context, "expected '\"'");
                },
            },
            State.TagArg => switch (byte) {
                '"' => {
                    state = State.TagExpectEnd;
                },
                else => {},
            },
        }
        context.pos += 1;
        if (byte == '\n') {
            context.line += 1;
            context.column = 0;
        } else {
            context.column += 1;
        }
    }
}

fn reportError(context: &const Context, comptime format: []const u8, args: ...) -> noreturn {
    std.debug.panic("{}:{}:{}: " ++ format, context.file_name, context.line + 1, context.column + 1, args);
}
