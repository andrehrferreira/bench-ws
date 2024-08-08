const std = @import("std");
const websocket = @import("./websocket.zig/src//websocket.zig");

const Allocator = std.mem.Allocator;
const Conn = websocket.Conn;
const Message = websocket.Message;
const Handshake = websocket.Handshake;

const CLIENTS_TO_WAIT_FOR: usize = 32;
var clients: std.ArrayList(*Conn) = undefined;
var client_count: u64 = 0;

const Context = struct {
    allocator: *Allocator,
};

fn generate_client_id() u64 {
    return @atomicRmw(u64, &client_count, 1, 0, .SeqCst);
}

fn handle_client(self: *Handler, message: Message) !void {
    const data = message.data;
    switch (message.type) {
        .binary => try self.conn.writeBin(data),
        .text => {
            if (std.unicode.utf8ValidateSlice(data)) {
                try self.conn.writeText(data);
            } else {
                self.conn.close();
            }
        },
        else => unreachable,
    }
}

fn find_client_index(conn: *Conn) !usize {
    var i: usize = 0;
    while (i < clients.items.len) {
        if (clients.items[i] == conn) {
            return i;
        }
        i += 1;
    }
    return error.ClientNotFound;
}

pub fn main() !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = general_purpose_allocator.deinit();
    const allocator = &general_purpose_allocator.allocator;

    var context = Context{ .allocator = allocator };
    clients = std.ArrayList(*Conn).init(allocator);

    const config = websocket.Config.Server{
        .port = 3008,
        .address = "127.0.0.1",
        .handshake_timeout_ms = 3000,
        .handshake_pool_count = 10,
        .handshake_max_size = 1024,
        .buffer_size = 8192,
        .max_size = 20_000_000,
    };

    std.debug.print("Waiting for {} clients to connect..\n", .{CLIENTS_TO_WAIT_FOR});

    // Start websocket listening on the given port,
    // specifying the handler struct that will serve
    try websocket.listen(Handler, allocator, &context, config);
}

const Handler = struct {
    conn: *Conn,
    context: *Context,

    pub fn init(_: Handshake, conn: *Conn, context: *Context) !Handler {
        try clients.append(conn);
        const client_id = generate_client_id();
        const name = try std.fmt.format(context.allocator, "Client{}", .{client_id});
        std.debug.print("{} connected ({} remain)\n", .{ name, CLIENTS_TO_WAIT_FOR - clients.items.len });
        return Handler{
            .conn = conn,
            .context = context,
        };
    }

    pub fn handle(self: *Handler, message: Message) !void {
        const data = message.data;
        switch (message.type) {
            .binary => try self.conn.writeBin(data),
            .text => {
                if (std.unicode.utf8ValidateSlice(data)) {
                    try self.conn.writeText(data);
                } else {
                    self.conn.close();
                }
            },
            else => unreachable,
        }

        // Broadcast message to all clients
        for (clients.items) |client| {
            if (client != self.conn) {
                try client.sendMessage(message);
            }
        }
    }

    pub fn close(self: *Handler) void {
        const index = find_client_index(self.conn) catch |err| {
            std.debug.print("Error finding client index: {}\n", .{err});
            return;
        };
        clients.swap_remove(index);
        std.debug.print("Client disconnected\n", .{});
    }
};
