const std = @import("std");
const net = std.net;

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    // Configuração do endereço IPv4 e opções do servidor
    var address = try net.Address.parseIp4(allocator, "127.0.0.1", 3008);
    defer address.deinit();

    var listener = try net.StreamServer.init(allocator, address, 100, .{});
    defer listener.deinit();

    std.log.info("Listening on 127.0.0.1:3008", .{});

    while (true) {
        // Aceita uma nova conexão
        var conn = try listener.accept();
        defer conn.deinit();

        std.log.info("New connection!", .{});

        var buffer: [256]u8 = undefined;
        while (true) {
            // Lê dados do cliente
            const bytes_read = conn.read(&buffer) catch |err| {
                std.log.warn("Read error: {s}", .{err});
                break;
            };
            if (bytes_read == 0) break; // Conexão fechada

            std.log.info("Received message: {s}", .{std.mem.sliceToString(buffer[0..bytes_read])});

            // Echo a mensagem de volta ao cliente
            _ = try conn.write(buffer[0..bytes_read]);
        }
    }
}
