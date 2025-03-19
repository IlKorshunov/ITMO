package info.kgeorgiy.ja.korshunov.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;
import info.kgeorgiy.java.advanced.hello.NewHelloServer;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.concurrent.*;

public class HelloUDPServer extends AbstractServer {
    private ExecutorService listen;
    private ExecutorService worker;
    private final Map<Integer, DatagramSocket> sockets = new ConcurrentHashMap<>();

    private void requestAndSend(DatagramSocket socket, DatagramPacket responsePacket, String format) {
        while (!socket.isClosed() && !Thread.currentThread().isInterrupted()) {
            try {
                byte[] buffer = new byte[socket.getReceiveBufferSize()];
                DatagramPacket packet = new DatagramPacket(buffer, buffer.length);
                socket.receive(packet);
                Runnable run = () -> {
                    try {
                        String request = new String(packet.getData(), packet.getOffset(), packet.getLength(), StandardCharsets.UTF_8);
                        String responseText = format.replace("$", request);
                        byte[] responseData = responseText.getBytes(StandardCharsets.UTF_8);
                        packet.setData(responseData);
                        socket.send(packet);
                    } catch (IOException e) {
                        System.err.println("Failed to send or receive packet: " + e.getMessage());
                    }
                };
                worker.execute(run);
            } catch (IOException e) {
                if (!socket.isClosed()) {
                    System.err.println("Error receiving packet: " + e.getMessage());
                }
            }
        }
    }

    private Runnable createRun(DatagramSocket socket, int size, String format) {
        return () -> {
            byte[] repsonce = new byte[size];
            DatagramPacket responsePacket = new DatagramPacket(repsonce, repsonce.length);
            requestAndSend(socket, responsePacket, format);
        };
    }

    public void start(int threads, Map<Integer, String> ports) {
        listen =  Executors.newFixedThreadPool(Math.max(1, ports.size()));
        worker = Executors.newFixedThreadPool(threads);
        for (Integer key : ports.keySet()) {
            try {
                DatagramSocket socket = new DatagramSocket(key);
                sockets.put(key, socket);
                int size = socket.getReceiveBufferSize();
                listen.execute(createRun(socket, size, ports.get(key)));
            } catch (SocketException e) {
                System.err.println("Error creating socket on port " + e.getMessage());
            }
        }

    }


    public void close() {
        sockets.values().forEach(DatagramSocket::close);
        listen.shutdown();
        worker.shutdown();
        try {
            if (!listen.awaitTermination(1, TimeUnit.SECONDS) ||
                    !worker.awaitTermination(1, TimeUnit.SECONDS)) {
                System.err.println("Executors did not terminate.");
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

    public static void main(String[] args) {
        if (args.length != 2) {
            System.err.println("error in args len");
            return;
        }
        int port = Integer.parseInt(args[0]);
        int threads = Integer.parseInt(args[1]);
        try (HelloUDPServer server = new HelloUDPServer()) {
            server.start(port, threads);
        }
    }
}
