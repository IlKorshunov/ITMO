package info.kgeorgiy.ja.korshunov.UDPProxy;

import java.io.IOException;
import java.net.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicBoolean;

public class Proxy {
    private final Map<Integer, List<SocketAddress>> in2out;
    final ExecutorService listeners;
    final ExecutorService workers;
    private final AtomicBoolean isRunning = new AtomicBoolean(true);
    private final List<DatagramSocket> sockets = new ArrayList<>();

    public Proxy(int threads, Map<Integer, List<SocketAddress>> map) {
        listeners = Executors.newFixedThreadPool((int) Math.ceil((double) threads / 2));
        workers = Executors.newFixedThreadPool((int) Math.ceil((double) threads / 2));
        in2out = map;
    }

    void close() {
        isRunning.set(false);
        sockets.forEach(DatagramSocket::close);
        listeners.shutdownNow();
        workers.shutdownNow();
        shutdownAndAwaitTermination(listeners, "Listeners pool");
        shutdownAndAwaitTermination(workers, "Workers pool");
    }

    private void shutdownAndAwaitTermination(ExecutorService pool, String poolName) {
        try {
            if (!pool.awaitTermination(3, java.util.concurrent.TimeUnit.SECONDS)) {
                pool.shutdownNow();
                if (!pool.awaitTermination(3, java.util.concurrent.TimeUnit.SECONDS)) {
                    throw new RuntimeException(poolName + " did not terminate");
                }
            }
        } catch (InterruptedException ie) {
            pool.shutdownNow();
            Thread.currentThread().interrupt();
        }
    }

    void run() {
        for (Integer inPort : in2out.keySet()) {
            listeners.submit(() -> {
                try (DatagramSocket requestSocket = new DatagramSocket(inPort)) {
                    sockets.add(requestSocket);
                    int size = requestSocket.getReceiveBufferSize();
                    byte[] response = new byte[size];
                    while (isRunning.get() && !requestSocket.isClosed() && !Thread.currentThread().isInterrupted()) {
                        DatagramPacket requestPacket = new DatagramPacket(response, response.length);
                        requestSocket.receive(requestPacket);
                        workers.submit(() -> {
                            redirect(requestPacket, inPort);
                        });
                    }
                } catch (IOException e) {
                    if (isRunning.get()) {
                        System.err.println("Error receiving packet on port " + inPort + ": " + e.getMessage());
                    }
                }
            });
        }
    }

    private void redirect(DatagramPacket requestPacket, int inPort) {
        List<SocketAddress> listToRedirect = in2out.get(inPort);
        for (SocketAddress address : listToRedirect) {
            try (DatagramSocket responseSocket = new DatagramSocket()) {
                DatagramPacket responsePacket = new DatagramPacket(requestPacket.getData(), requestPacket.getLength(), address);
                responseSocket.send(responsePacket);
            } catch (IOException e) {
                System.err.println("Failed to send packet: " + e.getMessage());
            }
        }
    }

    public static void main(String[] args) throws UnknownHostException {
        if (args.length % 3 != 0) {
            throw new IllegalArgumentException("Usage: java Proxy <localPort> <remoteIp> <remotePort>");
        }

        Map<Integer, List<SocketAddress>> map = new ConcurrentHashMap<>();
        for (int i  = 0; i < args.length; i += 3) {
            int localPort = Integer.parseInt(args[i]);
            InetAddress ip = InetAddress.getByName(args[i + 1]);
            int remotePort = Integer.parseInt(args[i + 2]);
            map.computeIfAbsent(localPort, k -> new ArrayList<>()).add(new InetSocketAddress(ip, remotePort));
        }

        Proxy proxy = new Proxy(4, map);
        proxy.run();
    }
}
