package info.kgeorgiy.ja.korshunov.UDPProxy;

import org.junit.jupiter.api.*;

import java.io.IOException;
import java.net.*;
import java.util.*;
import java.util.concurrent.*;

import static org.junit.jupiter.api.Assertions.*;

public class ProxyTest {
    private Proxy proxy;
    private Map<Integer, List<SocketAddress>> map;
    private int inputPort;
    private int remotePort;
    private String ip = "127.0.0.1";

    @BeforeEach
    public void setUp() throws UnknownHostException {
        inputPort = findFreePort();
        remotePort = findFreePort();

        map = new ConcurrentHashMap<>();
        List<SocketAddress> remoteAddresses = new ArrayList<>();
        remoteAddresses.add(new InetSocketAddress(InetAddress.getByName(ip), remotePort));
        map.put(inputPort, remoteAddresses);
        proxy = new Proxy(4, map);
        proxy.run();
    }

    @AfterEach
    public void tearDown() {
        proxy.close();
    }

    @Test
    public void testPacketRedirection() throws IOException {
        DatagramSocket sendSocket = new DatagramSocket();
        String message = "Hello, Proxy!";
        byte[] buffer = message.getBytes();
        InetSocketAddress address = new InetSocketAddress(InetAddress.getByName(ip), inputPort);

        DatagramPacket packetToSend = new DatagramPacket(buffer, buffer.length, address);
        sendSocket.send(packetToSend);

        DatagramSocket receiveSocket = new DatagramSocket(remotePort);
        receiveSocket.setSoTimeout(1000);

        byte[] receiveBuffer = new byte[1024];
        DatagramPacket receivedPacket = new DatagramPacket(receiveBuffer, receiveBuffer.length);

        receiveSocket.receive(receivedPacket);
        String receivedMessage = new String(receivedPacket.getData(), 0, receivedPacket.getLength());

        assertEquals(message, receivedMessage, "The received message should match the sent message");

        sendSocket.close();
        receiveSocket.close();
    }

    @Test
    public void testShutdown() {
        proxy.close();
        assertTrue(proxy.listeners.isShutdown(), "Listeners pool should be shut down");
        assertTrue(proxy.workers.isShutdown(), "Workers pool should be shut down");
    }

    private int findFreePort() {
        try (ServerSocket socket = new ServerSocket(0)) {
            return socket.getLocalPort();
        } catch (IOException e) {
            throw new RuntimeException("No free port found", e);
        }
    }
}
