package info.kgeorgiy.ja.korshunov.hello;

import info.kgeorgiy.java.advanced.hello.NewHelloServer;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.charset.StandardCharsets;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

// :NOTE: javadocs
public class HelloUDPNonblockingServer extends AbstractServer {
    private Selector selector;
    private Thread myThread;
    private final Map<Integer, DatagramChannel> mapChannels = new ConcurrentHashMap<>();

    public void start(int threads, Map<Integer, String> ports) {
        try {
            selector = Selector.open();

            for (Integer key : ports.keySet()) {
                DatagramChannel channel = DatagramChannel.open();
                channel.configureBlocking(false);
                InetSocketAddress address = new InetSocketAddress(key);
                channel.bind(address);
                channel.register(selector, SelectionKey.OP_WRITE | SelectionKey.OP_READ, new HandlerServer(ports.get(key), address));
                mapChannels.put(key, channel);
            }

            Runnable run = ( () -> {
                while (!Thread.currentThread().isInterrupted()) {
                    try {
                        if (selector.isOpen()) {
                            selector.select();
                            Set<SelectionKey> selectedKeys = selector.selectedKeys();
                            Iterator<SelectionKey> iterator = selectedKeys.iterator();

                            while (iterator.hasNext()) {
                                SelectionKey key = iterator.next();
                                iterator.remove();
                                if (key.isValid() && key.isReadable()) {
                                    DatagramChannel channel = (DatagramChannel) key.channel();
                                    HandlerServer handler = (HandlerServer) key.attachment();
                                    handleRead(channel, handler);
                                }
                            }
                        }
                    } catch (IOException e) {
                        System.err.println("Error in selection loop: " + e.getMessage());
                        break;
                    }
                }
            });

            myThread = new Thread(run);
            myThread.start();

        } catch (IOException e) {
            System.err.println("Error during open selector: " + e.getMessage());
        }
    }

    private void handleRead(DatagramChannel channel, HandlerServer handler) {
        try {
            ByteBuffer buffer = ByteBuffer.allocate(1024);
            InetSocketAddress outAdress = (InetSocketAddress) channel.receive(buffer);
            buffer.flip();
            String request = StandardCharsets.UTF_8.decode(buffer).toString();
            String out = handler.getResponse().replace("$", request);
            ByteBuffer outBuffer = ByteBuffer.wrap(out.getBytes(StandardCharsets.UTF_8));
            channel.send(outBuffer, outAdress);
        } catch (IOException e) {
            System.err.println("Error in read: " + e.getMessage());
        }
    }

    public void close() {
        myThread.interrupt();
        try {
            myThread.join();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }

        try {
            selector.close();
        } catch (IOException e) {
            System.err.println("Error in closing selector: " + e.getMessage());
        }

        // :NOTE: via stream
        for (Integer key : mapChannels.keySet()) {
            try {
                mapChannels.get(key).close();
            } catch (IOException e) {
                System.err.println("Error in closing channel: " + e.getMessage());
            }
        }

    }

    private static class HandlerServer {
        private final String response;
        private final InetSocketAddress address;

        public HandlerServer(String response, InetSocketAddress address) {
            this.response = response;
            this.address = address;
        }

        public String getResponse() {
            return this.response;
        }

        public InetSocketAddress getAddress() {
            return this.address;
        }
    }

}
