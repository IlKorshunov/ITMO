package info.kgeorgiy.ja.korshunov.hello;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

public class HelloUDPNonblockingClient extends AbstractClient {

    private int closeChannels = 0;

    public void run(String name, int port, String prefix, int threads, int perThread) {
        InetSocketAddress serverAddress = createSocketAddress(name, port);
        List<DatagramChannel> allChannels = new ArrayList<>();

        try (Selector selector = Selector.open()) {
            for (int i = 1; i <= Math.max(1, threads); i++) {
                DatagramChannel channel = DatagramChannel.open();
                channel.configureBlocking(false);
                channel.connect(serverAddress);
                channel.register(selector, SelectionKey.OP_WRITE | SelectionKey.OP_READ, new HandlerChannel(i, perThread, prefix, serverAddress, selector));
                allChannels.add(channel);
            }

            while (!Thread.interrupted() && closeChannels < threads) {
                selector.select(500);
                Set<SelectionKey> allKeys = selector.selectedKeys();
                if (allKeys.isEmpty()) {
                    selector.keys().forEach(key -> key.interestOps(SelectionKey.OP_WRITE));
                }

                Iterator<SelectionKey> iterator = allKeys.iterator();
                while (iterator.hasNext()) {
                    SelectionKey key = iterator.next();
                    iterator.remove();
                    if (key.isValid()) {
                        DatagramChannel channel = (DatagramChannel) key.channel();
                        HandlerChannel context = (HandlerChannel) key.attachment();
                        if (context.isFinished()) {
                            key.cancel();
                            channel.close();
                            closeChannels++;
                        } else {
                            if (key.isWritable()) {
                                handleWrite(channel, context);
                            } else if (key.isReadable()) {
                                handleRead(channel, context, key);
                            }
                        }
                    } else {
                        System.err.println("key invalid");
                    }
                }
            }
        } catch (IOException e) {
            System.err.println("Error during open selector: " + e.getMessage());
        }

        for (DatagramChannel cur : allChannels) {
            try {
                cur.close();
            } catch (IOException e) {
                System.err.println("Error closing channel: " + e.getMessage());
            }
        }
    }

    private void handleWrite(DatagramChannel channel, HandlerChannel context) {
        String request = context.getRequest();
        try {
            channel.send(ByteBuffer.wrap(request.getBytes(StandardCharsets.UTF_8)), context.serverAddress);
            channel.keyFor(context.selector).interestOps(SelectionKey.OP_READ);
        } catch (IOException e) {
            System.err.println("Error during write: " + e.getMessage());
        }
    }

    private void handleRead(DatagramChannel channel, HandlerChannel context, SelectionKey key) {
        ByteBuffer buffer = allocateBuffer(1024);
        try {
            buffer.clear();
            channel.receive(buffer);
            buffer.flip();
            String response = StandardCharsets.UTF_8.decode(buffer).toString();

            if (response.contains(context.getExpectedResponse())) {
                context.sentRequest();
            }
            key.interestOps(SelectionKey.OP_WRITE);
        } catch (IOException e) {
            System.err.println("Error during read: " + e.getMessage());
        }
    }

    private static class HandlerChannel {
        private final int curThread;
        private final int perThread;
        private int count;
        private final String prefix;
        private final InetSocketAddress serverAddress;
        private final Selector selector;

        private HandlerChannel(int curThread, int perThread, String prefix, InetSocketAddress serverAddress, Selector selector) {
            this.curThread = curThread;
            this.perThread = perThread;
            this.prefix = prefix;
            this.serverAddress = serverAddress;
            this.selector = selector;
            this.count = 1;
        }

        private String getRequest() {
            return createRequest(prefix, curThread, count);
        }

        private String getExpectedResponse() {
            return createExpectedResponse(prefix, curThread, count);
        }

        private void sentRequest() {
            count++;
        }

        private boolean isFinished() {
            return count > perThread;
        }
    }
}
