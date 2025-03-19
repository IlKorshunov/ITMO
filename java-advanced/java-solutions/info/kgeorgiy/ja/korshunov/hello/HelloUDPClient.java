package info.kgeorgiy.ja.korshunov.hello;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class HelloUDPClient extends AbstractClient {

    private void sendAndRequest(String prefix, int thread, int request, DatagramSocket threadSocket, InetSocketAddress address) throws IOException {
        String requestStr = createRequest(prefix, thread, request);
        String expect = createExpectedResponse(prefix, thread, request);

        byte[] forSend = requestStr.getBytes(StandardCharsets.UTF_8);
        DatagramPacket toSend = new DatagramPacket(forSend, forSend.length, address);

        CharsetDecoder decoder = getDecoder();
        int size = threadSocket.getReceiveBufferSize();
        ByteBuffer byteBuffer = allocateBuffer(size);

        byte[] forRequest = new byte[size];
        DatagramPacket toReceive = new DatagramPacket(forRequest, forRequest.length);

        do {
            try {
                System.out.println(requestStr);
                threadSocket.send(toSend);
                threadSocket.receive(toReceive);

                byteBuffer.clear();
                byteBuffer.put(toReceive.getData(), 0, toReceive.getLength());
                byteBuffer.flip();

                String response = decoder.decode(byteBuffer).toString();

                if (response.equals(expect)) {
                    break;
                }
            } catch (IOException e) {
                System.err.println("Error in thread " + thread + ": " + e.getMessage());
            }
        } while (true);
    }

    private Runnable createRunnable(String prefix, int perThread, int finalCurThread, InetSocketAddress address) {
        return () -> {
            try (DatagramSocket threadSocket = new DatagramSocket()) {
                threadSocket.setSoTimeout(500);
                for (int curNumber = 1; curNumber <= perThread; curNumber++) {
                    sendAndRequest(prefix, finalCurThread, curNumber, threadSocket, address);
                }
            } catch (IOException e) {
                System.err.println("Error in thread " + finalCurThread + ": " + e.getMessage());
            }
        };
    }

    public void run(String name, int port, String prefix, int threads, int perThread) {
        ExecutorService poolThreads = Executors.newFixedThreadPool(threads);
        InetSocketAddress address = createSocketAddress(name, port);

        for (int curThread = 1; curThread <= threads; curThread++) {
            poolThreads.execute(createRunnable(prefix, perThread, curThread, address));
        }

        poolThreads.shutdown();

        try {
            if (!poolThreads.awaitTermination(1, TimeUnit.MINUTES)) {
                System.err.println("Pool did not terminate :(");
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            System.err.println(e.getMessage());
        }
    }
}
