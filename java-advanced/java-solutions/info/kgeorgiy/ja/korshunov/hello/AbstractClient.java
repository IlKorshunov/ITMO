package info.kgeorgiy.ja.korshunov.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.nio.charset.CharsetDecoder;
import java.nio.ByteBuffer;

public abstract class AbstractClient implements HelloClient {
    protected InetSocketAddress createSocketAddress(String name, int port) {
        return new InetSocketAddress(name, port);
    }

    protected static String createRequest(String prefix, int thread, int request) {
        return prefix + thread + "_" + request;
    }

    protected static String createExpectedResponse(String prefix, int thread, int request) {
        return "Hello, " + createRequest(prefix, thread, request);
    }

    protected CharsetDecoder getDecoder() {
        return StandardCharsets.UTF_8.newDecoder();
    }

    protected ByteBuffer allocateBuffer(int size) {
        return ByteBuffer.allocate(size);
    }

    @Override
    public abstract void run(String name, int port, String prefix, int threads, int perThread);
}
