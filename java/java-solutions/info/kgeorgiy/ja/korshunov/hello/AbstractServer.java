package info.kgeorgiy.ja.korshunov.hello;

import info.kgeorgiy.java.advanced.hello.NewHelloServer;

import java.io.IOException;
import java.net.DatagramSocket;
import java.nio.channels.DatagramChannel;
import java.nio.channels.Selector;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public abstract class AbstractServer implements NewHelloServer {

    public abstract void start(int threads, Map<Integer, String> ports);

    public abstract void close();
}
