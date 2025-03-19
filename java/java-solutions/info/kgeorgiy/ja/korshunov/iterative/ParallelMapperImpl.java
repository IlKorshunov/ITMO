package info.kgeorgiy.ja.korshunov.iterative;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;
import java.util.function.Function;
import java.util.stream.IntStream;

import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

/**
 * Реализация интерфейса {@link ParallelMapper}, предоставляющая возможность параллельного выполнения функций.
 * Использует пул потоков для выполнения заданий.
 */
public class ParallelMapperImpl implements ParallelMapper {

    // :TO-IMPROVE: it's infinity function
    public static void main(String[] args) throws InterruptedException {
        var pm = new ParallelMapperImpl(2);
        pm.map(i -> {
            throw new RuntimeException();
        }, List.of(1, 2, 3));
        pm.map(i -> i + 1, List.of(1, 2, 3));
    }

    private final List<Thread> threads;
    private final Deque<Tasks<?, ?>> queue;

    /**
     * Создает экземпляр {@code ParallelMapperImpl} с указанным числом потоков.
     * Инициализирует и запускает потоки, которые будут обрабатывать задачи из очереди заданий.
     *
     * @param threads количество потоков для выполнения задач.
     */
    public ParallelMapperImpl(int threads) {
        this.threads = new ArrayList<>();
        this.queue = new ArrayDeque<>();
        // :NOTE: runnable before is function!
        Runnable runnable = () -> {
            try {
                while (!Thread.interrupted()) {
                    Tasks<?, ?> task;
                    synchronized (queue) {
                        while (queue.isEmpty()) {
                            queue.wait();
                        }
                        task = queue.poll();
                    }
                    task.run();
                }
            } catch (InterruptedException ignored) {
            }
        };
        for (int i = 0; i < threads; i++) {
            Thread thread = new Thread(runnable);
            this.threads.add(thread);
            thread.start();
        }
    }
    // per host: 1
    // threads: 3
    // 1 1 1 2 2 2 3 3 3
    // actual: [1 run] [1 wait] [1 wait]
    // excepted: [1 run] [2 run] [3 run]

    public <T, R> List<R> map(
            Function<? super T, ? extends R> f, List<? extends T> items, int step
    ) throws InterruptedException {
        List<Tasks<T, R>> result = new ArrayList<>();
        for (T arg : items) {
            Tasks<T, R> task = new Tasks<>(f, arg);
            result.add(task);
            synchronized (queue) {
                queue.add(task);
                queue.notify();
            }
        }
        List<Tasks<T, R>> it = IntStream.iterate(0, i -> i < result.size(), i -> i + step)
                .mapToObj(result::get)
                .toList();

        // :NOTE: Interrupted exception wasn't throw before!
        var results = new ArrayList<R>();
        for (var task : it) {
            try {
                results.add(task.getResult());
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                throw e;
            }
        }
        return results;
    }

    /**
     * Выполняет функцию {@code f} параллельно на каждом элементе списка {@code args}.
     * Результаты выполнения функции собираются в список и возвращаются в том же порядке, что и исходный список
     * аргументов.
     *
     * @param f    функция, применяемая к каждому элементу списка {@code args}.
     * @param args список аргументов, к которым применяется функция {@code f}.
     * @return список результатов выполнения функции {@code f}.
     * @throws InterruptedException если выполнение было прервано.
     */
    @Override
    public <T, R> List<R> map(Function<? super T, ? extends R> f, List<? extends T> args) throws InterruptedException {
        return map(f, args, 1);
    }

    /**
     * Завершает работу всех потоков, используемых {@code ParallelMapperImpl}.
     * Вызов этого метода гарантирует, что все потоки будут корректно завершены.
     */
    @Override
    public void close() {
        threads.forEach(Thread::interrupt);
        for (Thread thread : threads) {
            // :TO-IMPROVE: actually didn't stop thread before
            while (true) {
                try {
                    thread.join();
                    break;
                } catch (InterruptedException ignored) {
                    Thread.currentThread().interrupt();
                }
            }
        }
    }

    /**
     * Вспомогательный класс для представления задачи, которая должна быть выполнена.
     * Задача включает в себя функцию для применения и аргумент, к которому эта функция применяется.
     * После выполнения задачи результат можно получить с помощью метода {@code getResult()}.
     *
     * @param <T> тип аргумента функции.
     * @param <R> тип результата выполнения функции.
     */
    private static class Tasks<T, R> {
        private final Function<? super T, ? extends R> function;
        private final T argument;
        // :TO-IMPROVE: result or exception
        private R result;

        /**
         * Создает новую задачу с указанной функцией и аргументом.
         *
         * @param function функция для выполнения.
         * @param argument аргумент, на котором будет выполнена функция.
         */
        public Tasks(Function<? super T, ? extends R> function, T argument) {
            this.function = function;
            this.argument = argument;
        }

        /**
         * Выполняет задачу, применяя функцию к аргументу.
         */
        public void run() {
            var tmp = function.apply(argument);
            synchronized (function) {
                result = tmp;
                function.notify();
            }
        }

        /**
         * Возвращает результат выполнения задачи.
         * Если результат еще не готов, вызывающий поток блокируется до его готовности.
         *
         * @return результат выполнения задачи.
         * @throws InterruptedException если ожидание результата было прервано.
         */
        public R getResult() throws InterruptedException {
            synchronized (function) {
                while (result == null) {
                    function.wait();
                }
            }
            return result;
        }
    }
}
