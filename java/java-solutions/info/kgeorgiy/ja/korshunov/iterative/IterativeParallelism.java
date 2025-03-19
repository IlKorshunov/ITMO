package info.kgeorgiy.ja.korshunov.iterative;

import info.kgeorgiy.java.advanced.iterative.NewScalarIP;
import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.IntStream;

/**
 * Класс {@code IterativeParallelism} предоставляет методы для выполнения параллельных операций над коллекциями.
 * Эти операции включают поиск максимального или минимального элемента, проверку соответствия всех или любых элементов заданному предикату,
 * а также подсчет элементов, удовлетворяющих предикату. Класс поддерживает выполнение операций с использованием указанного
 * количества потоков.
 * Операции могут быть дополнительно настроены с помощью параметра шага, который позволяет обрабатывать элементы через заданные интервалы,
 * повышая гибкость в обработке входной коллекции.
 * Данный класс использует экземпляр {@link ParallelMapper} для управления распределением и выполнением потоков,
 * предлагая более эффективный способ обработки параллельных задач за счет повторного использования уже созданных потоков.
 *
 * @see ParallelMapperImpl
 */
public class IterativeParallelism implements NewScalarIP {
    private ParallelMapper parallelMapper;

    public IterativeParallelism() {

    }

    public IterativeParallelism(ParallelMapper parallelMapper) {
        this.parallelMapper = parallelMapper;
    }

    /**
     * Класс представляет собой поток для выполнения задачи над подсписком.
     * Этот поток принимает список элементов и функцию, которая применяется к этому списку.
     * После выполнения задачи результат можно получить с помощью метода {@code getResult()}.
     *
     * @param <T> Тип элементов списка, над которым выполняется задача.
     * @param <R> Тип результата выполнения функции над списком.
     */
    private static class TaskThread<T, R> extends Thread {

        private final List<? extends T> list;
        private final Function<List<? extends T>, ? extends R> function;
        private R result;

        private TaskThread(List<? extends T> list, Function<List<? extends T>, ? extends R> function) {
            this.list = list;
            this.function = function;
        }

        public void run() {

            result = function.apply(list);
        }

        public R getResult() {
            return result;
        }
    }

    /**
     * Выполняет параллельную обработку списка с использованием заданной функции.
     * Этот метод разделяет исходный список на подсписки, фильтрует их в соответствии с заданным шагом,
     * а затем применяет заданную функцию к каждой подсписке параллельно, используя заданное количество потоков.
     * Результаты функции собираются в список, который возвращается в конце выполнения.
     *
     * @param threads  Количество потоков, которые должны быть использованы для параллельной обработки.
     *                 Если исходный список меньше, чем количество потоков, будет использовано меньшее число потоков.
     * @param list     Исходный список элементов для обработки.
     * @param function Функция, применяемая к каждой подсписку элементов. Должна возвращать результат типа R.
     * @param step     Шаг, с которым выбираются элементы из исходного списка для формирования подсписков.
     *                 Например, шаг 2 означает, что в подсписки попадет каждый второй элемент исходного списка.
     * @param <T>      Тип элементов исходного списка.
     * @param <R>      Тип результатов, возвращаемых функцией.
     * @return Список результатов, возвращаемых функцией, примененной к каждой подсписку.
     * @throws InterruptedException если выполнение потока было прервано.
     */
    private <T, R> List<R> processInParallel(int threads, List<? extends T> list, Function<List<? extends T>, R> function, int step) throws InterruptedException {
        List<? extends T> afterStep = IntStream
                .iterate(0, i -> i < list.size(), i -> i + step)
                .mapToObj(list::get).toList();

        int len = afterStep.size();
        threads = Math.min(threads, len);
        int perThread = len / threads;
        int remain = len % threads;

        List<List<? extends T>> allLists = new ArrayList<>();
        int end = 0;
        for (int i = 0; i < threads; i++) {
            int start = end;
            end = start + perThread + (remain-- > 0 ? 1 : 0);
            List<? extends T> subList = afterStep.subList(start, end);
            allLists.add(subList);
        }

        List<R> results;
        if (parallelMapper != null) {
            results = parallelMapper.map(function, allLists);
        } else {
            List<TaskThread<T, R>> taskThreads = new ArrayList<>();
            for (List<? extends T> subList : allLists) {
                TaskThread<T, R> task = new TaskThread<>(subList, function);
                taskThreads.add(task);
                task.start();
            }
            results = new ArrayList<>();
            for (TaskThread<T, R> task : taskThreads) {
                task.join();
                results.add(task.getResult());
            }
        }

        return results;
    }




    /**
     * Возвращает максимальный элемент из списка с использованием заданного компаратора.
     * Обработка элементов списка выполняется параллельно с заданным числом потоков.
     *
     * @param threads    Количество потоков для параллельной обработки.
     * @param values     Список значений для поиска максимального элемента.
     * @param comparator Компаратор для сравнения элементов.
     * @param step       Шаг, с которым элементы передаются в функцию обработки.
     * @return Максимальный элемент из списка согласно компаратору.
     * @throws InterruptedException Если выполнение было прервано.
     */
    private <T> T findMaximum(int threads, List<? extends T> values, Comparator<? super T> comparator, int step) throws InterruptedException {
        Function<List<? extends T>, T> maxFunction = sublist -> sublist.stream().max(comparator).orElse(null);
        List<T> maxResults = processInParallel(threads, values, maxFunction, step);
        return maxResults.stream().max(comparator).orElse(null);
    }

    /**
     * Проверяет, удовлетворяют ли все элементы списка заданному предикату.
     * Обработка элементов списка выполняется параллельно с заданным числом потоков.
     *
     * @param threads   Количество потоков для параллельной обработки.
     * @param values    Список значений для проверки.
     * @param predicate Предикат для проверки элементов.
     * @param step      Шаг, с которым элементы передаются в функцию обработки.
     * @return true, если все элементы удовлетворяют предикату, иначе false.
     * @throws InterruptedException Если выполнение было прервано.
     */
    private <T> boolean findAll(int threads, List<? extends T> values, Predicate<? super T> predicate, int step) throws InterruptedException {
        Function<List<? extends T>, Boolean> allMatchFunction = sublist -> sublist.stream().allMatch(predicate);
        List<Boolean> results = processInParallel(threads, values, allMatchFunction, step);
        return results.stream().allMatch(Boolean::booleanValue);
    }

    /**
     * Проверяет, удовлетворяет ли хотя бы один элемент списка заданному предикату.
     * Обработка элементов списка выполняется параллельно с заданным числом потоков.
     *
     * @param threads   Количество потоков для параллельной обработки.
     * @param values    Список значений для проверки.
     * @param predicate Предикат для проверки элементов.
     * @param step      Шаг, с которым элементы передаются в функцию обработки.
     * @return true, если хотя бы один элемент удовлетворяет предикату, иначе false.
     * @throws InterruptedException Если выполнение было прервано.
     */
    private <T> boolean findAny(int threads, List<? extends T> values, Predicate<? super T> predicate, int step) throws InterruptedException {
        Function<List<? extends T>, Boolean> allMatchFunction = sublist -> sublist.stream().anyMatch(predicate);
        List<Boolean> results = processInParallel(threads, values, allMatchFunction, step);
        return results.stream().anyMatch(b -> b);
    }

    /**
     * Считает количество элементов списка, удовлетворяющих заданному предикату.
     * Обработка элементов списка выполняется параллельно с заданным числом потоков.
     *
     * @param threads   Количество потоков для параллельной обработки.
     * @param values    Список значений для подсчета.
     * @param predicate Предикат для проверки элементов.
     * @param step      Шаг, с которым элементы передаются в функцию обработки.
     * @return Количество элементов, удовлетворяющих предикату.
     * @throws InterruptedException Если выполнение было прервано.
     */
    private <T> int findCount(int threads, List<? extends T> values, Predicate<? super T> predicate, int step) throws InterruptedException {
        Function<List<? extends T>, Long> countFunction = sublist -> sublist.stream().filter(predicate).count();
        List<Long> results = processInParallel(threads, values, countFunction, step);
        return results.stream().mapToInt(Long::intValue).sum();
    }

    @Override
    public <T> T maximum(int threads, List<? extends T> values, Comparator<? super T> comparator, int step) throws InterruptedException {
        return findMaximum(threads, values, comparator, step);
    }

    public <T> T maximum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException {
        return findMaximum(threads, values, comparator, 1);
    }

    /**
     * Возвращает минимальный элемент из списка с использованием заданного компаратора.
     * Обработка элементов списка выполняется параллельно с заданным числом потоков.
     *
     * @param threads    Количество потоков для параллельной обработки.
     * @param values     Список значений для поиска минимального элемента.
     * @param comparator Компаратор для сравнения элементов.
     * @param step       Шаг, с которым элементы передаются в функцию обработки.
     * @return Минимальный элемент из списка согласно компаратору.
     * @throws InterruptedException Если выполнение было прервано.
     */
    @Override
    public <T> T minimum(int threads, List<? extends T> values, Comparator<? super T> comparator, int step) throws InterruptedException {
        return maximum(threads, values, comparator.reversed(), step);
    }

    @Override
    public <T> T minimum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException {
        return maximum(threads, values, comparator.reversed());
    }

    @Override
    public <T> boolean all(int threads, List<? extends T> values, Predicate<? super T> predicate, int step) throws InterruptedException {
        return findAll(threads, values, predicate, step);
    }

    @Override
    public <T> boolean all(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return findAll(threads, values, predicate, 1);
    }

    @Override
    public <T> boolean any(int threads, List<? extends T> values, Predicate<? super T> predicate, int step) throws InterruptedException {
        return findAny(threads, values, predicate, step);
    }

    @Override
    public <T> boolean any(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return findAny(threads, values, predicate, 1);
    }

    @Override
    public <T> int count(int threads, List<? extends T> values, Predicate<? super T> predicate, int step) throws InterruptedException {
        return findCount(threads, values, predicate, step);
    }

    @Override
    public <T> int count(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return findCount(threads, values, predicate, 1);
    }
}
