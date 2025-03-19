package info.kgeorgiy.ja.korshunov.Bag;

import net.java.quickcheck.collection.Pair;

import java.util.*;
import java.util.function.IntFunction;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Bag<T> implements Collection<T> {

    private static class EquivClass<T> {
        private int size;
        private static long idCounter = 0;
        private final List<Pair<T, Long>> elements;

        private EquivClass(T value, int count) {
            this.size = count;
            elements = new ArrayList<>();
            for (int i = 0; i < count; i++) {
                elements.add(new Pair<>(value, idCounter++));
            }
        }

        private void Add() {
            elements.add(new Pair<>(elements.get(0).getFirst(), idCounter++));
            size++;
        }

        private void Remove() {
            if (elements.isEmpty()) {
                throw new NoSuchElementException("No elements to remove");
            }
            elements.remove(0);
            size--;
        }

        public int removeAll() {
            int removedSize = size;
            elements.clear();
            size = 0;
            return removedSize;
        }

        public int getSize() {
            return size;
        }

        public List<Pair<T, Long>> getElements() {
            return elements;
        }

    }

    private class BagIterator implements Iterator<T> {
        private final Iterator<Map.Entry<T, EquivClass<T>>> iterator;
        private Map.Entry<T, EquivClass<T>> curEntry;
        private int curIndex;


        BagIterator() {
            this.iterator = elements.entrySet().iterator();
            this.curIndex = 0;
            this.curEntry = iterator.hasNext() ? iterator.next() : null;
        }


        @Override
        public boolean hasNext() {
            return curIndex < curEntry.getValue().getElements().size() || iterator.hasNext();
        }

        @Override
        public T next() { // ?
            if (!hasNext()) {
                throw new NoSuchElementException();
            }
            if (curIndex >= curEntry.getValue().getElements().size()) {
                curEntry = iterator.next();
                curIndex = 0;
            }
            return curEntry.getValue().getElements().get(curIndex++).getFirst();
        }

        @Override
        public void remove() { // ?
            if (curIndex == 0) {
                throw new IllegalStateException();
            }
            curEntry.getValue().Remove();
            if (curEntry.getValue().getElements().isEmpty()) {
                iterator.remove();
            }
            curIndex--;
        }
    }

    private final Map<T, EquivClass<T>> elements;
    private int size;

    public Bag() {
        this.elements = new HashMap<>();
        this.size = 0;
    }

    public Bag(Collection<T> elements) {
        this();
        addAll(elements);
    }


    @Override
    public int size() {
        return size;
    }

    @Override
    public boolean isEmpty() {
        return size() == 0;
    }

    @Override
    public Iterator<T> iterator() {
        return new BagIterator();
    }

    @Override
    public boolean contains(Object o) {
        return this.elements.containsKey(o);
    }

    @Override
    public boolean add(T t) {
        elements.computeIfAbsent(t, k -> new EquivClass<>(t, 0)).Add();
        size++;
        return true;
    }


    @Override
    public boolean remove(Object o) {
        EquivClass<T> equivClass = elements.get(o);
        if (equivClass == null || equivClass.getElements().isEmpty()) {
            return false;
        }
        equivClass.Remove();
        if (equivClass.getElements().isEmpty()) {
            elements.remove(o);
        }
        this.size--;
        return true;
    }

    @Override
    public boolean containsAll(Collection<?> c) { // тут надо будет, конечно, корректность проверить
        return c.stream().map(this::contains).reduce(false, Boolean::logicalOr);
    }

    @Override
    public boolean addAll(Collection<? extends T> c) {
        return c.stream().map(this::add).reduce(false, Boolean::logicalOr);
    }

    @Override
    public boolean removeAll(Collection<?> c) {
        return c.stream().map(this::remove).reduce(false, Boolean::logicalOr);
    }


    @Override
    public void clear() {
        this.elements.clear();
        this.size = 0;
    }

    @Override
    public Spliterator<T> spliterator() {
        return Collection.super.spliterator();
    }

    @Override
    public Stream<T> stream() {
        return elements.values().stream().flatMap(equivClass -> equivClass.getElements().stream().map(Pair::getFirst));
    }
    @Override
    public Stream<T> parallelStream() {
        return elements.values().parallelStream().flatMap(equivClass -> equivClass.getElements().stream().map(Pair::getFirst));
    }

    @Override
    public Object[] toArray() {
        return stream().toArray();
    }


    @Override
    public <T1> T1[] toArray(T1[] input) {
        Object[] ar = toArray();
        System.arraycopy(ar, 0, input, 0, Math.min(input.length, ar.length)); // надо переписать
        return input;
    }

    @Override
    public boolean removeIf(Predicate<? super T> filter) {
        Objects.requireNonNull(filter);

        List<T> elementsToRemove = elements.keySet().stream()
                .filter(filter)
                .toList();

        return removeKeys(elementsToRemove);
    }

    @Override
    public boolean retainAll(Collection<?> c) {
        Objects.requireNonNull(c);

        Set<T> keysToRemove = elements.keySet().stream()
                .filter(curKey -> !c.contains(curKey))
                .collect(Collectors.toSet());

        return removeKeys(keysToRemove);
    }

    private boolean removeKeys(Collection<T> keysToRemove) {
        return keysToRemove.stream()
                .map(this::removeEquivClass)
                .reduce(false, Boolean::logicalOr);
    }

    private boolean removeEquivClass(T key) {
        EquivClass<T> equivClass = elements.get(key);
        if (equivClass != null) {
            size -= equivClass.removeAll();
            elements.remove(key);
            return true;
        }
        return false;
    }

}
