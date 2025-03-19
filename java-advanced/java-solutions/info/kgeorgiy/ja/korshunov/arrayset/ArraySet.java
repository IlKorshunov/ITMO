package info.kgeorgiy.ja.korshunov.arrayset;

import java.util.*;

public class ArraySet<E> extends AbstractSet<E> implements SortedSet<E> { // extends AbstractSet<E>
    private final Comparator<? super E> comparator;
    private final ArrayList<E> elements;

    public ArraySet() {
        this.elements = new ArrayList<>();
        this.comparator = null;
    }

    public ArraySet(Comparator<? super E> comparator) {
        this.elements = new ArrayList<>();
        this.comparator = comparator;
    }

    public ArraySet(Collection<? extends E> collection, Comparator<? super E> comparator) {
        this.comparator = comparator;
        TreeSet<E> sortedSet = new TreeSet<>(comparator);
        sortedSet.addAll(collection);
        this.elements = new ArrayList<>(sortedSet);
    }


    public ArraySet(Collection<? extends E> collection) {
        this.comparator = null;
        this.elements = new ArrayList<>(new TreeSet<>(collection));
    }


    public ArraySet(SortedSet<E> set) {
        this.comparator = set.comparator();
        this.elements = new ArrayList<>(set);
    }


    @Override
    public Iterator<E> iterator() {return elements.iterator(); }

    @Override
    public int size() {return this.elements.size(); }

    @Override
    public Comparator<? super E> comparator() {
        return this.comparator != Comparator.naturalOrder() ? this.comparator : null;
    }

    private int binarySearch(E element, boolean includeLeft, boolean includeRight) {
        int index = Collections.binarySearch(elements, element, comparator);
        if (index < 0) {
            index = -(index + 1);
        } else {
            if (!includeLeft) {
                index += 1;
            }
            if (includeRight) {
                index += 1;
            }
        }
        return index;
    }

    private SortedSet<E> subSection(int fromElement, int toElement) {
        return new ArraySet<>(elements.subList(fromElement, toElement), comparator);
    }

    @Override
    @SuppressWarnings("unchecked")
    public SortedSet<E> subSet(E fromElement, E toElement) {
        if (fromElement == null || toElement == null ) {
            throw new NullPointerException("fromElement and toElement cannot be null");
        }

        if (comparator != null) {
            if ((comparator.compare(fromElement, toElement) > 0)) {
                throw new IllegalArgumentException();
            }
        } else {
            Comparator<E> comp = (Comparator<E>) Comparator.naturalOrder();
            if ((comp.compare(fromElement, toElement) > 0)) {
                throw new IllegalArgumentException();
            }
        }

        return subSection( binarySearch(fromElement,  true, false),  binarySearch(toElement,  true, false));
    }

    @Override
    public SortedSet<E> headSet(E toElement) {
        return subSection(0, binarySearch(toElement,  true, false));
    }

    @Override
    public SortedSet<E> tailSet(E fromElement) {
        return subSection(binarySearch(fromElement,  true, false), size());
    }

    private E getElement(int index) {
        if (isEmpty()) {
            throw new NoSuchElementException();
        }
        return elements.get(index);
    }

    @Override
    public E first() {
        return getElement(0);
    }

    @Override
    public E last() {
        return getElement(size() - 1);
    }

    @SuppressWarnings("unchecked")
    @Override
    public boolean contains(Object o) {
        if (o == null) {
            throw new NullPointerException("null values are not supported");
        }
        return Collections.binarySearch(elements, (E) o, comparator) >= 0;
    }


/*    @Override
    public boolean add(E element) {
        int position = Collections.binarySearch(elements, element, comparator);
        if (position >= 0) {
            return false;
        }
        int insertionPoint = -(position + 1);
        this.elements.add(insertionPoint, element);
        return true;
    }

    @Override
    public boolean addAll(Collection<? extends E> c) {
        boolean flag = false;
        TreeSet<E> sortedSet = new TreeSet<>(comparator);
        sortedSet.addAll(c);
        for (E element : sortedSet) {
            if (add(element)) {
                flag = true;
            }
        }
        return flag;
    }


    @SuppressWarnings("unchecked")
    @Override
    public boolean remove(Object o) {
        E castedObject = (E) o;
        int position = Collections.binarySearch(elements, castedObject, comparator);
        if (position >= 0) {
            this.elements.remove(position);
            return true;
        }
        return false;
    }

    @Override
    public boolean removeAll(Collection<?> c) {
        boolean flag = false;
        for (Object element : c) {
            if (remove(element)) {
                flag = true;
            }
        }
        return flag;
    }*/


/*    public static void main(String[] args) {
        SortedSet<Integer> mySet = new ArraySet<>(Arrays.asList(1280799703, 779406047, -1802278584));
        SortedSet<Integer> newSet = mySet.headSet(1280799703);
        for (Integer e : newSet) {
            System.out.println(e);
        }
    }*/

}


