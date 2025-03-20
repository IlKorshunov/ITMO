package queue;

import java.util.Arrays;
import java.util.Objects;
/*
import static queue.ArrayDequeModuleTest.fill;*/

public class ArrayQueueADT {

    // Model : a[start]...a[end]
    // Inv : start >= 0 && end >= 0 for i start ... end a[i] != null && start <= i <= end
    // Immut : for i start ... end a'[i] = a[i]
    private int start = 0;
    private int size = 0;
    private Object[] elements = new Object[4];

    // Pred : element != null
    // Post : end' = (end + 1)%element.length && a[end'] = element && immut;
    public static void enqueue(final ArrayQueueADT queue, Object element){
        Objects.requireNonNull(element);
        cycle(queue, queue.size + 2);
        int end = (queue.start + queue.size)%queue.elements.length;
        queue.elements[end] = element;
        queue.size++;
    }

    public static void cycle(final ArrayQueueADT queue, int size){
        if (queue.elements.length >= size){
            return;
        }
        Object[] tmp = new Object[2 * size];
        copy(queue, tmp);
        queue.elements = tmp;
        queue.start = 0;
    }

    public static void copy(final ArrayQueueADT queue, Object [] tmp){
        int end = (queue.start + queue.size)% queue.elements.length;
        if (end >= queue.start) {
            System.arraycopy(queue.elements, queue.start, tmp, 0, end - queue.start);
        } else {
            System.arraycopy(queue.elements, queue.start, tmp, 0, queue.elements.length - queue.start);
            System.arraycopy(queue.elements, 0, tmp, queue.elements.length - queue.start, end);
        }
    }


    // Pred : size() >= 1
    // Post : n' == n && immut && R = a[n]

    public static Object element(final ArrayQueueADT queue){
        assert size(queue) > 0;
        return queue.elements[queue.start];
    }

    // Pred : size() >= 1
    // Post : start' = (start+1)%elememnts.length && R = a[n]
    static public Object dequeue(final ArrayQueueADT queue) {
        Object result = queue.elements[queue.start];
        queue.elements[queue.start] = null;
        queue.start = (queue.start + 1) % queue.elements.length;
        queue.size--;
        return result;
    }

    // Pred : true
    // Post : R == size() && n' == n && immut
    public static int size(final ArrayQueueADT queue) {
        return queue.size;
    }

    // Pred : true
    // Post: R == (size() == 0) && n' == n' && immut
    public static boolean isEmpty(final ArrayQueueADT queue){
        return queue.size == 0;
    }

    // Pred : true
    // Post : size() == 0 && end == start = 0
    public static void clear(final ArrayQueueADT queue){
        queue.elements = new Object[queue.elements.length];
        queue.start = 0;
        queue.size = 0;
    }

    public static Object[] toArray(final ArrayQueueADT queue){
        Object [] temp = new Object[size(queue)];
        copy(queue, temp);
        return temp;
    }

}
