package queue;

import java.util.Arrays;
import java.util.Objects;

/*import static queue.ArrayDequeModuleTest.fill;*/

public class ArrayQueueModule {

    // Model : a[start]...a[end]
    // Inv : start >= 0 && end >= 0 for i start ... end a[i] != null && start <= i <= end
    // Immut : for i start ... end a'[i] = a[i]
    private static int start = 0;
    private static int size = 0;
    private static Object[] elements = new Object[4];

    // Pred : element != null
    // Post : size++ && a[(start + size)%elements.length] = element && immut;
   public static void enqueue(Object element){
        Objects.requireNonNull(element);
        cycle(size + 2);
        int end = (start + size)%elements.length;
        elements[end] = element;
        size++;
    }
    //Pred :  true
    // Post :  start = 0 && elements.length' = 2*elements.length && size' = size && Immut

    public static void cycle(int size){
       if (elements.length >= size){
           return;
       }
        Object[] tmp = new Object[2 * size];
        copy(tmp);
        elements = tmp;
        start = 0;
    }

    // Pred : Object[] tmp = new Object[2 * size];
    // Post : start <= end && for i start ... end && elements'[i] = elements[i]
    public static void copy(Object [] tmp){
       int end = (start + size)% elements.length;
        if (end >= start) {
            System.arraycopy(elements, start, tmp, 0, end - start);
        } else {
            System.arraycopy(elements, start, tmp, 0, elements.length - start);
            System.arraycopy(elements, 0, tmp, elements.length - start, end);
        }
    }


    // Pred : size() >= 1
    // Post : n' == n && immut && R = a[n]

    public static Object element(){
        assert size() > 0;
        return elements[start];
    }

    // Pred : size() >= 1
    // Post : start' = (start+1)%elememnts.length && R = a[n]
    static public Object dequeue() {
        Object result = elements[start];
        elements[start] = null;
        start = (start + 1) % elements.length;
        size--;
        return result;
    }

    // Pred : true
    // Post : R == size() && n' == n && immut
    public static int size() {
        return size;
    }

    // Pred : true
    // Post: R == (size() == 0) && n' == n' && immut
    public static boolean isEmpty(){
        return size == 0;
    }

    // Pred : true
    // Post : size() == 0 && start = 0
    public static void clear(){
        elements = new Object[elements.length];
        start = 0;
        size = 0;
    }

    // Pred: size >= 0
    // Post: мы вернем очердь так как просят в условии
    public static Object[] toArray(){
        Object [] temp = new Object[size()];
        copy(temp);
        return temp;
    }

}

