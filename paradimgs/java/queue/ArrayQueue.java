package queue;

import java.util.Arrays;
import java.util.Objects;



public class ArrayQueue extends AbstractQueue{

    // Model : a[start]...a[end]
    // Inv : start >= 0 && end >= 0 for i start ... end a[i] != null && start <= i <= end
    // Immut : for i start ... end a'[i] = a[i]
    private  int start = 0;

    private Object[] elements = new Object[4];

    // Pred : element != null
    // Post : end' = (end + 1)%element.length && a[end'] = element && immut;

    @Override
    public void enequeImpl(Object element) {
        Objects.requireNonNull(element);
        cycle(size + 2);
        int end = (start + size)%elements.length;
        elements[end] = element;
    }

    public void cycle(int size){
        if (elements.length >= size){
            return;
        }
        Object[] tmp = new Object[2 * size];
        copy(tmp);
        elements = tmp;
        start = 0;
    }

    public void copy(Object [] tmp){
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


    @Override
    public Object elementImpl() {
        return elements[start];
    }

    // Pred : size() >= 1
    // Post : start' = (start+1)%elememnts.length && R = a[n]

    @Override
    public void dequeImpl() {
        result = elements[start];
        elements[start] = null;
        start = (start + 1) % elements.length;
    }

    // Pred : true
    // Post : R == size() && n' == n && immut


    // Pred : true
    // Post: R == (size() == 0) && n' == n' && immut

    // Pred : true
    // Post : size() == 0 && end == start = 0

    @Override
    protected void clearImpl() {
        elements = new Object[elements.length];
        start = 0;
    }

    public  int indexOf(Object element){
        int a = start;
        int count = 0;
        while(elements[a] != null){
            if (Objects.equals(elements[a], element)){
                return count;
            }
            a = (a + 1) % elements.length;
            count++;
        }
        return -1;
    }

    public int lastIndexOf(Object element){
        int a = (start + size - 1) % elements.length;
        if (a < 0) {
            a = elements.length - 1;
        }
        int count = size() - 1;
        while (elements[a] != null){
            if (Objects.equals(elements[a], element)){
                return count;
            }
            a--;
            if(a == -1) {
                a = elements.length - 1;
            }
            count--;
        }
        return -1;
    }

    public Object[] toArray(){
        Object [] temp = new Object[size];
        copy(temp);
        return temp;
    }

}
