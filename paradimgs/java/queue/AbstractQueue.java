package queue;

public abstract class AbstractQueue implements Queue {
    protected int size;
    protected Object result;


    public void enqueue(Object element){
        enequeImpl(element);
        this.size++;
    }

    public abstract void enequeImpl(Object element);

    public Object dequeue(){
        assert this.size > 0;
        dequeImpl();
        this.size--;
        return result;
    }

    public abstract void dequeImpl();

    public  Object element(){
        assert this.size > 0;
        return elementImpl();
    }

    public abstract Object elementImpl();

    public int size(){
        return this.size;
    }

    public  boolean isEmpty(){
        return this.size == 0;
    }

    public void clear(){
        size = 0;
        clearImpl();
    }
    protected abstract void clearImpl();

    public abstract int indexOf(Object element);

    public abstract int lastIndexOf(Object element);

}