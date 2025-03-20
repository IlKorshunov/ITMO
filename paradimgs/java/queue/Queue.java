package queue;

public interface Queue {

    // Model : a[start]...a[end] // ?
    // Inv : start >= 0  for i start ... end a[i] != null && start <= i <= size
    // Immut : for i start ... end a'[i] = a[i]

    // Pred : element != null
    // a[size] = element && size++ && Immut && Inv
    void enqueue(Object element);

    // Pred : size >= 0 && element[start] != null
    // Post : start' = start + 1 && size-- &&  Immut && Inv
    Object dequeue();

    //  Pred : size >= 0 && element[start] != null
    // R = a[start] && Immut && Inv
    Object element();

    // Pred : size >= 0
    // Post : R = size && Immut && Inv
    int size();

    // Pred : size >= 0
    // Post : R = (size == 0)
    boolean isEmpty();

    // Pred : true
    // Post : start = 0 && size == 0 && for i 0 ... n a[i] = null
    void clear();

    // Pred : true
    // Post : min index : a[index] == element, -1 if not exist && Immut && Inv
    int indexOf(Object element);

    // Pred : true
    // Post : max index : a[index] == element, -1 if not exist && Immut && Inv
    int lastIndexOf(Object element);
}
