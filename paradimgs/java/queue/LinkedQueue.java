package queue;

import java.util.Objects;

public class LinkedQueue extends AbstractQueue{
    private Node head, tail;
    private Node mod;

    private static class Node{
        private Object element;
        private Node prev;
        private Node next;


        public Node(Object element, Node prev) {
            assert element != null;
            this.element = element;
            this.prev = prev;
        }
    }

    @Override
    public void enequeImpl(Object element) {
        assert element != null;
        if (head == null){
            tail =  new Node(element, null);
            head = tail;
        } else {
            tail = new Node(element, tail);
            tail.prev.next = tail;
        }
    }


    @Override
    public void dequeImpl() {
        result = head.element;
        head = head.next;
        if (head != null){
            head.prev = null;
        } else {
            tail = null;
        }
    }

    public int indexOf(Object element){
        int count = 0;
        Node mod = head;
        while (mod != null){
            if (Objects.equals(mod.element, element)){
                return count;
            }
            count++;
            mod = mod.next;
        }
        return -1;
    }

    public int lastIndexOf(Object element){
        int count = 0;
        int res = -1;
        Node mod = head;
        while (mod != null){
            if (Objects.equals(mod.element, element)){
                res = count;
            }
            count++;
            mod = mod.next;
        }
        return res;
    }


    @Override
    public Object elementImpl() {
        return head.element;
    }

    @Override
    public void clearImpl(){
        head = null;
        tail = null;
    }

}