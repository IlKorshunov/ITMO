package expression;

import expression.GenericExpression;

import java.util.Objects;

public class Const<T> implements GenericExpression<T> {
    private final T constanta;

    public Const(T constanta){
        this.constanta = constanta;
    }

    @Override
    public String toString() {
        return String.valueOf(constanta);
    }

    public T evaluate(T x) {
        return constanta;
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return constanta;
    }

    public boolean equals (Object o){
        if((o == null) || (getClass() != o.getClass())){
           return false;
        }
        return (Objects.equals(constanta, Integer.parseInt(o.toString())));
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(constanta);
    }

    protected int count(int number){
        return Integer.bitCount( number);
    }

}
