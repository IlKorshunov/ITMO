package expression;

import expression.GenericExpression;

import java.util.Objects;

public class Variable<T> implements GenericExpression<T> {

    private final String x;


    public Variable (String x) {
        this.x = x;
    }

    @Override
    public String toString() {
        return x;
    }

    public T evaluate(T x) {
        return x;
    }



    public boolean equals (Object o){
        if((o == null) || (getClass() != o.getClass())){
            return false;
        }
        return (Objects.equals(x, o.toString()));
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(x);
    }

    @Override
    public T evaluate(T a, T b, T c) {
        T fin = null;
        if (Objects.equals(x, "x")){
            fin = a;
        } else if (Objects.equals(x, "y")){
            fin = b;
        } else if (Objects.equals(x, "z")){
            fin = c;
        }
        return fin;
    }
}
