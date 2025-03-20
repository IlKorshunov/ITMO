package expression;

import expression.Types.Type;
import expression.GenericExpression;

import java.util.Objects;

public abstract class all<T> implements GenericExpression<T> {

    protected final GenericExpression<T> one;
    protected final GenericExpression<T> two;
    String znak;
    protected Type<T> oper;

    protected StringBuilder sb = new StringBuilder();

    public all(GenericExpression<T> one, GenericExpression<T> two, Type<T> oper, String znak){
        this.one = one;
        this.two = two;
        this.znak = znak;
        this.oper = oper;
        sb.append(one).append(" ").append(znak).append(" ").append(two);

    }

    @Override
    public String toString(){
        return "(" + sb + ")";
    }


    public boolean equals (Object o) {
        if((o == null) || (this.getClass() != o.getClass())){
            return false;
        }
        all<?> another = (all<?>) o;
        return (one.equals(another.one) && two.equals(another.two));
    }

    public int hashCode(){
        return ((Objects.hashCode(one)*101 + Objects.hashCode(two))*103 + Objects.hashCode(znak)*217);
    }

    public T evaluate(T x, T y, T z) {
        return funk (one.evaluate(x, y, z) , two.evaluate(x, y, z));
    }

    public T evaluate(T x) {
        return funk (one.evaluate(x) , two.evaluate(x));
    }

    public abstract T funk (T a, T b);

}
