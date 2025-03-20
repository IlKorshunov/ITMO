/*package expression;

import expression.Types.Type;

public class Count<T> implements TripleExpression<T> {
    private final TripleExpression<T> count;
    protected final Type<T> oper;
    public Count(TripleExpression<T> count1, Type<T> oper) {
        this.count = count1;
        this.oper = oper;
    }
    public String toString() {
        return "count(" + count.toString() + ")";
    }


    public T evaluate(T x) {
        return oper.count(count.evaluate(x));
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return oper.count(count.evaluate(x, y, z));
    }
}*/
