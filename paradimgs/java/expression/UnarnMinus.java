package expression;

import expression.Types.Type;
import expression.GenericExpression;

import java.util.Objects;

public class UnarnMinus<T> implements GenericExpression<T> {

    protected GenericExpression<T> one;
    protected final Type<T> oper;
    public UnarnMinus(GenericExpression<T> one, Type<T> oper) {
        this.one = one;
        this.oper = oper;
    }


    public T evaluate(T x) {
        return oper.unar(one.evaluate(x));
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return  oper.unar(one.evaluate(x, y, z));
    }

    @Override
    public String toString() {
        return "-" + "(" + one.toString() + ")";
    }
}
