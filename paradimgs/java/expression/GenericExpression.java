package expression;

public interface GenericExpression<T> {
    T evaluate(T x, T y, T z);
    T evaluate(T x);

}
