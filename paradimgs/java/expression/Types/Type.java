package expression.Types;

public interface Type <T>{
    T add(T x, T y);
    T sub(T x, T y);
    T mul(T x, T y);
    T div(T x, T y);
    T unar(T x);

   // T count(T x);

    T parse(String s);
}
