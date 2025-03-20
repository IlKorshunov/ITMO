package expression;
import expression.Types.Type;
import expression.GenericExpression;

public class Add<T> extends all<T> {

    public Add(GenericExpression<T> one, GenericExpression<T> two, Type<T> oper) {
        super(one, two, oper, "+");
    }

    public T funk (T a, T b){
        return oper.add(a,b);
    }

}
