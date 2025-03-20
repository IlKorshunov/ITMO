package expression;

import expression.Types.Type;
import expression.GenericExpression;

public class Divide<T> extends all<T>  {

    public Divide (GenericExpression<T> one, GenericExpression<T> two, Type<T> oper) {
        super(one, two, oper,"/");
    }



    public T funk (T a, T b){
        return oper.div(a, b);
    }
}
