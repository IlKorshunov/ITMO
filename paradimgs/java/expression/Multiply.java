package expression;

import expression.Types.Type;
import expression.GenericExpression;

public class Multiply<T> extends all<T> {


    public Multiply (GenericExpression<T> one, GenericExpression<T> two, Type<T> oper) {
        super(one, two, oper,"*");
    }



    public T funk (T a, T b){
        return oper.mul(a, b);
    }
}
