package expression;


import expression.Types.Type;
import expression.GenericExpression;

public class  Subtract<T> extends all<T>{


    public Subtract (GenericExpression<T> one, GenericExpression<T> two, Type<T> oper) {
        super(one, two, oper,  "-");
    }



    public T funk (T a, T b){
        return oper.sub(a, b);
    }
}
