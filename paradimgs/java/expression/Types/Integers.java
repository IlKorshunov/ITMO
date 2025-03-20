package expression.Types;

import expression.exceptions.CheckMistakes;
import expression.exceptions.CheckOverflow;

public class Integers implements Type<Integer>{
    @Override
    public Integer add(Integer x, Integer y) {
        if ((x > 0 && y > 0 && x > Integer.MAX_VALUE - y) || (x < 0 && y < 0 && x < Integer.MIN_VALUE - y)){
            throw new CheckOverflow("overflow");
        } else {
            return x + y;
        }
    }

    @Override
    public Integer sub(Integer x, Integer y) {
        if ((y > 0 && Integer.MIN_VALUE + y > x) || (y < 0 && Integer.MAX_VALUE + y < x)){ // ?
            throw new CheckOverflow("overflow");
        } else {
            return x - y;
        }
    }

    @Override
    public Integer mul(Integer x, Integer y) {
        int mul = x * y;
        if ((x != 0 && mul / x != y) || (y != 0 && mul / y != x)) { // ?
            throw new CheckOverflow("overflow");
        }
        return x * y;
    }

    @Override
    public Integer div(Integer x, Integer y) {
        if (x == Integer.MIN_VALUE && y == -1){
            throw new CheckOverflow("overflow");
        } else if (y == 0) {
            throw new CheckMistakes("divide by zero");
        }else {
            return x / y;
        }
    }

    @Override
    public Integer unar(Integer x) {
        if (x == Integer.MIN_VALUE){
            throw new CheckOverflow("overflow");
        } else {
            return -1 * x;
        }
    }

    @Override
    public Integer parse(String x) {
        return Integer.parseInt(x);
    }


}
