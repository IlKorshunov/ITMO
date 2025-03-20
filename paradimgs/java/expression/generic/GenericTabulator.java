package expression.generic;

import expression.GenericExpression;
import expression.Types.*;
import expression.exceptions.ExpressionParser;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Scanner;

public class GenericTabulator implements Tabulator {
    @Override
    public Object[][][] tabulate(String mode, String expression, int x1, int x2, int y1, int y2, int z1, int z2) throws Exception {
        Type<?> oper = null;
        if (Objects.equals(mode, "i")){
            oper = new Integers();
        } else if (Objects.equals(mode, "d")){
            oper = new Doubles();
        } else if (Objects.equals(mode, "bi")){
            oper = new BigIntegers();
        } else if (Objects.equals(mode, "u")){
            oper = new UncheckedIntegers();
        } else if (Objects.equals(mode, "f")){
            oper = new Floats();
        } else if (Objects.equals(mode, "s")){
            oper = new Shorts();
        }
        return ans(oper, expression, x1, x2, y1, y2, z1, z2);
    }

    public <T> Object[][][] ans(Type<T> oper, String expression, int x1, int x2, int y1, int y2, int z1, int z2) throws Exception{
        GenericExpression<T> exception = new ExpressionParser<>(oper).parse(expression);
        int first = x2 - x1 + 1;
        int second = y2 - y1 + 1;
        int third = z2 - z1 + 1;
        Object[][][] ans = new Object[first][second][third];
        for (int i = 0; i < first; i++) {
            for (int j = 0; j < second; j++) {
                for (int k = 0; k < third; k++) {
                    try {
                        ans[i][j][k] = exception.evaluate(oper.parse(Integer.toString(x1 + i)),
                                oper.parse(Integer.toString(y1 + j)),
                                oper.parse(Integer.toString(z1 + k)));
                    } catch (ArithmeticException e) {
                        ans[i][j][k] = null;
                    }
                }
            }
        }
        return ans;
    }
}
