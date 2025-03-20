package expression.exceptions;

import expression.*;
import expression.Types.Type;
import expression.GenericExpression;

import static expression.exceptions.Lexem.*;

public class ExpressionParser<T> {
    private BaseParser parser;
    private final Type<T> oper;

    public ExpressionParser(Type<T> oper) {
        this.oper = oper;
    }

    public GenericExpression<T> parse(String expression){
       // System.err.println(expression);
        parser = new BaseParser(expression);
        parser.getToken();
        return parseExpr();
    }

    public GenericExpression<T> parseExpr(){
        return SetClear();
    }


    public GenericExpression<T> SetClear(){
        GenericExpression<T> now = SumSub();
        return parseSetClear(now);
    }

    public GenericExpression<T> parseSetClear(GenericExpression<T> now){
        return now;
    }

    public GenericExpression<T> SumSub(){
        GenericExpression<T> now = MulDiv();
        return parseSumSub(now);
    }

    public GenericExpression<T> parseSumSub(GenericExpression<T> now){
        GenericExpression<T> expr;
        if (parser.getTypeToken() == PLUS){
            parser.getToken();
            expr = MulDiv();
            return parseSumSub(new Add<>(now, expr, oper));
        } else if (parser.getTypeToken() == MINUS){
            parser.getToken();
            expr = MulDiv();
            return parseSumSub(new Subtract<>(now, expr, oper));
        } else {
            return now;
        }
    }

    public GenericExpression<T> MulDiv(){
        GenericExpression<T> now = parseVarCons();
        return parseMulDiv(now);
    }
    public GenericExpression<T> parseMulDiv(GenericExpression<T> now){
        GenericExpression<T> expr;
        if (parser.getTypeToken() == MUL){
            parser.getToken();
            expr = parseVarCons();
            return parseMulDiv(new Multiply<>(now, expr, oper));
        } else if (parser.getTypeToken() == DIV){
            parser.getToken();
            expr = parseVarCons();  //
            return parseMulDiv(new Divide<>(now, expr, oper));
        } else {
            return now;
        }
    }

    public GenericExpression<T> parseVarCons(){
        String str;
        if (parser.getTypeToken() == CONST){
            str = parser.getExpression();
            parser.getToken();
            return new Const<>(oper.parse(str));
        } else if (parser.getTypeToken() == VARIABLE){
            str = parser.getExpression();
            parser.getToken();
            return new Variable<>(str);
        } else if (parser.getTypeToken() == MINUS){
            parser.getToken();
            if (parser.getTypeToken() == CONST){
                str =  parser.getExpression();
                parser.getToken();
                return new Const<>(oper.parse('-' + str));
            } /*else if (parser.getTypeToken() == VARIABLE){
                str =  parser.getExpression();
                parser.getToken();
                return new UnarnMinus<>(new Variable<>(str), oper);
            } */
            return new UnarnMinus<>(parseVarCons(), oper);
        } else if (parser.getTypeToken() == LBREK){
            parser.getToken();
            GenericExpression<T> stapl = parseExpr();
            if (parser.getTypeToken() == RBREK) {
                parser.getToken();
                return stapl;
            }
        }
            throw new CheckBrek("unknown symbol, position : " + parser.pos);
    }
}
