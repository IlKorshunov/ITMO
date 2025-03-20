package expression.exceptions;

import static expression.exceptions.Lexem.*;

public class BaseParser {

    protected StringBuilder expression;
    protected Lexem token;
    private final StringSource source;
    private char ch;

    private int balance;
    int pos;
    private static final char Final = '\0';  


    public BaseParser(String string) {
        this.source = new StringSource(string);
        nextChar();
    }

    private void nextChar(){
        ch = source.hasNext() ? source.next() : Final;
        pos++;
    }
    private void skipWhiteSpaces(){  
        while (Character.isWhitespace(ch)) {
            nextChar();
        }
    }

    protected boolean Digit(){
        return between('0', '9');
    }
    protected boolean between(final char from, final char to) {
        return from <= ch && ch <= to;
    }

    protected void getToken(){
        expression = new StringBuilder();
        skipWhiteSpaces();
        if (Digit()){
            token = CONST;
            while (Digit()){
                expression.append(ch);
                nextChar();
            }
        } else if ( ch == '+'){
            takeOper(PLUS);
        } else if ( ch == '-'){
            takeOper(MINUS);
        } else if ( ch == '*'){
            takeOper(MUL);
        } else if ( ch == '/'){
            takeOper(DIV);
        } else if ( ch == '('){
            balance++;
            takeOper(LBREK);
        } else if ( ch == 'x' || ch == 'y' || ch == 'z'){
            token = VARIABLE;
            expression.append(ch);
            nextChar();
        } else if (ch == Final){
            takeOper(END);
        } else if (ch == ')'){
            balance--;
            takeOper(RBREK);
        } else {
                throw new CheckMistakes("Unknown symbol");
        }
    }

    private void takeOper(Lexem lex) {
        expression.append(ch);
        token = lex;
        nextChar();
    }

    protected Lexem getTypeToken(){
        return token;
    }

    protected String getExpression(){
        return String.valueOf(expression);
    }
}
