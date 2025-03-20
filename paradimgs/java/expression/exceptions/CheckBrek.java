package expression.exceptions;

public class CheckBrek extends RuntimeException{
    public CheckBrek (String message){
        super(message);
    }

    public CheckBrek(String message, Throwable cause) {
        super(message, cause);
    }
}
