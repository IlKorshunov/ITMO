package expression.Types;

public class Shorts implements Type<Short>{

    @Override
    public Short add(Short x, Short y) {
        return (short) (x + y);
    }

    @Override
    public Short sub(Short x, Short y) {
        return (short) (x - y);
    }

    @Override
    public Short mul(Short x, Short y) {
        return (short) (x * y);
    }

    @Override
    public Short div(Short x, Short y) {
        return (short) (x / y);
    }

    @Override
    public Short unar(Short x) {
        return (short) (-x);
    }



    @Override
    public Short parse(String s) {
        return (short) Integer.parseInt(s);
    }
}
