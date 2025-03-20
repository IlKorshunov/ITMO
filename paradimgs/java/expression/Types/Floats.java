package expression.Types;

import java.math.BigInteger;

public class Floats implements Type<Float>{

    @Override
    public Float add(Float x, Float y) {
        return x+y;
    }

    @Override
    public Float sub(Float x, Float y) {
        return x-y;
    }

    @Override
    public Float mul(Float x, Float y) {
        return x*y;
    }

    @Override
    public Float div(Float x, Float y) {
        return x/y;
    }

    @Override
    public Float unar(Float x) {
        return -1*x;
    }


    @Override
    public Float parse(String s) {
        return Float.parseFloat(s);
    }
}
