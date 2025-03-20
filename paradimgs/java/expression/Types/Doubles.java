package expression.Types;

public class Doubles implements Type<Double>{

    @Override
    public Double add(Double x, Double y) {
        return x + y;
    }

    @Override
    public Double sub(Double x, Double y) {
        return x - y;
    }

    @Override
    public Double mul(Double x, Double y) {
        return x * y;
    }

    @Override
    public Double div(Double x, Double y) {
        return x / y;
    }

    @Override
    public Double unar(Double x) {
        return -x;
    }

    @Override
    public Double parse(String x) {
        return Double.parseDouble(x);
    }


}
