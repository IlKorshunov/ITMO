package expression.Types;

public class UncheckedIntegers implements Type<Integer>{
    @Override
    public Integer add(Integer x, Integer y) {
        return x+y;
    }

    @Override
    public Integer sub(Integer x, Integer y) {
        return x-y;
    }

    @Override
    public Integer mul(Integer x, Integer y) {
        return x*y;
    }

    @Override
    public Integer div(Integer x, Integer y) {
        return x/y;
    }

    @Override
    public Integer unar(Integer x) {
        return -1*x;
    }


    @Override
    public Integer parse(String s) {
        return Integer.parseInt(s);
    }
}
