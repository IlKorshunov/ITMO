package expression.Types;

import java.math.BigInteger;

public class BigIntegers implements Type<BigInteger>{
    @Override
    public BigInteger add(BigInteger x, BigInteger y) {
        return x.add(y);
    }

    @Override
    public BigInteger sub(BigInteger x, BigInteger y) {
        return x.subtract(y);
    }

    @Override
    public BigInteger mul(BigInteger x, BigInteger y) {
        return x.multiply(y);
    }

    @Override
    public BigInteger div(BigInteger x, BigInteger y) {

        return x.divide(y);
    }

    @Override
    public BigInteger unar(BigInteger x) {
        return x.negate();
    }

    @Override
    public BigInteger parse(String x) {
        return new BigInteger(x);
    }

}
