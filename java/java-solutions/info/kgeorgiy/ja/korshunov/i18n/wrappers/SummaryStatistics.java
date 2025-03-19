package info.kgeorgiy.ja.korshunov.i18n.wrappers;

public interface SummaryStatistics<T extends Number> {
    T getMax();
    T getMin();
    double getAverage();
}

