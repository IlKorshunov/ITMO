package info.kgeorgiy.ja.korshunov.i18n.wrappers;

import java.util.DoubleSummaryStatistics;

public class DoubleAdapter implements SummaryStatistics<Double> {
    private final DoubleSummaryStatistics stats;

    public DoubleAdapter(DoubleSummaryStatistics stats) {
        this.stats = stats;
    }

    @Override
    public Double getMax() {
        return stats.getMax();
    }

    @Override
    public Double getMin() {
        return stats.getMin();
    }

    @Override
    public double getAverage() {
        return stats.getAverage();
    }
}
