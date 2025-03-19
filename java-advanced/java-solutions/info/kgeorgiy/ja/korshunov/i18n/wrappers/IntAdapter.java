package info.kgeorgiy.ja.korshunov.i18n.wrappers;

import info.kgeorgiy.ja.korshunov.i18n.wrappers.SummaryStatistics;

import java.util.IntSummaryStatistics;
import java.util.DoubleSummaryStatistics;

public class IntAdapter implements SummaryStatistics<Integer> {
    private final IntSummaryStatistics stats;

    public IntAdapter(IntSummaryStatistics stats) {
        this.stats = stats;
    }

    @Override
    public Integer getMax() {
        return stats.getMax();
    }

    @Override
    public Integer getMin() {
        return stats.getMin();
    }

    @Override
    public double getAverage() {
        return stats.getAverage();
    }
}

