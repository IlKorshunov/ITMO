package info.kgeorgiy.ja.korshunov.i18n.statistics;

import info.kgeorgiy.ja.korshunov.i18n.application.StatType;
import info.kgeorgiy.ja.korshunov.i18n.application.TypeStat;
import info.kgeorgiy.ja.korshunov.i18n.wrappers.SummaryStatistics;

import java.text.BreakIterator;
import java.text.Collator;
import java.text.DateFormat;
import java.util.*;
import java.util.function.Supplier;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public abstract class AbstractStats<T, S extends Number, R extends SummaryStatistics<S>> {
    protected boolean isNull;
    protected String text;
    protected  List<T> items;
    protected  TypeStat type;
    protected  Set<T> uniqueItems;
    protected T longestItem;
    protected T shortestItem;
    protected double average;
    protected S max, min;
    protected  BreakIterator iterator;
    protected  Locale inputLocale, outputLocale;
    protected  Collator myCollator;
    protected  Collector<T, ?, R> collector;
    protected  boolean flag;

    public AbstractStats(TypeStat type, Locale inputLocale, Locale outputLocale, String text, BreakIterator iterator, boolean flag, Collector<T, ?, R> collector) {
        this.type = type;
        this.flag = flag;
        this.inputLocale = inputLocale;
        this.outputLocale = outputLocale;
        this.text = text;
        this.myCollator = Collator.getInstance(inputLocale);
        this.iterator = iterator;
        this.iterator.setText(text);
        this.average = 0;
        this.items = new ArrayList<>();
        this.uniqueItems = new HashSet<>();
        this.collector = collector;
        isNull = false;
    }

    public void delete() {
        text = "";
        items = new ArrayList<>();
        uniqueItems = new HashSet<>();
        longestItem = null;
        shortestItem = null;
        average = 0.0;
        max = null;
        min = null;
    }


    protected abstract Stream<T> getStream();
    protected abstract int getItemLength(T item);


    public static ResourceBundle getBundle(Locale outputLocale) { // здесь вроде можно получше сделать
        String packageName = AbstractStats.class.getPackageName();
        int lastDotIndex = packageName.lastIndexOf('.');
        String parentPackage = packageName.substring(0, lastDotIndex);
        String baseName = parentPackage + ".bundles.bundle_" + outputLocale;
        ResourceBundle curBundle = ResourceBundle.getBundle(baseName, outputLocale);
        return curBundle;
    }

    private <V> V getValue(Supplier<V> sup, V defaultValue) {
        if (isNull) {
            return defaultValue;
        }
        return sup.get();
    }

    private boolean getBoolType() {
        return (type == TypeStat.DATA || type == TypeStat.NUMBER || type == TypeStat.MONEY);
    }

    public String getReport() {
        ResourceBundle curBundle = getBundle(outputLocale);
        count();
        String output = String.format(outputLocale,
                """
                        %s
                            %s: %d (%d %s).
                            %s: "%s".
                            %s: "%s".
                            %s: "%.2f".
                        """,
                curBundle.getString(String.valueOf(type)),
                curBundle.getString("number" + type), getValue(this::getCount, 0), getValue(this::getUniqCount, 0),getCorrectOutput("uni", getUniqCount()),
                curBundle.getString("min" + type), getValue(this::getMin, (getBoolType() ? "0" : "")),
                curBundle.getString("max" + type), getValue(this::getMax, (getBoolType() ? "0" : "")),
                curBundle.getString("avg" + type), getValue(this::getAverage, 0.0));
        if (flag) {
            output += String.format(outputLocale,
                    """
                            %s: "%s".
                            %s: "%s".
                        """,
                    curBundle.getString("min" + type + "Length"), getValue(this::getShortestItem, ""),
                    curBundle.getString("max" + type + "Length"), getValue(this::getLongestItem, ""));
        }

        return output;
    }

    private String getCorrectOutput(String key, int in) {
        ResourceBundle bundle = getBundle(outputLocale);
        String[] forms = bundle.getString(key).split("\\|");
        int mod10 = in % 10;

        if (mod10 == 1) {
            return forms[0];
        } else {
            return forms[1];
        }
    }

    public void count() {
        Stream<T> myStream = getStream();

        List<T> itemsList = myStream.toList();
        if (itemsList.isEmpty()) {
            isNull = true;
            shortestItem = null;
            longestItem = null;
            max = null;
            min = null;
            average = 0;
        } else {
            R stats = itemsList.stream().collect(collector);
            max = stats.getMax();
            min = stats.getMin();
            average = stats.getAverage();
            if (flag) {
                shortestItem = itemsList.stream().filter(item -> getItemLength(item) == min.intValue())
                        .min(myCollator).orElse(null);
                longestItem = itemsList.stream().filter(item -> getItemLength(item) == max.intValue())
                        .max(myCollator).orElse(null);
            }
        }
    }


    public void SetText(String text) {
        this.text = text;
    }

    public int getCount() {
        return items.size();
    }

    public int getUniqCount() {
        return uniqueItems.size();
    }

    public S getMax() {
        return max;
    }

    public S getMin() {
        return min;
    }

    public double getAverage() {
        return average;
    }

    public T getLongestItem() {
        return longestItem;
    }

    public T getShortestItem() {
        return shortestItem;
    }

    public String getType() {
        return type.toString();
    }


    private String formatDate(Date date) {
        if (date == null) {
            return "";
        }
        DateFormat dateFormat = DateFormat.getDateInstance(DateFormat.LONG, outputLocale);
        return dateFormat.format(date);
    }

}
