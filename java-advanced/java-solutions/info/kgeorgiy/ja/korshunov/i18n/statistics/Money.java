package info.kgeorgiy.ja.korshunov.i18n.statistics;

import info.kgeorgiy.ja.korshunov.i18n.application.StatType;
import info.kgeorgiy.ja.korshunov.i18n.application.TypeStat;
import info.kgeorgiy.ja.korshunov.i18n.wrappers.DoubleAdapter;

import java.text.BreakIterator;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.Locale;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@StatType(TypeStat.MONEY)
public class Money extends AbstractStats<Double, Double, DoubleAdapter> {
    private NumberFormat currencyFormatInput;

    private NumberFormat currencyFormatOutrput;
    public Money(Locale inputLocale, Locale outputLocale, String text) {
        super(TypeStat.MONEY, inputLocale, outputLocale, text, BreakIterator.getWordInstance(inputLocale), false,
                Collectors.collectingAndThen(Collectors.summarizingDouble(Double::doubleValue), DoubleAdapter::new));
    }

    @Override
    protected Stream<Double> getStream() {
        this.currencyFormatInput  = NumberFormat.getCurrencyInstance(inputLocale);
        Stream.Builder<Double> builder = Stream.builder();
        BreakIterator dateIterator = BreakIterator.getWordInstance(inputLocale);
        dateIterator.setText(text);
        int start = dateIterator.first();
        int end = dateIterator.next();

        while (end != BreakIterator.DONE) {
            String subText = text.substring(start, end).trim();
            if (!subText.isEmpty()) {
                try {
                    int nextEnd = end;
                    while (nextEnd < text.length() && (Character.isDigit(text.charAt(nextEnd)) || text.charAt(nextEnd) == '+' || text.charAt(nextEnd) == '-' || text.charAt(nextEnd) == ',' || text.charAt(nextEnd) == '.')) {
                        nextEnd++;
                    }
                    String potentialNumber = text.substring(start, nextEnd).trim();
                    if (potentialNumber.charAt(0) == '€') {
                        uniqueItems.add(Double.parseDouble(potentialNumber.substring(1)));
                        items.add(Double.parseDouble(potentialNumber.substring(1)));
                        builder.add(Double.parseDouble(potentialNumber.substring(1)));
                    }
                    Number number = currencyFormatInput.parse(potentialNumber);
                    Double doubleValue = number.doubleValue();
                    items.add(doubleValue);
                    uniqueItems.add(doubleValue);
                    builder.add(doubleValue);
                    start = nextEnd;
                    end = dateIterator.following(start);
                } catch (ParseException e) {
                    // Игнорируем неверные форматы чисел
                }
            }
            start = end;
            end = dateIterator.next();
        }
        return builder.build();
    }

    @Override
    protected int getItemLength(Double item) {
        throw new UnsupportedOperationException("getItemLength не поддерживается для чисел");
    }


/*    public static void main(String[] args) {
        // Пример с деванагари цифрами
        String devanagariText = "₹१२३.४५, ₹४५६.७८, ₹७८९.९०";
        Money devanagariNumbers = new Money(new Locale("hi", "IN"), Locale.US, devanagariText);
        devanagariNumbers.count();

        double mondeyRu = devanagariNumbers.getMax();
        NumberFormat rubleFormat = NumberFormat.getCurrencyInstance(new Locale("ru", "RU"));
        String formattedCurrency = rubleFormat.format(mondeyRu);
        System.out.println("Devanagari Currency - Max Value: " + formattedCurrency);
        System.out.println("Devanagari Currency - Min Value: " + devanagariNumbers.getMin());
        System.out.println("Devanagari Currency - Average Value: " + devanagariNumbers.getAverage());
        System.out.println("Devanagari Currency - Count: " + devanagariNumbers.getCount());
        System.out.println("Devanagari Currency - Unique Count: " + devanagariNumbers.getUniqCount());
    }*/
}
