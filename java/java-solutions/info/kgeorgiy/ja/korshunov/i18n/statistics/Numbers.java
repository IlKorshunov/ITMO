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

@StatType(TypeStat.NUMBER)
public class Numbers extends AbstractStats<Double, Double, DoubleAdapter> {
    private NumberFormat numberFormat;

    public Numbers(Locale inputLocale, Locale outputLocale, String text) {
        super(TypeStat.NUMBER, inputLocale, outputLocale, text, BreakIterator.getWordInstance(inputLocale), false,
                Collectors.collectingAndThen(Collectors.summarizingDouble(Double::doubleValue), DoubleAdapter::new));
    }

    @Override
    protected Stream<Double> getStream() {
        this.numberFormat = NumberFormat.getInstance(Locale.CHINESE);
        Stream.Builder<Double> builder = Stream.builder();
        BreakIterator charIterator = BreakIterator.getCharacterInstance(Locale.CHINESE);
        charIterator.setText(text);
        int start = charIterator.first();
        int end = charIterator.next();

        while (end != BreakIterator.DONE) {
            String subText = text.substring(start, end).trim();
            if (!subText.isEmpty()) {
                try {
                    int nextEnd = end;
                    while (nextEnd < text.length() && (Character.isDigit(text.charAt(nextEnd)) || text.charAt(nextEnd) == '+' || text.charAt(nextEnd) == '-' || text.charAt(nextEnd) == ',')) {
                        nextEnd++;
                    }
                    String potentialNumber = text.substring(start, nextEnd).trim();
                    Number number = numberFormat.parse(potentialNumber);
                    Double doubleValue = number.doubleValue();
                    items.add(doubleValue);
                    uniqueItems.add(doubleValue);
                    builder.add(doubleValue);
                    start = nextEnd;
                    end = charIterator.following(start);
                } catch (ParseException e) {
                    // Игнорируем неверные форматы чисел
                }
            }
            start = end;
            end = charIterator.next();
        }
        return builder.build();
    }

    @Override
    protected int getItemLength(Double item) {
        throw new UnsupportedOperationException("getItemLength не поддерживается для чисел");
    }



/*    public static void main(String[] args) {
        String devanagariText = "१२३, ४५६, ७८९";
        Numbers devanagariNumbers = new Numbers(new Locale("hi", "IN"), Locale.US, devanagariText);

        System.out.println("Devanagari Numbers - Max Value: " + devanagariNumbers.getMax());
        System.out.println("Devanagari Numbers - Min Value: " + devanagariNumbers.getMin());
        System.out.println("Devanagari Numbers - Average Value: " + devanagariNumbers.getAverage());
        System.out.println("Devanagari Numbers - Count: " + devanagariNumbers.getCount());
        System.out.println("Devanagari Numbers - Unique Count: " + devanagariNumbers.getUniqCount());

        String thaiText = "๑๒๓ ๔๕๖ ๗๘๙";
        Numbers thaiNumbers = new Numbers(new Locale("th", "TH"), Locale.US, thaiText, BreakIterator.getWordInstance(new Locale("th", "TH")));

        System.out.println("Thai Numbers - Max Value: " + thaiNumbers.getMaxLen());
        System.out.println("Thai Numbers - Min Value: " + thaiNumbers.getMinLen());
        System.out.println("Thai Numbers - Average Value: " + thaiNumbers.getAverage());
        System.out.println("Thai Numbers - Count: " + thaiNumbers.getCount());
        System.out.println("Thai Numbers - Unique Count: " + thaiNumbers.getUniqCount());
    }*/
}
