package info.kgeorgiy.ja.korshunov.i18n.statistics;

import info.kgeorgiy.ja.korshunov.i18n.application.StatType;
import info.kgeorgiy.ja.korshunov.i18n.application.TypeStat;
import info.kgeorgiy.ja.korshunov.i18n.wrappers.DoubleAdapter;

import java.text.*;
import java.util.Date;
import java.util.Locale;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@StatType(TypeStat.DATA)
public class Dates extends AbstractStats<Date, Double, DoubleAdapter> {
    private final DateFormat dateFormat;

    public Dates(Locale inputLocale, Locale outputLocale, String text) {
        super(TypeStat.DATA, inputLocale, outputLocale, text, BreakIterator.getWordInstance(inputLocale), false,
                Collectors.collectingAndThen(Collectors.summarizingDouble(Date::getTime), DoubleAdapter::new));
        this.dateFormat = DateFormat.getDateInstance(DateFormat.LONG, outputLocale);
    }

    @Override
    protected Stream<Date> getStream() {
        Stream.Builder<Date> builder = Stream.builder();
        BreakIterator sentenceIterator = BreakIterator.getSentenceInstance(inputLocale);
        sentenceIterator.setText(text);

        int sentenceStart = sentenceIterator.first();

        for (int sentenceEnd = sentenceIterator.next(); sentenceEnd != BreakIterator.DONE; sentenceStart = sentenceEnd, sentenceEnd = sentenceIterator.next()) {
            String sentence = text.substring(sentenceStart, sentenceEnd).trim();
            if (!sentence.isEmpty()) {
                BreakIterator wordIterator = BreakIterator.getWordInstance(inputLocale);
                wordIterator.setText(sentence);
                int wordStart = wordIterator.first();
                int wordCount = 0;
                StringBuilder threeWords = new StringBuilder();

                for (int wordEnd = wordIterator.next(); wordEnd != BreakIterator.DONE; wordStart = wordEnd, wordEnd = wordIterator.next()) {
                    String word = sentence.substring(wordStart, wordEnd).trim();
                    if (!word.isEmpty()) {
                        if (wordCount > 0) {
                            threeWords.append(" ");
                        }
                        threeWords.append(word);
                        wordCount++;
                        if (wordCount == 3) {
                            String potentialDate = threeWords.toString();
                            Date date = parseDate(potentialDate);
                            if (date != null) {
                                items.add(date);
                                uniqueItems.add(date);
                                builder.add(date);
                            }
                            threeWords.setLength(0);
                            wordCount = 0;
                        }
                    }
                }
            }
        }

        return builder.build();
    }

    private Date parseDate(String input) {
        if (inputLocale == Locale.GERMANY) {
            String[] germanFormats = {
                    "dd.MM.yyyy",
                    "dd MMMM yyyy",
                    "dd.MM.yy",
            };

            for (String format : germanFormats) {
                try {
                    SimpleDateFormat sdf = new SimpleDateFormat(format, inputLocale);
                    sdf.setLenient(false);
                    ParsePosition posit = new ParsePosition(0);
                    Date date = sdf.parse(input, posit);
                    if (date != null && posit.getIndex() == input.length()) {
                        return date;
                    }
                } catch (Exception ignored) {
                }
            }
        }

        for (int format : new int[]{DateFormat.FULL, DateFormat.LONG, DateFormat.MEDIUM, DateFormat.SHORT}) {
            DateFormat dateFormat = DateFormat.getDateInstance(format, inputLocale);
            ParsePosition posit = new ParsePosition(0);
            Date date = dateFormat.parse(input, posit);
            if (date != null && posit.getIndex() == input.length()) {
                return date;
            }
        }

        return null;
    }

    public String formatDate(Date date) {
        return dateFormat.format(date);
    }

    @Override
    protected int getItemLength(Date item) {
        throw new UnsupportedOperationException("getItemLength не поддерживается для дат");
    }
}
