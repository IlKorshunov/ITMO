package info.kgeorgiy.ja.korshunov.i18n.statistics;

import info.kgeorgiy.ja.korshunov.i18n.application.StatType;
import info.kgeorgiy.ja.korshunov.i18n.application.TypeStat;
import info.kgeorgiy.ja.korshunov.i18n.wrappers.IntAdapter;

import java.text.BreakIterator;
import java.util.Locale;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@StatType(TypeStat.WORD)
public class Words extends AbstractStats<String, Integer, IntAdapter> {
    public Words(Locale inputLocale, Locale outputLocale, String text) {
        super(TypeStat.WORD, inputLocale, outputLocale, text, BreakIterator.getWordInstance(inputLocale), true,
                Collectors.collectingAndThen(Collectors.summarizingInt(String::length), IntAdapter::new));
    }


    @Override
    protected Stream<String> getStream() {
        Stream.Builder<String> builder = Stream.builder();
        int startWord = 0;
        int endWord;

        while ((endWord = iterator.next()) != BreakIterator.DONE) {
            String curWord = text.substring(startWord, endWord).trim();
            if (curWord.length() > 0  && curWord.chars().anyMatch(Character::isLetter)) { //  &&
                uniqueItems.add(curWord);
                items.add(curWord);
                builder.add(curWord);
            }
            startWord = endWord;
        }

        return builder.build();
    }

    @Override
    protected int getItemLength(String item) {
        return item.length();
    }

}
