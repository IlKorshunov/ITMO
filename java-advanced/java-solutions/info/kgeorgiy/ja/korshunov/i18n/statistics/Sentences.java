package info.kgeorgiy.ja.korshunov.i18n.statistics;

import info.kgeorgiy.ja.korshunov.i18n.application.StatType;
import info.kgeorgiy.ja.korshunov.i18n.application.TypeStat;
import info.kgeorgiy.ja.korshunov.i18n.wrappers.IntAdapter;

import java.text.BreakIterator;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;


@StatType(TypeStat.SENTENCE)
public class Sentences extends AbstractStats<String, Integer, IntAdapter> {
    private Map<String, Integer> sentenceWordCountMap;

    public Sentences(Locale inputLocale, Locale outputLocale, String text) {
        super(TypeStat.SENTENCE, inputLocale, outputLocale, text, BreakIterator.getSentenceInstance(inputLocale), true,
                Collectors.collectingAndThen(Collectors.summarizingInt(String::length), IntAdapter::new));
    }

    //

    @Override
    protected Stream<String> getStream() {
        this.sentenceWordCountMap = new HashMap<>();
        Stream.Builder<String> builder = Stream.builder();
        int startWord = 0;
        int endWord;

        while ((endWord = iterator.next()) != BreakIterator.DONE) {
            String curSent = text.substring(startWord, endWord).trim();
            if (curSent.length() > 0 && (curSent.chars().anyMatch(Character::isLetter) || curSent.chars().anyMatch(Character::isDigit))) { // curSent.chars().anyMatch(Character::isLetter)
                int wordCount = countWords(curSent);
                uniqueItems.add(curSent);
                items.add(curSent);
                sentenceWordCountMap.put(curSent, wordCount);
                builder.add(curSent);
            }
            startWord = endWord;
        }
        return builder.build();
    }

    private int countWords(String sentence) {
        BreakIterator wordIterator = BreakIterator.getWordInstance(inputLocale);
        wordIterator.setText(sentence);
        int count = 0;
        int start = wordIterator.first();
        for (int end = wordIterator.next(); end != BreakIterator.DONE; start = end, end = wordIterator.next()) {
            String word = sentence.substring(start, end).trim();
            if (word.length() > 0 && word.chars().anyMatch(Character::isLetter)) {
                count++;
            }
        }
        return count;
    }

    @Override
    protected int getItemLength(String item) {
        return item.length();
    }


}
