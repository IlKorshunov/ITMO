package info.kgeorgiy.ja.korshunov.i18n.application;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Locale;

public class Statistic {

    public static void main(String[] args) {
        try {
            checkArgs(args);

            Locale inputLocale = Locale.forLanguageTag(args[0]);
            Locale outputLocale = Locale.forLanguageTag(args[1]);
            Path inputFile = Path.of(args[2]);
            Path outputFile = Path.of(args[3]);
            ReadWriteFile workerFile = new ReadWriteFile(inputFile, outputFile);
            String text = workerFile.GetInput();
            AllStat stat = new AllStat(inputLocale, outputLocale, text);
            String statistic = stat.getReport();
            workerFile.SetOutput(statistic);
        } catch (IllegalArgumentException e) {
            System.err.println("Invalid arguments: " + e.getMessage());
        } catch (Exception e) {
            System.err.println(e.getMessage());
        }
    }

    private static void checkArgs(String[] args) {
        if (args.length != 4) {
            throw new IllegalArgumentException("Exactly 4 arguments are required: inputLocale, outputLocale, inputFile, outputFile");
        }
        if (isInValidLocale(args[0]) || isInValidLocale(args[1])) {
            throw new IllegalArgumentException("Invalid input locale");
        }

        Path inputFile = Path.of(args[2]);
        Path outputFile = Path.of(args[3]);
        var isValidInputFile = Files.exists(inputFile) && Files.isReadable(inputFile); 
        if (!isValidInputFile || (!Files.exists(outputFile) && !Files.isWritable(outputFile))) {
            throw new IllegalArgumentException("Error in input or output file");
        }
    }

    private static boolean isInValidLocale(String localeStr) {
        try {
            Locale locale = Locale.forLanguageTag(localeStr);
            return locale == null || locale.toString().isEmpty();
        } catch (Exception e) {
            return true;
        }
    }
}
