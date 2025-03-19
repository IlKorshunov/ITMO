package info.kgeorgiy.ja.korshunov.i18n.tests;

import info.kgeorgiy.ja.korshunov.i18n.application.AllStat;
import info.kgeorgiy.ja.korshunov.i18n.application.ReadWriteFile;
import info.kgeorgiy.ja.korshunov.i18n.statistics.*;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

import org.junit.jupiter.api.*;
import static org.junit.jupiter.api.Assertions.*;

@DisplayName("Tests for text statistic")
public class Tests {

    private static Locale ruLocale;
    private static Locale enLocale;
    private static String testsName;

    @BeforeAll
    static void beforeAll() {
        ruLocale = new Locale("ru", "RU");
        enLocale = new Locale("en", "GB");
        String packageName = Tests.class.getPackageName();
        int lastDot = packageName.lastIndexOf(".");
        String parentPack = packageName.substring(0, lastDot);
        testsName = parentPack + ".files";
    }

    void simpleTest(AbstractStats<?, ?, ?> stat, List<Integer> expect) {
        List<String> input = Arrays.asList(
                "",
                "simple.",
                "simple simple.",
                "1234.",
                "1 2 3 4.",
                "£7, £14.",
                "14 March 2020."
        );

        for (int i = 0; i < input.size(); i++) {
            try {
                AbstractStats<?, ?, ?> newStat = stat.getClass()
                        .getConstructor(Locale.class, Locale.class, String.class)
                        .newInstance(enLocale, enLocale, input.get(i));
                newStat.count();
                int count = newStat.getCount();
                assertEquals(expect.get(i), count, String.format("Не совпадают значения в строке %s, объект: %s", input.get(i), newStat.getType()));
            } catch (Exception e) {
                fail("Ошибка при создании нового экземпляра объекта: " + e.getMessage());
            }
        }
    }

    @Test
    public void baseTesting() {
        Dates date = new Dates(enLocale, enLocale, "");
        Money money = new Money(enLocale, enLocale, "");
        Numbers number = new Numbers(enLocale, enLocale, "");
        Sentences sent = new Sentences(enLocale, enLocale, "");
        Words word = new Words(enLocale, enLocale, "");

        List<Integer> expectDate = new ArrayList<>(Arrays.asList(0, 0, 0, 0, 0, 0, 1));
        List<Integer> expectMoney = new ArrayList<>(Arrays.asList(0, 0, 0, 0, 0, 2, 0));
        List<Integer> expectNumbers = new ArrayList<>(Arrays.asList(0, 0, 0, 1, 4, 2, 2));
        List<Integer> expectSent = new ArrayList<>(Arrays.asList(0, 1, 1, 1, 1, 1, 1));
        List<Integer> expectWord = new ArrayList<>(Arrays.asList(0, 1, 2, 0, 0, 0, 1));

        simpleTest(date, expectDate);
        simpleTest(money, expectMoney);
        simpleTest(number, expectNumbers);
        simpleTest(sent, expectSent);
        simpleTest(word, expectWord);
    }

    //
    @Test
    public void input0() throws IOException {
        String input = """
                यहाँ एक उदाहरण है जिसमें देवनागरी में 2 वाक्य, 13 शब्द, 5 संख्याएँ, 1 तिथि और 1 मुद्रा है: आज 24 जनवरी 2024 है। मेरे पास ₹१२३.४५, ₹४५६.७८, ₹७८९.९०, २ किताबें और ३ पेन हैं।\s
                """.replace(System.lineSeparator(), "\n");
        runTest("input2.txt", "output2.txt", input, new Locale("hi", "IN"), new Locale("ru", "RU"));
    }

    @Test
    public void english() throws IOException {
        String input = "";
        runTest("english.txt", "expectEnglish.txt", input, new Locale("en", "GB"), new Locale("ru", "RU"));
    }

    @Test
    public void german() throws IOException {
        String input = "";
        runTest("german.txt", "expectGerman.txt", input, new Locale("de", "DE"), new Locale("ru", "RU"));
    }

    private void runTest(String inputFileName, String outputFileName, String expectedText, Locale inputLocale, Locale outputLocale) throws IOException {
        final String inputPath = getPathToFile(inputFileName);
        final String outputPath = getPathToFile(outputFileName);
        ReadWriteFile workerFile = new ReadWriteFile(Path.of(inputPath), Path.of(outputPath));
        String text = workerFile.GetInput().replace(System.lineSeparator(), "\n");
        //assertEquals(expectedText, text, String.format("The texts from the files do not match. expected : %s, actual : %s", expectedText, text));

        AllStat stat = new AllStat(inputLocale, outputLocale, text);
        String toWrite = stat.getReport();
        workerFile.SetOutput(toWrite);

        String expect = Files.readString(Path.of(outputPath)).replace(System.lineSeparator(), "\n").trim();
        String actual = Files.readString(Path.of(outputPath)).replace(System.lineSeparator(), "\n").trim();

        assertEquals(expect, actual, String.format("Не совпадают отчеты %s", expect));
    }

    private String getPathToFile(String fileName) {
        try {
            Path path = Paths.get("java-advanced", "java-solutions", "info", "kgeorgiy", "ja", "korshunov", "i18n", "files", fileName);
            return path.toString();
        } catch (Exception e) {
            throw new RuntimeException("Ошибка при получении пути к файлу: " + fileName, e);
        }
    }
}
