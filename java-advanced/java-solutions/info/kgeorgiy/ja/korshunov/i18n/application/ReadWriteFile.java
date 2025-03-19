package info.kgeorgiy.ja.korshunov.i18n.application;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.regex.Pattern;

public class ReadWriteFile {
    private final Path in, out;
    private String input, output;

    private static final Pattern EMOJI_PATTERN = Pattern.compile(
            "[\\uD83C-\\uDBFF\\uDC00-\\uDFFF]+");

    public ReadWriteFile(Path in, Path out) {
        this.in = in;
        this.out = out;
    }

    private void readFromFile() {
        StringBuilder str = new StringBuilder();
        try (BufferedReader reader = Files.newBufferedReader(in, StandardCharsets.UTF_8)) {
            String line = reader.readLine();
            while (line != null) {
                str.append(line).append(System.lineSeparator());
                line = reader.readLine();
            }
        } catch (FileNotFoundException e) {
            throw new RuntimeException(e.getMessage());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        this.input = str.toString();

/*        if (containsEmoji(this.input)) {
            throw new RuntimeException("We don't support emoji.");
        }*/
    }

    private boolean containsEmoji(String text) {
        return EMOJI_PATTERN.matcher(text).find();
    }

    private void writeFile() {
        try (BufferedWriter writer = Files.newBufferedWriter(out, StandardCharsets.UTF_8)) {
            writer.write(output);
        } catch (IOException e) {
            throw new RuntimeException("Error writing file: " + e.getMessage(), e);
        }
    }

    public String GetInput() {
        readFromFile();
        return input;
    }

    public void SetOutput(String output) {
        this.output = output;
        writeFile();
    }
}
