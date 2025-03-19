package info.kgeorgiy.ja.korshunov.exam;

import info.kgeorgiy.ja.korshunov.i18n.statistics.AbstractStats;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.stream.Stream;

// есть скрипт encoding.sh в scripts
// 1) не успел написать rmdir. Написал бы через walkFileTree, предварительно реализовав FileVisitor
// 3) Не успел написать тесты
public class FileManager {
    Path curDir;
    static ResourceBundle curBundle;

    public FileManager(Path start, ResourceBundle bundle) {
        this.curDir = start;
        curBundle = bundle;
    }

    private static class Utilities {

        public static ResourceBundle getBundle(Locale outputLocale) {
            String packageName = FileManager.class.getPackageName();
            int lastDotIndex = packageName.lastIndexOf('.');
            String parentPackage = packageName.substring(0, lastDotIndex);
            String baseName = parentPackage + ".bundles.bundle_" + outputLocale;
            return ResourceBundle.getBundle(baseName, outputLocale);
        }

        public static void writeMessage(Path path, String message) {
            // :NOTE: * PrintStream плохо
            try (PrintStream writer = (path != null) ? new PrintStream(path.toFile(), StandardCharsets.UTF_8) : System.out) {
                writer.println(message);
            } catch (IOException e) {
                throw new RuntimeException("Error writing to error output", e);
            }
        }

        public static boolean isCorrectPath(Path path, Path err, boolean isDir) {
            if (Files.exists(path)) {
                if (isDir && !Files.isDirectory(path)) {
                    writeMessage(err, curBundle.getString("path.not.directory"));
                    return false;
                }
                return true;
            } else {
                writeMessage(err, curBundle.getString("path.not.exist"));
                return false;
            }
        }
    }

    private void dir(Path out, Path err) {
        if (Utilities.isCorrectPath(curDir, err, true)) {
            try (PrintStream writer = new PrintStream(Files.newOutputStream(out), true, StandardCharsets.UTF_8);
                 Stream<Path> stream = Files.list(curDir)) {
                stream.forEach(entry -> writer.println((Files.isDirectory(entry) ? "[DIR] " : "") + entry.getFileName()));
            } catch (IOException e) {
                Utilities.writeMessage(err, curBundle.getString("io.exception.read"));
            }
        }
    }

    private void cd(Path to, Path out, Path err) {
        Path origin = curDir.resolve(to).normalize();
        if (Utilities.isCorrectPath(origin, err, true)) {
            curDir = origin;
            Utilities.writeMessage(out, curBundle.getString("directory.success.change"));
        } else {
            Utilities.writeMessage(err, curBundle.getString("some.error"));
        }
    }

    private void create(String name, Path out, Path err) {
        Path origin = curDir.resolve(name).normalize();
        if (Utilities.isCorrectPath(curDir, err, true)) {
            try {
                Files.createFile(origin);
                Utilities.writeMessage(out, curBundle.getString("file.success.create"));
            } catch (SecurityException e) {
                Utilities.writeMessage(err, curBundle.getString("security.exception"));
            } catch (IOException e) {
                Utilities.writeMessage(err, curBundle.getString("io.exception.create"));
            }
        }
    }

    private void del(String name, Path out, Path err) {
        Path origin = curDir.resolve(name).normalize();
        try {
            if (Files.deleteIfExists(origin)) {
                Utilities.writeMessage(out, curBundle.getString("file.deleted"));
            } else {
                Utilities.writeMessage(err, curBundle.getString("file.not.exist"));
            }
        } catch (SecurityException e) {
            Utilities.writeMessage(err, curBundle.getString("security.exception"));
        } catch (IOException e) {
            Utilities.writeMessage(err, curBundle.getString("io.exception.delete"));
        }
    }

    private void cat(Path forRead, Path out, Path err) {
        Path origin = curDir.resolve(forRead).normalize();
        try {
            List<String> allLines = Files.readAllLines(origin, StandardCharsets.UTF_8); // предполагаю, что кодировка UTF-8 и файл не слишком большой
            Utilities.writeMessage(out, String.join(System.lineSeparator(), allLines));
        } catch (IOException e) {
            Utilities.writeMessage(err, curBundle.getString("file.read.error"));
        }
    }

    private void mkdir(Path create, Path out, Path err) {
        Path newDirPath = curDir.resolve(create).normalize();
        if (Files.exists(newDirPath)) {
            Utilities.writeMessage(err, curBundle.getString("directory.already.exist"));
            return;
        }

        try {
            Files.createDirectory(newDirPath);
            Utilities.writeMessage(out, curBundle.getString("directory.success.create"));
        } catch (IOException e) {
            Utilities.writeMessage(err, curBundle.getString("io.exception.directory"));
        }
    }

    // :NOTE: * плохо что не сделанно, но ладно
    private void rmdir() {
        System.out.println("Метод не реализован");
    }
    // :NOTE: * демострацию хотелось бы да
    public static void main(String[] args) {

    }
}
