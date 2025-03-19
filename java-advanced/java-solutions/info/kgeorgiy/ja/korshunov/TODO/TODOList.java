package info.kgeorgiy.ja.korshunov.TODO;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Stream;

// TODO : 1) упростить логику writeTask
// TODO : 2) дописать побольше тестов
// TODO : 3) подумать над пробелами между ~~ и задачей. Рассмотреть случаи, когда они там есть, либо их нет
// TODO : 4) Было бы здорово добавить doneMark, чтобы тесты этого не учитывали
public class TODOList {
    static String doneMark = "✓";
    static Map<String, Marks> label2mark = new HashMap<>();
    static Map<String, String> label2task = new HashMap<>();
    enum Marks {complete, uncomplete, all}

    static void getTasks(Path in) {
        try (BufferedReader reader = Files.newBufferedReader(in, StandardCharsets.UTF_8)) {
            String curLine;
            while ((curLine = reader.readLine()) != null) {
                String[] get;
                boolean isUncomplete = curLine.startsWith("~~") && curLine.endsWith("~~");
                String trimmedLine = isUncomplete ? curLine.substring(2, curLine.length() - 2).trim() : curLine.trim();
                get = trimmedLine.split(":");
                if (get.length != 2) {
                    System.err.println("Invalid task format");
                } else {
                    label2mark.put(get[0].trim(), isUncomplete ? Marks.uncomplete : Marks.complete);
                    label2task.put(get[0].trim(), get[1].trim());
                }

            }
        } catch (IOException e) {
            System.err.println("error " + e.getMessage());
        }
    }

/*    static void getTasks(Path in) {
        try (Stream<String> lines = Files.lines(in, StandardCharsets.UTF_8)) { // поскольку в TODOList не должно быть много строк
        // то этот подход, наверное, можно использовать, по скольку он более приятен для чтения
            lines.forEach(curLine -> {
                boolean isUncomplete = curLine.startsWith("~~") && curLine.endsWith("~~");
                String trimmedLine = isUncomplete ? curLine.substring(2, curLine.length() - 2).trim() : curLine.trim();
                String[] get = trimmedLine.split(":");
                if (get.length != 2) {
                    System.err.println("Invalid task format");
                } else {
                    label2mark.put(get[0].trim(), isUncomplete ? Marks.uncomplete : Marks.complete);
                    label2task.put(get[0].trim(), get[1].trim());
                }
            });
        } catch (IOException e) {
            System.err.println("Error: " + e.getMessage());
        }
    }*/


    static void deleteTask(String label) {
        if (label2mark.remove(label) == null || label2task.remove(label) == null) {
            System.err.println("Task with label \"" + label + "\" does not exist");
        }
    }

    static void addTask(String label, String task, Marks mark) {
        if (label2task.containsKey(label)) {
            System.err.println("Task with label \"" + label + "\" already exists");
        }
        label2task.put(label, task);
        label2mark.put(label, mark);
    }

    static void markTask(String label, Marks mark) {
        if (!label2mark.containsKey(label)) {
            System.err.println("Task with label \"" + label + "\" does not exist");
        }
        label2mark.put(label, mark);
    }

    static void writeTask(Path out, Marks mark) {
        try (PrintStream writer = (out != null) ? new PrintStream(out.toFile(), StandardCharsets.UTF_8) : System.out) {
            label2task.forEach((curKey, value) -> {
                Marks curMark = label2mark.get(curKey);
                if (mark == Marks.all || curMark == mark) {
                    writer.printf(curMark == Marks.complete ? "%s : %s\n" : "~~ %s : %s ~~\n", curKey, value);
                }
            });
        } catch (FileNotFoundException e) {
            System.err.println("File not found: " + e.getMessage());
        } catch (UnsupportedEncodingException e) {
            System.err.println("Unsupported encoding: " + e.getMessage());
        } catch (IOException e) {
            System.err.println("Error: " + e.getMessage());
        }
    }

    public static void handleUserInput(Path in, Path out) {
        Scanner sc = new Scanner(System.in);

        System.out.println("Write 'exit' to quit.");

        while (true) {
            String input = sc.nextLine();
            if (input.equals("exit")) {
                break;
            }
            String[] curInput = input.split(" ");
            try {
                switch (curInput[0]) {
                    case "read" -> {
                        if (!checkArgsLength(curInput, 1)) break;
                        getTasks(in);
                    }
                    case "add" -> {
                        if (!checkArgsLength(curInput, 4, true)) break;
                        String label = curInput[1];
                        String mark = curInput[curInput.length - 1];
                        String task = String.join(" ", Arrays.copyOfRange(curInput, 2, curInput.length - 1));
                        addTask(label, task, Marks.valueOf(mark));
                    }
                    case "delete" -> {
                        if (!checkArgsLength(curInput, 2)) break;
                        deleteTask(curInput[1]);
                    }
                    case "mark" -> {
                        if (!checkArgsLength(curInput, 3)) break;
                        markTask(curInput[1], Marks.valueOf(curInput[2]));
                    }
                    case "write" -> {
                        if (!checkArgsLength(curInput, 2)) break;
                        writeTask(out, Marks.valueOf(curInput[1]));
                    }
                    default -> System.out.println("Unknown command: " + curInput[0]);
                }
            } catch (RuntimeException e) {
                System.err.println("Error: " + e.getMessage());
            }
        }

        sc.close();
    }

    private static boolean checkArgsLength(String[] args, int expectedLength) {
        return checkArgsLength(args, expectedLength, false);
    }

    private static boolean checkArgsLength(String[] args, int expectedLength, boolean allowGreater) {
        boolean valid = allowGreater ? args.length >= expectedLength : args.length == expectedLength;
        if (!valid) {
            System.err.printf("Usage: %s\n", args[0]);
        }
        return valid;
    }


    public static void main(String[] args) {
        Path in, out;
        in = Path.of(args[0]);
        out = Path.of(args[1]);
        handleUserInput(in, out);
    }
}