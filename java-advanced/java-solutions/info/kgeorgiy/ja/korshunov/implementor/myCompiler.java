package info.kgeorgiy.ja.korshunov.implementor;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import java.io.File;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;

/**
 * Класс предоставляет методы для компиляции сгенерированных реализаций интерфейсов или классов.
 * Включает в себя методы для получения полного имени класса реализации, получения пути к файлу с исходным кодом
 * и непосредственно компиляции файла.
 */
public class myCompiler {
    /**
     * Возвращает полное имя класса реализации.
     *
     * @param token класс или интерфейс, для которого генерируется реализация
     * @return строка с полным именем класса реализации
     */
    public static String getImplName(final Class<?> token) {
        return token.getPackageName() + "." + token.getSimpleName() + "Impl";
    }

    /**
     * Получает путь к файлу с исходным кодом реализации.
     *
     * @param root корневой путь, где должен быть расположен файл
     * @param clazz класс или интерфейс, для которого генерируется реализация
     * @return {@link Path} объект, указывающий путь к файлу исходного кода
     */
    public static Path getFile(final Path root, final Class<?> clazz) {
        return root.resolve(getImplName(clazz).replace(".", File.separator) + ".java").toAbsolutePath();
    }

    /**
     * Осуществляет компиляцию файла исходного кода.
     *
     * @param root корневой путь, где расположен файл
     * @param file строка, содержащая путь к файлу исходного кода
     * @param token класс или интерфейс, для которого выполняется компиляция
     */
    private static void compile(final Path root, final String file, final Class<?> token) {
        final JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        final String classpath = root + File.pathSeparator + getClassPath(token);
        final String[] args = {"-cp", classpath, "-encoding", StandardCharsets.UTF_8.name(), file};
        compiler.run(null, null, null, args);
    }

    /**
     * Возвращает путь класса в файловой системе.
     *
     * @param token класс, путь которого необходимо получить
     * @return строка, представляющая путь класса в файловой системе
     * @throws AssertionError если не удалось преобразовать URL в URI
     */
    private static String getClassPath(final Class<?> token) {
        try {
            return Path.of(token.getProtectionDomain().getCodeSource().getLocation().toURI()).toString();
        } catch (final URISyntaxException e) {
            throw new AssertionError(e);
        }
    }

    /**
     * Компилирует файл исходного кода для заданного класса или интерфейса.
     *
     * @param root корневой путь, где должен быть расположен файл
     * @param clazz класс или интерфейс, для которого выполняется компиляция
     */
    public void compile(final Path root, final Class<?> clazz) {
        final String file = getFile(root, clazz).toString();
        compile(root, file, clazz);
    }
}
