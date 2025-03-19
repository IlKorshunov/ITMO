package info.kgeorgiy.ja.korshunov.implementor;

import info.kgeorgiy.java.advanced.implementor.Impler;
import info.kgeorgiy.java.advanced.implementor.ImplerException;
import info.kgeorgiy.java.advanced.implementor.JarImpler;
import java.io.*;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.zip.ZipEntry;

/**
 * The {@code Implementor} class provides functionality for automatic generation of implementations for interfaces and classes,
 * as well as for creating JAR files containing these implementations. The class implements the {@link Impler} and {@link JarImpler} interfaces.
 */

public class Implementor implements JarImpler {



    /**
     * The newline character used for formatting the generated code.
     */
    private static final String NEWLINE = System.lineSeparator();

    /**
     * An instance of the compiler used for compiling the generated classes.
     */
    myCompiler compiler = new myCompiler();

    // attempt to record imports
/*    private void writeImports(Class<?> token, BufferedWriter writer) throws IOException {
        var types = Arrays.stream(token.getMethods()).flatMap(method -> Arrays.stream(method.getParameterTypes())).distinct().collect(Collectors.toList());
        Arrays.stream(token.getMethods()).map(Method::getReturnType).distinct().forEach(types::add);
        var toImport = types.stream().filter(type -> !type.isPrimitive() && !type.getPackageName().equals("java.lang")).collect(Collectors.toSet());
        for (Class<?> type : toImport) {
            writer.write("import " + type.getCanonicalName() + ";" + NEWLINE);
        }
        writer.write(NEWLINE);
    }*/

    // to prevent copy-pasts
    /**
     * Checks the correctness of the arguments for generating an implementation.
     * The generation of the implementation cannot be performed for {@code final} or {@code private} classes.
     *
     * @param token The class or interface for which to generate the implementation.
     * @param root The path to the root directory for saving the implementation.
     * @throws ImplerException If the arguments are incorrect or generation is not possible.
     */
    private void check(Class<?> token, Path root) throws ImplerException {
        if (token == null || root == null) {
            throw new ImplerException("Token and root should not be null.");
        }
        if (Modifier.isFinal(token.getModifiers()) || Modifier.isPrivate(token.getModifiers())) {
            throw new ImplerException("Cannot implement final or private class: " + token.getCanonicalName());
        }
    }

    // hw 4
    /**
     * Generates and writes the definition of the class implementing the specified interface or class.
     * Includes the package declaration, class declaration, and implementations of all methods.
     *
     * @param token The class or interface for which the implementation is generated.
     * @param writer {@link BufferedWriter} used for writing code.
     * @throws IOException If an I/O error occurs during writing.
     */
    private void writeClass(Class<?> token, BufferedWriter writer) throws IOException {
        String package_name = token.getPackageName();
        if (!package_name.isEmpty()) {
            writer.write("package" + " " + package_name + ";" + NEWLINE);
        }
        //writeImports(token, writer);
        writer.write("public class " + escapeSymbols(token.getSimpleName()) + "Impl " + "implements " + escapeSymbols(token.getCanonicalName()) + " {" + NEWLINE);
        for (Method method : token.getMethods()) {
            writeMethod(method, writer);
        }
        writer.write("}" + NEWLINE);
    }

    /**
     * Generates and writes the implementation of a method.
     * Includes the method declaration and its body, which returns a default value.
     *
     * @param method The method for which the implementation is generated.
     * @param writer {@link BufferedWriter} used for writing code.
     * @throws IOException If an I/O error occurs during writing.
     */
    private void writeMethod(Method method, BufferedWriter writer) throws IOException {
        Class<?>[] types = method.getParameterTypes();
        String parameters = IntStream.range(0, types.length).mapToObj(i -> String.format("%s arg%d", types[i].getCanonicalName(), i)).collect(Collectors.joining(", "));
        String returnType = method.getReturnType().getCanonicalName();
        String methodName = method.getName();
        String for_write = "public " + returnType + " " + methodName + "(" + parameters + ") " + "{" + NEWLINE + "return " + getReturnStatement(method.getReturnType()) + "}" + NEWLINE;
        writer.write(for_write);
    }

    /**
     * Generates a return value string for a method based on its return type.
     * Supports returning default values for primitive types and {@code null} for reference types.
     *
     * @param returnType The return type of the method.
     * @return A string with the code for the method's return value.
     */
    private String getReturnStatement(Class<?> returnType) {
        if (returnType.equals(Void.TYPE)) {
            return ";";
        } else if (returnType.equals(boolean.class)) {
            return "false;" + NEWLINE;
        } else if (returnType.isPrimitive()) {
            return "0;" + NEWLINE;
        } else {
            return "null;" + NEWLINE;
        }
    }

    /**
     * Checks the input arguments for correctness and generates an implementation for the specified class or interface.
     *
     * @param token The type for which an implementation needs to be generated.
     * @param root The root directory where the generated implementation will be saved.
     * @throws ImplerException If the input arguments are incorrect, or if generating the implementation is impossible.
     */
    @Override
    public void implement(Class<?> token, Path root) throws ImplerException {
        check(token, root);
        try {
            Path outputFile = root.resolve(token.getPackage().getName().replace('.', File.separatorChar)).resolve(token.getSimpleName() + "Impl.java");
            Files.createDirectories(outputFile.getParent().toAbsolutePath());
            try (BufferedWriter writer = Files.newBufferedWriter(outputFile)) {
                writeClass(token, writer);
            }
        } catch (IOException e) {
            throw new ImplerException("Error accessing file system: " + e.getMessage());
        }
    }

    ////// HW 5
    /**
     * Converts characters of a string to their Unicode escaped representation.
     * <p>
     * For each character of the string whose value exceeds 128, its hexadecimal
     * Unicode representation is generated, preceded by {@code \\u}. For all other characters,
     * their usual representation is used. This can be useful for generating string literals
     * containing non-ASCII characters in Java source code.
     *
     * @param input The original string to be converted.
     * @return A string where all characters exceeding 128 are replaced with their Unicode escaped representations.
     */
    public static String escapeSymbols(String input) {
        StringBuilder escaped = new StringBuilder();
        for (char c : input.toCharArray()) {
            if (c >= 128) {
                StringBuilder hexValue = new StringBuilder(Integer.toHexString(c));
                while (hexValue.length() < 4) {
                    hexValue.insert(0, "0");
                }
                escaped.append("\\u").append(hexValue);
            } else {
                escaped.append(c);
            }
        }
        return escaped.toString();
    }

    /**
     * Generates a JAR file containing the compiled class with the implementation for the specified type.
     *
     * @param token The type for which the implementation is generated.
     * @param jarFile The path to the JAR file where the implementation will be saved.
     * @throws ImplerException If the JAR file generation is not possible.
     */
    @Override
    public void implementJar(Class<?> token, Path jarFile) throws ImplerException {
        Path tempDir;
        try {
            if (!Files.exists(jarFile.getParent())){
                throw new ImplerException("The file does not exist");
            }
            tempDir = Files.createTempDirectory(jarFile.getParent(), "implementor");
            check(token, jarFile);
            implement(token, tempDir);
            compiler.compile(tempDir, token);
            //convertJar(token, jarFile, tempDir);
            convertJar(jarFile, tempDir, token);
        } catch (IOException e) {
            throw new ImplerException("Error accessing file system: " + e.getMessage());
        }
    }

    // 1-st solution
    /**
     * Creates a JAR file for the compiled classes.
     * <p>
     * This method automatically generates a manifest file for the JAR file specifying the main class. Then,
     * it uses the {@code jar} command-line tool to assemble the JAR file that includes all the compiled
     * classes from the temporary directory.
     *
     * @param token The type for which the implementation is generated.
     * @param jarFile The path to the JAR file being created.
     * @param tempDir The temporary directory with the compiled classes.
     * @throws ImplerException if the creation of the manifest file or JAR file fails.
     */
    private void convertJar(Path jarFile, Path tempDir, Class<?> token) throws ImplerException {
        var manifest = new Manifest();
        try (JarOutputStream jarOutputStream = new JarOutputStream(Files.newOutputStream(jarFile), manifest)) {
            var className = myCompiler.getImplName(token);
            jarOutputStream.putNextEntry(new ZipEntry(className.replace(".", "/") + ".class"));
            jarOutputStream.write(Files.readAllBytes(tempDir.resolve(className.replace(".", File.separator) + ".class")));
            jarOutputStream.closeEntry();
        } catch (IOException e) {
            throw new ImplerException("asdf", e);
        }
/*        Path manifestFile = tempDir.resolve("MANIFEST.MF");
        try (BufferedWriter writer = Files.newBufferedWriter(manifestFile)) {
            writer.write("Manifest-Version: 1.0");
            writer.newLine();
            writer.write("Main-Class: " + "info.kgeorgiy.ja.korshunov.implementor.Implementor");
        } catch (IOException e) {
            throw new ImplerException("Can't create Manifest", e);
        }
        try {
            ProcessBuilder processBuilder = new ProcessBuilder("jar", "cfm", jarFile.toString(), manifestFile.toString(), "-C", tempDir.toString(), ".");
            // :NOTE: something is here
            Process process = processBuilder.start();
            if (process.waitFor() != 0) {
                throw new ImplerException("Error creating .jar file");
            }
        } catch (IOException | InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new ImplerException("Error during execution of external command", e);
        }*/
    }

    /**
     * Generates an implementation for the specified class or interface and saves the result in the specified directory,
     * or creates a JAR file with the generated implementation, depending on the number of arguments.
     * Accepts the following command line arguments:
     *
     * @param args the command line arguments passed to the program.
     * @throws ClassNotFoundException if the class specified in {@code args[0]} cannot be found.
     * @throws ImplerException if an error occurs during the generation of the implementation or creation of the JAR file.
     */
    public static void main(String[] args) throws ClassNotFoundException, ImplerException {
        switch (args.length) {
            case 2:
                new Implementor().implement(Class.forName(args[0]), Paths.get(args[1]));
                break;
            case 3:
                new Implementor().implementJar(Class.forName(args[1]), Paths.get(args[2]));
                break;
            default:
                System.out.println("Usage: java Main <class-name> <output-path> [jar]");
                break;
        }
    }

}
