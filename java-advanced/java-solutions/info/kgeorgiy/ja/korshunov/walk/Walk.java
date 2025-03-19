package info.kgeorgiy.ja.korshunov;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;


public class Walk {
    private static final int BUFFER_SIZE = 8192;
    public static String hash(final byte[] bytes) {
        int hash = 0;
        if (bytes != null) {
            for (final byte b : bytes) {
                hash += b & 0xff;
                hash += hash << 10;
                hash ^= hash >>> 6;
            }
            hash += hash << 3;
            hash ^= hash >>> 11;
            hash += hash << 15;
        }
        return String.format("%08x", hash);
    }

    public static void walk(String in, String out) throws IOException {
        try  (BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(out), StandardCharsets.UTF_8));
        BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream(in), StandardCharsets.UTF_8))){
            String filePath;
            while ((filePath = reader.readLine()) != null) {
                try {
                    byte[] fileContent = Files.readAllBytes(Paths.get(filePath));
                    String fileHash = hash(fileContent);
                    writer.write(fileHash + " " + filePath);
                    writer.newLine();
                } catch (FileNotFoundException e) {
                    System.out.println("Не могу найти файл");
                } catch (InterruptedIOException e){
                    String errorHash = "0".repeat(8);
                    writer.write(errorHash + " " + filePath);
                    writer.newLine();
                    System.out.println("Поток записи был прерван");
                } catch (OutOfMemoryError e){
                    String errorHash = "0".repeat(8);
                    writer.write(errorHash + " " + filePath);
                    writer.newLine();
                    System.out.println("Переполнение памяти");
                } catch (IOException e) {
                    System.out.println("IOException при записи");
                    e.printStackTrace();
                }
            }
        } catch (UnsupportedEncodingException e) {
            System.out.println("Не поддерживается кодировка");
        } catch (OutOfMemoryError e) {
            System.out.println("Переполнение памяти");
        } catch (FileNotFoundException e) {
            System.out.println("Указанный файл не удается найти");
        } catch (IOException e) {
            System.out.println("IOException при чтении");
            e.printStackTrace();
        }
    }
    public static void main(String[] args) throws IOException {
        if (args == null || args.length != 2 || args[0] == null || args[1] == null) {
            System.out.println("Проблема с аргументами");
        } else {
            walk(args[0], args[1]);
        }

    }
}
