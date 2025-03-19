package info.kgeorgiy.ja.korshunov.TODO;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class TODOListTest {
    static Path in = Path.of("C:\\Users\\Ilya\\OneDrive\\Рабочий стол\\java 4 term\\java-advanced\\java-solutions\\info\\kgeorgiy\\ja\\korshunov\\TODO\\input.txt");
    static Path out = Path.of("C:\\Users\\Ilya\\OneDrive\\Рабочий стол\\java 4 term\\java-advanced\\java-solutions\\info\\kgeorgiy\\ja\\korshunov\\TODO\\output.txt");
    static Path expect = Path.of("C:\\Users\\Ilya\\OneDrive\\Рабочий стол\\java 4 term\\java-advanced\\java-solutions\\info\\kgeorgiy\\ja\\korshunov\\TODO\\expectOut.txt");

    @BeforeEach
    void setUp() {
        TODOList.label2mark.clear();
        TODOList.label2task.clear();
    }

    @Test
    void testSimple() throws IOException {
        TODOList.getTasks(in);
        TODOList.writeTask(out, TODOList.Marks.all);
        assertFilesEqual(out, expect);
    }

    private void assertFilesEqual(Path file1, Path file2) throws IOException {
        List<String> file1Lines = Files.readAllLines(file1);
        List<String> file2Lines = Files.readAllLines(file2);

        assertEquals(file2Lines, file1Lines, "Files are not equal");
    }
}
