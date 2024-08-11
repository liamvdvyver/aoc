package org.day_04;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.util.Reader;

import java.util.List;

public class SolveDay04Test {
    static String partialPath = "src/main/resources/problems/day_04/04_partial.txt";
    static String fullPath = "src/main/resources/problems/day_04/04.txt";

    static List<String> partialTxt = Reader.read(partialPath);
    static List<String> fullTxt = Reader.read(fullPath);

    @Test
    void testSolvePartOne() {
        assertEquals(2, SolveDay04.solvePartOne(partialTxt));
        assertEquals(464, SolveDay04.solvePartOne(fullTxt));
    }

    @Test
    void testSolvePartTwo() {
        assertEquals(4, SolveDay04.solvePartTwo(partialTxt));
    }
}
