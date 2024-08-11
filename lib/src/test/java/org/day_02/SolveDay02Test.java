package org.day_02;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;

public class SolveDay02Test {

    static String fullPath = "src/main/resources/problems/day_02/02.txt";
    static String partialPath = "src/main/resources/problems/day_02/02_partial.txt";
    static List<String> fullText;
    static List<String> partialText;

    static {
        fullText = Reader.read(fullPath);
        partialText = Reader.read(partialPath);
    }

    @Test
    void testSolvePartOne() {
        assertEquals(15, SolveDay02.solvePartOne(partialText));
        assertEquals(13924, SolveDay02.solvePartOne(fullText));
    }

    @Test
    void testSolvePartTwo() {
        assertEquals(12, SolveDay02.solvePartTwo(partialText));
        assertEquals(13448, SolveDay02.solvePartTwo(fullText));
    }
}
