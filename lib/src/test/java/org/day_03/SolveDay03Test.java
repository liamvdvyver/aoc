package org.day_03;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.util.Reader;

import java.util.List;

public class SolveDay03Test {

    static String partialPath = "src/main/resources/problems/day_03/03_partial.txt";
    static String fullPath = "src/main/resources/problems/day_03/03.txt";

    static List<String> partialTxt = Reader.read(partialPath);
    static List<String> fullTxt = Reader.read(fullPath);

    @Test
    void testSolvePartOne() {
        assertEquals(157, SolveDay03.solvePartOne(partialTxt));
        assertEquals(7795, SolveDay03.solvePartOne(fullTxt));
    }

    @Test
    void testSolvePartTwo() {
        assertEquals(70, SolveDay03.solvePartTwo(partialTxt));
        assertEquals(2703, SolveDay03.solvePartTwo(fullTxt));
    }
}
