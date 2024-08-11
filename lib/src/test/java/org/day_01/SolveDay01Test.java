package org.day_01;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

public class SolveDay01Test {
    @Test
    void testSolvePartOne() {
        assertEquals(
                24000,
                SolveDay01.solvePartOne(
                        SolveDay01.parse(
                                SolveDay01.read(
                                        "src/main/resources/problems/day_01/01_partial.txt"))));
        assertEquals(
                71502,
                SolveDay01.solvePartOne(
                        SolveDay01.parse(
                                SolveDay01.read("src/main/resources/problems/day_01/01.txt"))));
    }

    @Test
    void testSolvePartTwo() {
        assertEquals(
                45000,
                SolveDay01.solvePartTwo(
                        SolveDay01.parse(
                                SolveDay01.read(
                                        "src/main/resources/problems/day_01/01_partial.txt"))));

        assertEquals(
                208191,
                SolveDay01.solvePartTwo(
                        SolveDay01.parse(
                                SolveDay01.read(
                                        "src/main/resources/problems/day_01/01.txt"))));
    }
}
