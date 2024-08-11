package org.day_04;

import org.util.Reader;

import java.util.List;

class SolveDay04 {

    public static long solvePartOne(List<String> input) {
        return input.stream().map(ln -> new ElfSeatGroup(ln)).filter(e -> e.isContained()).count();
    }

    public static long solvePartTwo(List<String> input) {
        return input.stream()
                .map(ln -> new ElfSeatGroup(ln))
                .filter(e -> e.isOverlapping())
                .count();
    }

    public static void main(String[] args) {

        String fullInput = "lib/src/main/resources/problems/day_04/04.txt";
        List<String> input = Reader.read(fullInput);

        System.out.println("Part one:");
        System.out.println(solvePartOne(input));

        System.out.println("Part two:");
        System.out.println(solvePartTwo(input));
    }
}
