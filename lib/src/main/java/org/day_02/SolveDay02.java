package org.day_02;

import java.util.List;
import org.util.Reader;

public class SolveDay02 {

    public static Integer solvePartOne (List<String> input) {
        return input.stream().map(
            (ln) -> (new RoundPartOne(ln)).score()
        ).reduce(0, (a, b) -> a + b);
    }

    public static Integer solvePartTwo (List<String> input) {
        return input.stream().map(
            (ln) -> (new RoundPartTwo(ln)).score()
        ).reduce(0, (a, b) -> a + b);
    }

    public static void main(String[] args) {

        String fullInput = "lib/src/main/resources/problems/day_02/02.txt";
        List<String> input = Reader.read(fullInput);

        System.out.println("Part one:");
        System.out.println(solvePartOne(input));

        System.out.println("Part two:");
        System.out.println(solvePartTwo(input));
    }

}
