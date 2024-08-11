package org.day_01;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Comparator;

public class SolveDay01 {

    public static ArrayList<ArrayList<Integer>> parse(Iterable<String> input) {

        ArrayList<ArrayList<Integer>> ret = new ArrayList<>();

        for (String s : input) {

            if (s.isEmpty()) {
                ret.addLast(new ArrayList<>());
            } else {
                if (ret.isEmpty()) {
                    ret.addLast(new ArrayList<>());
                }
                ret.getLast().add(Integer.parseInt(s));
            }
        }

        return ret;
    }

    public static Integer solvePartOne(ArrayList<ArrayList<Integer>> nums) {

        return nums.stream()
                .map((ints) -> ints.stream().reduce(0, (a, b) -> a + b))
                .reduce(0, (a, b) -> Math.max(a, b));
    }

    public static Integer solvePartTwo(ArrayList<ArrayList<Integer>> nums) {

        return nums.stream()
                .map((ints) -> ints.stream().reduce(0, (a, b) -> a + b))
                .sorted(Comparator.reverseOrder())
                .limit(3)
                .reduce(0, (a, b) -> a + b);
    }

    public static ArrayList<String> read(String filename) {

        ArrayList<String> input = new ArrayList<String>();

        try {
            BufferedReader inRaw = new BufferedReader(new FileReader(filename));

            String nextLine = inRaw.readLine();
            while (nextLine != null) {
                input.addLast(nextLine);
                nextLine = inRaw.readLine();
            }

            inRaw.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return input;
    }

    public static void main(String[] args) {

        System.out.println("Part one:");
        System.out.println(
                solvePartOne(parse(read("lib/src/main/resources/problems/day_01/01.txt"))));

        System.out.println("Part Two:");
        System.out.println(
                solvePartTwo(parse(read("lib/src/main/resources/problems/day_01/01.txt"))));
    }
}
