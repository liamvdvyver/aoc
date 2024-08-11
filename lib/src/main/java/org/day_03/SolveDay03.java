package org.day_03;

import org.day_03.ElfTeam.ElfTeamBuilder;
import org.util.Reader;

import java.util.ArrayList;
import java.util.List;

public class SolveDay03 {

    public static Integer solvePartOne(List<String> input) {
        return input.stream().map((ln) -> new Rucksack(ln).getScore()).reduce(0, (a, b) -> a + b);
    }

    public static Integer solvePartTwo(List<String> input) {

        List<ElfTeam> elfTeams = new ArrayList<>();
        ElfTeamBuilder elfBuilder = new ElfTeam.ElfTeamBuilder();

        for (int i = 0; i < input.size(); i++) {

            elfBuilder.add(input.get(i));

            if (i % 3 == 2) {
                elfTeams.add(elfBuilder.build());
            }
        }

        return elfTeams.stream().map(ElfTeam::getScore).reduce(0, (a, b) -> a + b);
    }

    public static void main(String[] args) {

        String fullInput = "lib/src/main/resources/problems/day_03/03.txt";
        List<String> input = Reader.read(fullInput);

        System.out.println("Part one:");
        System.out.println(solvePartOne(input));

        System.out.println("Part two:");
        System.out.println(solvePartTwo(input));
    }
    ;
}
