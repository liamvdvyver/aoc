package org.day_04;

import java.util.Arrays;
import java.util.List;
import java.util.function.BiPredicate;
import java.util.stream.Collectors;

class ElfSeatGroup {

    private List<ElfSection> sections;

    public ElfSeatGroup(List<ElfSection> sections) {
        this.sections = sections;
    }

    public ElfSeatGroup(String inputLn) {
        this.sections =
                Arrays.stream(inputLn.split(","))
                        .map(s -> new ElfSection(s))
                        .collect(Collectors.toList());
    }

    private boolean anyPair(BiPredicate<ElfSection, ElfSection> pred) {
        for (int i = 0; i < sections.size(); i++) {
            for (int j = 0; j < sections.size(); j++) {
                if (j == i) continue;
                ElfSection fst = sections.get(i);
                ElfSection snd = sections.get(j);
                if (pred.test(fst, snd)) {
                    return true;
                }
            }
        }
        return false;
    }

    public boolean isContained() {
        return anyPair((fst, snd) -> fst.lower >= snd.lower && fst.upper <= snd.upper);
    }

    public boolean isOverlapping() {
        return anyPair(
                (fst, snd) ->
                        (fst.lower <= snd.lower && snd.lower <= fst.upper
                                || fst.lower <= snd.upper && snd.upper <= fst.upper));
    }
}
