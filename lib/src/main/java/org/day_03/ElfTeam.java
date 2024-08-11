package org.day_03;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

class ElfTeam {

    List<Rucksack> rucksacks;

    public static class ElfTeamBuilder {

        public ElfTeamBuilder() {}

        private List<Rucksack> rucksacks = new ArrayList<>();

        public ElfTeamBuilder add(String itemNames) {
            rucksacks.add(new Rucksack(itemNames));
            return this;
        }

        public ElfTeamBuilder add(Rucksack rucksack) {
            rucksacks.add(rucksack);
            return this;
        }

        public ElfTeam build() {
            List<Rucksack> ret = rucksacks;
            this.rucksacks = new ArrayList<>();
            return new ElfTeam(ret);
        }
    }

    private ElfTeam(List<Rucksack> rucksacks) {
        this.rucksacks = rucksacks;
    }

    private Map<Item, Integer> getItemFrequency() {
        Map<Item, Integer> freqMap = new HashMap<>();
        for (Rucksack r : rucksacks) {
            for (Item i : r.getAllItems().getItems()) {
                freqMap.computeIfPresent(i, (k, v) -> v + 1);
                freqMap.putIfAbsent(i, 1);
            }
        }
        return freqMap;
    }

    public Set<Item> getCommonItems() {
        return getItemFrequency().entrySet().stream()
                .filter(e -> e.getValue() == rucksacks.size())
                .map(e -> e.getKey())
                .collect(Collectors.toSet());
    }

    public Integer getScore() {
        Set<Item> items = getCommonItems();
        if (items.size() != 1) {
            throw new RuntimeException("Not a single common item");
        }
        return items.stream().map(Item::getPriority).reduce(0, (a, b) -> a + b);
    }
}
