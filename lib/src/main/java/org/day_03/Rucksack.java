package org.day_03;

import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

class Rucksack {

    public Rucksack(String itemNames) {
        this.itemNames = itemNames;
        this.leftCompartment = new Compartment(itemNames.substring(0, itemNames.length() / 2));
        this.rightCompartment =
                new Compartment(itemNames.substring(itemNames.length() / 2, itemNames.length()));
    }

    private String itemNames;

    private Compartment leftCompartment;

    private Compartment rightCompartment;

    public Compartment getAllItems() {
        return new Compartment(itemNames);
    }

    private Set<Item> getCommonItems() {
        Set<Item> commonItems = new HashSet<>(leftCompartment.getItems());
        return commonItems.stream()
                .filter((e) -> this.rightCompartment.getItems().contains(e))
                .collect(Collectors.toSet());
    }

    public Integer getScore() {
        return getCommonItems().stream().map(Item::getPriority).reduce(0, (a, b) -> a + b);
    }

    public static void main(String[] args) {
        Rucksack test = new Rucksack("vJrwpWtwJgWrhcsFMMfFFhFp");
        System.out.println(test.getScore());
    }
}
