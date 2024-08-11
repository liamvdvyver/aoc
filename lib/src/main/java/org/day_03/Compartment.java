package org.day_03;

import java.util.HashSet;
import java.util.Set;

class Compartment {

    public Compartment(String itemNames) {
        this.itemNames = itemNames;
    }

    private String itemNames;
    private final Set<Item> itemSet = new HashSet<>();

    public Set<Item> getItems() {
        if (itemSet.isEmpty() && !itemNames.isEmpty()) {
            for (int i = 0; i < itemNames.length(); i++) {
                itemSet.add(Item.getItem(itemNames.charAt(i)));
            }
        }
        return itemSet;
    }
}
