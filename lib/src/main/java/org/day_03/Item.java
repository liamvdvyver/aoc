package org.day_03;

import java.util.HashMap;
import java.util.Map;

class Item {

    private Character name;

    private static final Map<Item, Integer> priorityMap = new HashMap<>();

    public static Integer priority(Item item) {

        String alphabet = "abcdefghijklmnopqrstuvwxyz";

        if (priorityMap.containsKey(item)) {
            return priorityMap.get(item);
        } else {
            Integer ret = alphabet.indexOf(Character.toLowerCase(item.getName())) + 1;
            if (Character.isUpperCase(item.getName())) ret += 26;
            priorityMap.put(item, ret);

            return ret;
        }
    }

    private static final Map<Character, Item> items = new HashMap<>();

    public Integer getPriority() {
        return priority(this);
    }

    public Character getName() {
        return name;
    }

    private Item(Character name) {
        this.name = name;
    }

    public static Item getItem(Character name) {

        if (!Character.isLetter(name)) throw new RuntimeException("Name is not a letter");

        if (items.containsKey(name)) {
            return items.get(name);
        } else {
            Item ret = new Item(name);
            items.put(name, ret);
            return ret;
        }
    }
    ;
}
