package org.day_03;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

public class RucksackTest {
    @Test
    void testGetScore() {
        assertEquals(16, (new Rucksack("vJrwpWtwJgWrhcsFMMfFFhFp")).getScore());
        assertEquals(38, (new Rucksack("jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL")).getScore());
        assertEquals(42, (new Rucksack("PmmdzqPrVvPwwTWBwg")).getScore());
        assertEquals(22, (new Rucksack("wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn")).getScore());
        assertEquals(20, (new Rucksack("ttgJtRGJQctTZtZT")).getScore());
        assertEquals(19, (new Rucksack("CrZsJsPPZsGzwwsLwLmpwMDw")).getScore());
    }

}

