package org.day_04;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

public class ElfSeatGroupTest {
    @Test
    void testIsContained() {
        assertTrue((new ElfSeatGroup("2-8,3-7")).isContained());
        assertTrue((new ElfSeatGroup("6-6,4-6")).isContained());
        assertTrue((new ElfSeatGroup("1-5,1-5")).isContained());
        assertFalse((new ElfSeatGroup("1-3,4-5")).isContained());
        assertFalse((new ElfSeatGroup("1-3,2-4")).isContained());
    }

    @Test
    void testIsOverlapping() {
        assertFalse((new ElfSeatGroup("1-2,3-4")).isOverlapping());
        assertTrue((new ElfSeatGroup("1-2,2-3")).isOverlapping());
        assertTrue((new ElfSeatGroup("2-3,1-2")).isOverlapping());
        assertTrue((new ElfSeatGroup("1-4,2-3")).isOverlapping());
        assertTrue((new ElfSeatGroup("2-3,1-4")).isOverlapping());
    }
}

