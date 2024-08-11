package org.day_04;

class ElfSection {

    Integer lower;
    Integer upper;

    public ElfSection(Integer lower, Integer upper) {
        this.lower = lower;
        this.upper = upper;
    }

    public ElfSection(String bounds) {
        String[] splitBounds = bounds.split("-");
        this.lower = Integer.parseInt(splitBounds[0]);
        this.upper = Integer.parseInt(splitBounds[1]);
    }
}
