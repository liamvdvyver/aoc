package org.day_02;

public class RoundPartOne extends Round {

    public RoundPartOne(String strategyLine) {
        String[] words = strategyLine.split(" ");
        this.opponent = new Play(words[0]);
        this.own = new Play(words[1]);
    }

}
