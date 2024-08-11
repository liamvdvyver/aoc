package org.day_02;

import java.util.Map;
import java.util.HashMap;

public class RoundPartTwo extends Round {

    private static final Map<String, Outcome> outcomeMap = new HashMap<>();

    static {
        outcomeMap.put("X", Outcome.LOSS);
        outcomeMap.put("Y", Outcome.DRAW);
        outcomeMap.put("Z", Outcome.WIN);
    }

    private static Outcome getOutcome(String str) {return outcomeMap.get(str);}

    public RoundPartTwo(String strategyLine) {
        String[] words = strategyLine.split(" ");
        this.opponent = new Play(words[0]);
        Outcome goal = getOutcome(words[1]);
        switch(goal) {
            case Outcome.LOSS -> {this.own = new Play(opponent.beats());}
            case Outcome.WIN -> {this.own = new Play(opponent.beatenBy());}
            case Outcome.DRAW -> {this.own = new Play(opponent.shape);}
        }

    }
}
