package org.day_02;

import java.util.HashMap;
import java.util.Map;

// abstract class Round {
class Round {

    public static enum Outcome {
        WIN,
        LOSS,
        DRAW
    }

    protected Play opponent;
    protected Play own;

    public Outcome getOutcome() {
        return own.doesBeat(opponent) ? Outcome.WIN : own.isBeatenBy(opponent) ? Outcome.LOSS : Outcome.DRAW;
    }

    private static final Map<Outcome, Integer> outcomeScoreMap = new HashMap<>();

    static {
        outcomeScoreMap.put(Outcome.WIN, 6);
        outcomeScoreMap.put(Outcome.DRAW, 3);
        outcomeScoreMap.put(Outcome.LOSS, 0);
    }

    private Integer getOutcomeScore() {
        return outcomeScoreMap.get(getOutcome());
    }

    public Integer score() {
        return getOutcomeScore() + own.getShapeScore();
    }

}
