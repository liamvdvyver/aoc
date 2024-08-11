package org.day_02;

import java.util.HashMap;
import java.util.Map;

class Play {

    public static enum Shape {
        ROCK,
        PAPER,
        SCISSORS
    };

    private static final Map<String, Shape> parseHandMap = new HashMap<>();

    static {
        parseHandMap.put("A", Shape.ROCK);
        parseHandMap.put("B", Shape.PAPER);
        parseHandMap.put("C", Shape.SCISSORS);
        parseHandMap.put("X", Shape.ROCK);
        parseHandMap.put("Y", Shape.PAPER);
        parseHandMap.put("Z", Shape.SCISSORS);
    }

    private static final Map<Shape, Shape> beatenByMap = new HashMap<>();

    static {
        beatenByMap.put(Shape.ROCK, Shape.PAPER);
        beatenByMap.put(Shape.PAPER, Shape.SCISSORS);
        beatenByMap.put(Shape.SCISSORS, Shape.ROCK);
    }

    private static final Map<Shape, Shape> beatsMap = new HashMap<>();

    static {
        beatsMap.put(Shape.ROCK, Shape.SCISSORS);
        beatsMap.put(Shape.PAPER, Shape.ROCK);
        beatsMap.put(Shape.SCISSORS, Shape.PAPER);
    }

    private static final Map<Shape, Integer> shapeScoreMap = new HashMap<>();

    static {
        shapeScoreMap.put(Shape.ROCK, 1);
        shapeScoreMap.put(Shape.PAPER, 2);
        shapeScoreMap.put(Shape.SCISSORS, 3);
    }

    public static Shape parseShape(String str) {
        return parseHandMap.get(str);
    }

    public Shape shape;

    public Play(String str) {
        this(parseShape(str));
    }

    public Play(Shape shape) {
        this.shape = shape;
    }

    public Shape beatenBy() {
        return beatenByMap.get(shape);
    }

    public Shape beats() {
        return beatsMap.get(shape);
    }

    public boolean isBeatenBy(Play opponent) {
        return beatenBy() == opponent.shape;
    }

    public boolean doesBeat(Play opponent) {
        return beats() == opponent.shape;
    }

    public Integer getShapeScore() {
        return shapeScoreMap.get(shape);
    }
}
