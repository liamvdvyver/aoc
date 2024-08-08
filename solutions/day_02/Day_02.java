import java.util.HashMap;
import java.util.Scanner;

enum Shape {
    ROCK,
    PAPER,
    SCISSORS
};

enum Outcome {
    WIN,
    LOSS,
    DRAW
}

class Play {
    public Shape shape;

    static final HashMap<String, Shape> parseHandMap = new HashMap<String, Shape>();

    static {
        parseHandMap.put("A", Shape.ROCK);
        parseHandMap.put("B", Shape.PAPER);
        parseHandMap.put("C", Shape.SCISSORS);
        parseHandMap.put("X", Shape.ROCK);
        parseHandMap.put("Y", Shape.PAPER);
        parseHandMap.put("Z", Shape.SCISSORS);
    }

    public Play (String str) {
        this.shape = parseHandMap.get(str);
    }

    // beats(A) = B if B beats A
    static HashMap<Shape, Shape> beatsMap = new HashMap<Shape, Shape>();

    static {
        beatsMap.put(Shape.ROCK, Shape.PAPER);
        beatsMap.put(Shape.PAPER, Shape.SCISSORS);
        beatsMap.put(Shape.SCISSORS, Shape.ROCK);
    }

    public Boolean beats(Play opponent) {
        return (beatsMap.get(opponent.shape) == this.shape);
    }

    static HashMap<Shape, Integer> shapeScoreMap = new HashMap<Shape, Integer>();

    static {
        shapeScoreMap.put(Shape.ROCK, 1);
        shapeScoreMap.put(Shape.PAPER, 2);
        shapeScoreMap.put(Shape.SCISSORS, 3);
    }

    public Integer shapeScore() {
        return shapeScoreMap.get(this.shape);
    }
}

class Round {
    public Play opponent;
    public Play own;
    public Outcome goal;

    public Round(Play opponent, Play own) {
        this.opponent = opponent;
        this.own = own;
    }

    public Round() {}

    public Outcome outcome() {
        return own.beats(opponent)
                ? Outcome.WIN
                : opponent.beats(own) ? Outcome.LOSS : Outcome.DRAW;
    }

    private static HashMap<Outcome, Integer> outcomeScoreMap = new HashMap<Outcome, Integer>();
    static {
        outcomeScoreMap.put(Outcome.WIN, 6);
        outcomeScoreMap.put(Outcome.DRAW, 3);
        outcomeScoreMap.put(Outcome.LOSS, 0);
    };

    private Integer outcomeScore() {
        return outcomeScoreMap.get(this.outcome());
    };

    public Integer score() {
        return this.outcomeScore() + this.own.shapeScore();
    }

    public void parseGuideLine(String line) {
        String[] words = line.split(" ");
        this.opponent = new Play(words[0]);
        this.own = new Play(words[1]);
    };

    // Part 2
    private static HashMap<String, Outcome> outcomeMap = new HashMap<String, Outcome>();
    static {
        outcomeMap.put("X", Outcome.LOSS);
        outcomeMap.put("Y", Outcome.DRAW);
        outcomeMap.put("Z", Outcome.WIN);
    };

    public void parseGuideLine2(String line) {
        String[] words = line.split(" ");
        this.opponent = new Play(words[0]);
        this.goal = outcomeMap.get(words[1]);
    };

    public static HashMap<Shape, Shape> winMap = new HashMap<Shape, Shape>();
    static {
        winMap.put(Shape.SCISSORS, Shape.ROCK);
        winMap.put(Shape.ROCK, Shape.PAPER);
        winMap.put(Shape.PAPER, Shape.SCISSORS);
    };

    public static HashMap<Shape, Shape> lossMap = new HashMap<Shape, Shape>();
    static {
        lossMap.put(Shape.SCISSORS, Shape.PAPER);
        lossMap.put(Shape.ROCK, Shape.SCISSORS);
        lossMap.put(Shape.PAPER, Shape.ROCK);
    };

    public static HashMap<Shape, Shape> drawMap = new HashMap<Shape, Shape>();
    static {
        lossMap.put(Shape.SCISSORS, Shape.SCISSORS);
        lossMap.put(Shape.ROCK, Shape.ROCK);
        lossMap.put(Shape.PAPER, Shape.PAPER);
    };

    private static HashMap<Outcome, HashMap<Shape, Shape>> moveMap = new HashMap<Outcome, HashMap<Shape, Shape>>();
    {
        moveMap.put(Outcome.LOSS, lossMap);
        moveMap.put(Outcome.WIN, winMap);
        moveMap.put(Outcome.DRAW, drawMap);
    };

    public int score2() {

        this.own = moveMap.get(this.goal).get(this.opponent.shape);
        return 0;
    }
}

public class Day_02 {

    public static void main(String[] args) {

        Integer score = 0;
        Integer score2 = 0;

        Scanner in = new Scanner(System.in);

        while (in.hasNextLine()) {

            String ln = in.nextLine();

            Round curRound = new Round();
            curRound.parseGuideLine(ln);
            score += curRound.score();

            Round curRound2 = new Round();
            curRound2.parseGuideLine2(ln);
            score += curRound.score();

        };

        in.close();

        System.out.print(String.valueOf(score));

    }
}
