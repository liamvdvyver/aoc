import java.util.Scanner;

public class Day_01 {

    public static void main(String[] args) {

        long maxCount = 0, curCount = 0;

        Scanner in = new Scanner(System.in);

        while (in.hasNextLine()) {

            String ln = in.nextLine();

            if (ln == "") {

                maxCount = Math.max(curCount, maxCount);
                curCount = 0;

            } else {
                curCount += Integer.parseInt(ln);
            };
        };

        maxCount = Math.max(curCount, maxCount);

        in.close();

        System.out.println(String.valueOf(maxCount));

    }
}
