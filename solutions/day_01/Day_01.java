import java.util.Arrays;
import java.util.Scanner;

public class Day_01 {

    public static void main(String[] args) {

        // 1st value is current count
        long[] maxs = {0, 0, 0, 0};

        Scanner in = new Scanner(System.in);

        while (in.hasNextLine()) {

            String ln = in.nextLine();

            if (ln == "") {

                Arrays.sort(maxs);
                maxs[0] = 0;

            } else {
                maxs[0] += Integer.parseInt(ln);
            };
        };

        Arrays.sort(maxs);

        in.close();

        long ans = Arrays.stream(maxs).skip(1).reduce(0, (x, y) -> x + y);
        System.out.println(String.valueOf(ans));
    }
}
