import helper.Helper;

import java.util.List;

public class D1 {

    public static void main(String[] args) {
        q1();
        q2();
    }

    public static void q1() {
        List<String> input = Helper.input("day1.txt");

        int sum = input.stream()
            .mapToInt(Integer::parseInt)
            .map(weight -> (weight/3) - 2)
            .sum();

        System.out.println(sum);

    }

    public static void q2() {
        List<String> input = Helper.input("day1.txt");
        Integer heyy;
        int sum = input.stream()
            .mapToInt(Integer::parseInt)
            .map(weight -> fuel(weight))
            .sum();
        System.out.println(sum);
    }

    public static int fuel(int acc) {
        acc = acc/3 - 2;
        if(acc <= 0) {
            return 0;
        } else { 
            return acc + fuel(acc);
        }
    }
}