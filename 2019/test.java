import helper.Helper;

import java.util.List;

public class test {

    public static void main(String[] args) {
        Integer a = 1;
        adder(a);
        System.out.println(a);
    }

    public static void adder(Integer acc) {
        if (acc.equals(10)) {
            return;
        }
        acc++;
        adder(acc);
    }
}