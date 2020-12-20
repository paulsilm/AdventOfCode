import helper.Helper;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class D2 {

    public static void main(String[] args) {
        q1();
        //q2();
    }

    public static void q1() {
        List<String> input = Helper.input("day2.txt");
        List<Integer> codes = Arrays.stream(input.get(0).split(","))
            .map(Integer::parseInt)
            .collect(Collectors.toList());
        
        codes = machine(codes, 12, 2);
        System.out.println(codes.get(0));
    }    

    public static void q2() {
        List<String> input = Helper.input("day2.txt");
        List<Integer> original = Arrays.stream(input.get(0).split(","))
            .map(Integer::parseInt)
            .collect(Collectors.toList());

        for(int i = 0; i < 100; i++) {
            for(int j = 1; j < 100; j++) {
                List<Integer> codes = machine(original, i, j);
                if (codes.get(0) == 19690720) {
                    System.out.println("noun: " + codes.get(1) + "\nverb: " + codes.get(2));
                    return;
                }
            }
        }
    }    

    public static List<Integer> machine(List<Integer> original, int noun, int verb) {
        List<Integer> codes = new ArrayList<>(original);
        codes.set(1, noun);
        codes.set(2, verb);
        for(int i = 0; i < codes.size(); i += 4) {
            switch(codes.get(i)) {
                case 1: 
                    codes.set(codes.get(i+3), codes.get(codes.get(i+1)) + codes.get(codes.get(i+2)));
                    break;
                case 2: 
                    codes.set(codes.get(i+3), codes.get(codes.get(i+1)) * codes.get(codes.get(i+2)));
                    break;
                case 99:
                    return codes;
            }
        }
        return codes;
    }
}