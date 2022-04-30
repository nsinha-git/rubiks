package org.nish.app;

import java.util.Scanner;

public class Application {

    HashDb hashDb = new HashDb("/tmp");


    public static void main() {
        Scanner scanner = new Scanner(System.in);

        while(true) {
            System.out.println("Enter I for Insert, Q for Query, D for delete, E for Exit");
            if (scanner.hasNextLine()) {
                String next = scanner.nextLine();
                if (next.equals("E")) return;
                if (next.equals("I")) processPut();
                if (next.equals("Q")) processGet();
                if (next.equals("D")) processDelete();

            }
        }
    }


    public static void processPut() {}
    public static void processGet() {}
    public static void processDelete() {}




}
