package org.util;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Reader {

    public static List<String> read(String filename) {
        List<String> ret = new ArrayList<String>();
        try {
            BufferedReader file = new BufferedReader(new FileReader(filename));
            String next = file.readLine();
            while (next != null) {
                ret.addLast(next);
                next = file.readLine();
            }
            file.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return ret;
    }
}
