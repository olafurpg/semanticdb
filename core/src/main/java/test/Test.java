package test;

import java.util.ArrayList;
import java.util.HashMap;

class Test {
    public <T> ArrayList<T> foo(HashMap<? extends Integer, String[]> n, T e) { return null; }
}