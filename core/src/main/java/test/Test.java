package test;

import java.util.ArrayList;
import java.util.HashMap;

class Test {
    class Inner {
        class Foo {
        }
    }

    class Inner2 {
        class Foo {
        }
    }

    public void move(Inner.Foo foo) {
    }

    public void move(Inner2.Foo foo) {
    }

    public <T extends CharSequence> ArrayList<T> foo(HashMap<Integer, String[]> n, T e, int number) {
        return null;
    }
}