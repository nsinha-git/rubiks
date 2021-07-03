package nsinha;

import java.io.Serializable;

public class TestJavaClass implements Serializable {

    int a;
    int[] b;

    public static TestJavaClass getInstance(int n) {

        TestJavaClass t = new TestJavaClass();
        t.a =1;
        t.b = new int [n];
        return t;
    }

}
