package lib;

public class IntArray {
    private int a[];

    public IntArray(int e0, int e1, int e2) { 
        a = new int[3];
        a[0] = e0;
        a[1] = e1;
        a[2] = e2;
    }
    
    public int at(int index) {
        return a[index];
    }

    public int length() {
        return a.length;
    }
}
