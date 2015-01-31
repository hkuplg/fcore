package f2j;

public class FunctionalList {
    final Object x;
    final FunctionalList xs;
    final boolean empty;
    final int len;

    public FunctionalList() {
        empty = true;
        len = 0;
        this.x = null;
        this.xs = null;
    }

    public FunctionalList(Object x, FunctionalList xs) {
        empty = false;
        len = xs.length() + 1;
        this.x = x;
        this.xs = xs;
    }

    public Object head() {
        return x;
    }

    public FunctionalList tail() {
        return xs;
    }

    public int length() {
        return len;
    }

    public boolean isEmpty() {
        return empty;
    }

    public Object at(int index) {
        if(index == 0)
            return x;
        return xs.at(index - 1);
    }
}
