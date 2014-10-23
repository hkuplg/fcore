package hk.hku.cs.f2j;

public class Cons extends FunctionalList {
    final Integer x;
    final FunctionalList xs;

    public Cons(Integer x, FunctionalList xs) {
        this.x = x;
        this.xs = xs;
    }

    public Integer head() {
        return x;
    }

    public FunctionalList tail() {
        return xs;
    }
}
