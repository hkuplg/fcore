package hk.hku.cs.f2j;

public class Cons implements FunctionalList {
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

    public Integer at(int index) {
        if(index == 0)
            return head();
        else
            return tail().at(index - 1);
    }
}
