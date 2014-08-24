package hk.hku.cs.f2j;

public interface FunctionalList {
    Integer head();
    FunctionalList tail();
}

class Nil implements FunctionalList { 
    public Integer head() {
        return null;
    }

    public FunctionalList tail() {
        return null;
    }
}

class Cons implements FunctionalList {
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