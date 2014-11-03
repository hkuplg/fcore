package f2j;

public class FunctionalTree{
    final Integer value;
    final FunctionalTree left;
    final FunctionalTree right;
    final boolean empty;

    public FunctionalTree(){
        empty = true;
        value=0;
        left=null;
        right=null;
    }

    public FunctionalTree(Integer value, FunctionalTree left, FunctionalTree right){
        empty = false;
        this.value = value;
        this.left = left;
        this.right = right;
    }

    boolean isEmpty(){
        return empty;
    }

    public Integer getValue(){
        return value;
    }

    public FunctionalTree getLeft(){
        return left;
    }

    public FunctionalTree getRight(){
        return right;
    }
}
