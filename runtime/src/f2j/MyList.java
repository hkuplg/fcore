package f2j;

import java.util.LinkedList;

public class MyList {
  LinkedList<Integer> lst;

  public MyList() {
    lst = new LinkedList<Integer>();
  }

  public MyList cons(Integer x) {
    lst.addFirst(x);
    return this;
  }

  public Integer head() {
    if (lst.isEmpty())
      return 0;
    return lst.element();
  }

  public MyList tail() {
    lst.pop();
    return this;
  }

  public int length() {
    return lst.size();
  }

  public boolean isEmpty() {
    return lst.size() == 0;
  }

  public Integer at(int index) {
    return lst.get(index);
  }

  public static MyList genFuncList(int n) {
    MyList wh = new MyList();

    for (int i = n; i > 0; i --) {
      wh.cons(i % 5 + 1);
    }
    return wh;
  }

}
