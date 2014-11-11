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


  // public static void main(String[] args) {

  //   MyList lst = new MyList();

  //   lst.cons(3);
  //   lst.cons(4);

  //   MyList l1 = lst.tail();
  //   System.out.println(l1.at(0));

  // }


}
