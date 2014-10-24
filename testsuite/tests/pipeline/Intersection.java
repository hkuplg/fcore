public class Intersection
{
  static int apply ()
  {
    class Fun1 extends f2j.Closure
    {
      f2j.Closure x2 = this;
      public void apply ()
      {
        final java.lang.Integer x3 = (java.lang.Integer) x2.x;
        java.lang.Integer x4 = x3 + 1;
        out = x4;
      }
    }
    f2j.Closure x1 = new Fun1();
    class Fun5 extends f2j.Closure
    {
      f2j.Closure x6 = this;
      public void apply ()
      {
        final java.lang.Integer x7 = (java.lang.Integer) x6.x;
        out = 0;
      }
    }
    f2j.Closure x5 = new Fun5();
    f2j.tuples.Tuple2 x8 = new f2j.tuples.Tuple2(x5, 1);
    f2j.Closure x0 = x1;
    x0.x = x8;
    x0.apply();
    final java.lang.Integer temp9 = (java.lang.Integer) x0.out;
    return temp9;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}