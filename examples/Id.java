public class Id
{
  static int apply ()
  {
    class Fun1 extends f2j.Closure
    {
      f2j.Closure x2 = this;
      public void apply ()
      {
        final Object x3 = x2.arg;
        res = x3;
      }
    }
    f2j.Closure x4 = new Fun1();
    x4.arg = new Fun1();
    x4.apply();
    final f2j.Closure x5 = (f2j.Closure) x4.res;
    f2j.Closure x6 = x5;
    x6.arg = 10;
    x6.apply();
    final java.lang.Integer x7 = (java.lang.Integer) x6.res;
    return x7;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}