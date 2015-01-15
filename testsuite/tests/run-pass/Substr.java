public class Substr
{
  static Object apply ()
  {
    class Fun1 extends f2j.Closure
    {
      f2j.Closure x2 = this;
      public void apply ()
      {
        final java.lang.String x3 = (java.lang.String) x2.arg;
        class Fun4 extends f2j.Closure
        {
          f2j.Closure x5 = this;
          public void apply ()
          {
            final java.lang.Integer x6 = (java.lang.Integer) x5.arg;
            class Fun7 extends f2j.Closure
            {
              f2j.Closure x8 = this;
              public void apply ()
              {
                final java.lang.Integer x9 = (java.lang.Integer) x8.arg;
                final java.lang.String x10 = x3.<java.lang.Integer, java.lang.Integer>substring(x6, x9);
                res = x10;
              }
            }
            res = new Fun7();
          }
        }
        res = new Fun4();
      }
    }
    final java.lang.Integer x17 = 3;
    final java.lang.Integer x14 = 1;
    final java.lang.String x11 = "abcde";
    f2j.Closure x12 = new Fun1();
    x12.arg = x11;
    x12.apply();
    final f2j.Closure x13 = (f2j.Closure) x12.res;
    f2j.Closure x15 = x13;
    x15.arg = x14;
    x15.apply();
    final f2j.Closure x16 = (f2j.Closure) x15.res;
    f2j.Closure x18 = x16;
    x18.arg = x17;
    x18.apply();
    final java.lang.String x19 = (java.lang.String) x18.res;
    return x19;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}