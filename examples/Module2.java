public class Module2
{
  static int apply ()
  {
    class Let1
    {
      Object temp;
      f2j.Closure x3;
      f2j.Closure x4;
      {
        class Fun19 extends f2j.Closure
        {
          f2j.Closure x20 = this;
          public void apply ()
          {
            final java.lang.Integer x21 = (java.lang.Integer) x20.arg;
            class Fun22 extends f2j.Closure
            {
              f2j.Closure x23 = this;
              public void apply ()
              {
                final java.lang.Integer x24 = (java.lang.Integer) x23.arg;
                final java.lang.Integer x25 = x21 + x24;
                res = x25;
              }
            }
            res = new Fun22();
          }
        }
        x3 = new Fun19();
        class Fun26 extends f2j.Closure
        {
          f2j.Closure x27 = this;
          public void apply ()
          {
            final java.lang.Integer x28 = (java.lang.Integer) x27.arg;
            class Fun29 extends f2j.Closure
            {
              f2j.Closure x30 = this;
              public void apply ()
              {
                final java.lang.Integer x31 = (java.lang.Integer) x30.arg;
                final java.lang.Integer x32 = x28 - x31;
                res = x32;
              }
            }
            res = new Fun29();
          }
        }
        x4 = new Fun26();
        final f2j.tuples.Tuple2 x33 = new f2j.tuples.Tuple2(x3, x4);
        temp = x33;
      }
    }
    Let1 x1 = new Let1();
    final f2j.tuples.Tuple2 x2 = (f2j.tuples.Tuple2) x1.temp;
    final f2j.tuples.Tuple2 x34 = x2;
    final f2j.tuples.Tuple2 x35 = x34;
    final f2j.Closure x36 = (f2j.Closure) x35._1;
    final f2j.Closure x37 = x36;
    final java.lang.Integer x41 = 2;
    final java.lang.Integer x38 = 1;
    f2j.Closure x39 = x37;
    x39.arg = x38;
    x39.apply();
    final f2j.Closure x40 = (f2j.Closure) x39.res;
    f2j.Closure x42 = x40;
    x42.arg = x41;
    x42.apply();
    final java.lang.Integer x43 = (java.lang.Integer) x42.res;
    return x43;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}