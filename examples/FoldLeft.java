public class FoldLeft
{
  static int apply ()
  {
    class Let0
    {
      Object out;
      f2j.Closure x2;
      {
        class Fun28 extends f2j.Closure
        {
          f2j.Closure x29 = this;
          {
            class Fun31 extends f2j.Closure
            {
              f2j.Closure x32 = this;
              {
                class Fun34 extends f2j.Closure
                {
                  f2j.Closure x35 = this;
                  public void apply ()
                  {
                    final f2j.FunctionalList x36 = (f2j.FunctionalList) x35.x;
                    final Object x33 = x32.x;
                    final f2j.Closure x30 = (f2j.Closure) x29.x;
                    java.lang.Boolean x38 = x36.isEmpty();
                    Object ifres37;
                    if (x38)
                    {
                      ifres37 = x33;
                    }
                    else
                    {
                      f2j.Closure x41 = x2;
                      x41.x = x30;
                      final f2j.Closure temp42 = (f2j.Closure) x41.out;
                      f2j.Closure x44 = x30;
                      x44.x = x33;
                      final f2j.Closure temp45 = (f2j.Closure) x44.out;
                      java.lang.Integer x46 = x36.head();
                      f2j.Closure x43 = temp45;
                      x43.x = x46;
                      x43.apply();
                      final Object temp47 = x43.out;
                      f2j.Closure x40 = temp42;
                      x40.x = temp47;
                      final f2j.Closure temp48 = (f2j.Closure) x40.out;
                      f2j.FunctionalList x49 = x36.tail();
                      f2j.Closure x39 = temp48;
                      x39.x = x49;
                      x39.apply();
                      final Object temp50 = x39.out;
                      ifres37 = temp50;
                    }
                    out = ifres37;
                  }
                  public f2j.Closure clone ()
                  {
                    f2j.Closure c = new Fun34();
                    c.x = this.x;
                    c.apply();
                    return (f2j.Closure) c;
                  }
                }
                f2j.Closure x34 = new Fun34();
                out = x34;
              }
              public void apply ()
              {
              }
              public f2j.Closure clone ()
              {
                f2j.Closure c = new Fun31();
                c.x = this.x;
                c.apply();
                return (f2j.Closure) c;
              }
            }
            f2j.Closure x31 = new Fun31();
            out = x31;
          }
          public void apply ()
          {
          }
          public f2j.Closure clone ()
          {
            f2j.Closure c = new Fun28();
            c.x = this.x;
            c.apply();
            return (f2j.Closure) c;
          }
        }
        f2j.Closure x28 = new Fun28();
        x2 = x28;
        out = 1;
      }
    }
    Let0 x0 = new Let0();
    java.lang.Integer x1 = (java.lang.Integer) x0.out;
    return x1;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}