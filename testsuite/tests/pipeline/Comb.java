public class Comb
{
  static int apply ()
  {
    class Fun1 extends f2j.Closure
    {
      f2j.Closure x2 = this;
      public void apply ()
      {
        final f2j.Closure x3 = (f2j.Closure) x2.x;
        f2j.Closure x5 = x3;
        x5.x = 12;
        final f2j.Closure temp6 = (f2j.Closure) x5.out;
        f2j.Closure x4 = temp6;
        x4.x = 3;
        x4.apply();
        final java.lang.Integer temp7 = (java.lang.Integer) x4.out;
        out = temp7;
      }
      public f2j.Closure clone ()
      {
        f2j.Closure c = new Fun1();
        c.x = this.x;
        c.apply();
        return (f2j.Closure) c;
      }
    }
    f2j.Closure x1 = new Fun1();
    class Fun9 extends f2j.Closure
    {
      f2j.Closure x8 = this;
      {
        class Fun11 extends f2j.Closure
        {
          f2j.Closure x12 = this;
          public void apply ()
          {
            final java.lang.Integer x13 = (java.lang.Integer) x12.x;
            final java.lang.Integer x10 = (java.lang.Integer) x8.x;
            final java.lang.Boolean x15 = x13 == 0;
            final java.lang.Boolean x16 = x13 == x10;
            final java.lang.Boolean x17 = x15 || x16;
            java.lang.Integer ifres14;
            if (x17)
            {
              ifres14 = 1;
            }
            else
            {
              final java.lang.Integer x20 = x10 - 1;
              f2j.Closure x19 = x8;
              x19.x = x20;
              final f2j.Closure temp21 = (f2j.Closure) x19.out;
              final java.lang.Integer x22 = x13 - 1;
              f2j.Closure x18 = temp21;
              x18.x = x22;
              x18.apply();
              final java.lang.Integer temp23 = (java.lang.Integer) x18.out;
              final java.lang.Integer x26 = x10 - 1;
              f2j.Closure x25 = x8.clone();
              x25.x = x26;
              final f2j.Closure temp27 = (f2j.Closure) x25.out;
              f2j.Closure x24 = temp27;
              x24.x = x13;
              x24.apply();
              final java.lang.Integer temp28 = (java.lang.Integer) x24.out;
              final java.lang.Integer x29 = temp23 + temp28;
              ifres14 = x29;
            }
            out = ifres14;
          }
          public f2j.Closure clone ()
          {
            f2j.Closure c = new Fun11();
            c.x = this.x;
            c.apply();
            return (f2j.Closure) c;
          }
        }
        f2j.Closure x11 = new Fun11();
        out = x11;
      }
      public void apply ()
      {
      }
      public f2j.Closure clone ()
      {
        f2j.Closure c = new Fun9();
        c.x = this.x;
        c.apply();
        return (f2j.Closure) c;
      }
    }
    f2j.Closure x9 = new Fun9();
    f2j.Closure x0 = x1;
    x0.x = x9;
    x0.apply();
    final java.lang.Integer temp30 = (java.lang.Integer) x0.out;
    return temp30;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}