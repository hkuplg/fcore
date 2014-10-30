public class EvenOddEncoded2
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
        x5.x = 1;
        final f2j.Closure temp6 = (f2j.Closure) x5.out;
        f2j.Closure x4 = temp6;
        x4.x = 7;
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
      public void apply ()
      {
        final java.lang.Integer x10 = (java.lang.Integer) x8.x;
        final java.lang.Boolean x12 = x10 == 0;
        f2j.Closure ifres11;
        if (x12)
        {
          class Fun13 extends f2j.Closure
          {
            f2j.Closure x14 = this;
            public void apply ()
            {
              final java.lang.Integer x15 = (java.lang.Integer) x14.x;
              final java.lang.Boolean x17 = x15 == 0;
              java.lang.Integer ifres16;
              if (x17)
              {
                ifres16 = 1;
              }
              else
              {
                final java.lang.Boolean x19 = x15 == 1;
                java.lang.Integer ifres18;
                if (x19)
                {
                  ifres18 = 0;
                }
                else
                {
                  f2j.Closure x21 = x8;
                  x21.x = 1;
                  x21.apply();
                  final f2j.Closure temp22 = (f2j.Closure) x21.out;
                  final java.lang.Integer x23 = x15 - 1;
                  f2j.Closure x20 = temp22;
                  x20.x = x23;
                  x20.apply();
                  final java.lang.Integer temp24 = (java.lang.Integer) x20.out;
                  ifres18 = temp24;
                }
                ifres16 = ifres18;
              }
              out = ifres16;
            }
            public f2j.Closure clone ()
            {
              f2j.Closure c = new Fun13();
              c.x = this.x;
              c.apply();
              return (f2j.Closure) c;
            }
          }
          f2j.Closure x13 = new Fun13();
          ifres11 = x13;
        }
        else
        {
          class Fun25 extends f2j.Closure
          {
            f2j.Closure x26 = this;
            public void apply ()
            {
              final java.lang.Integer x27 = (java.lang.Integer) x26.x;
              final java.lang.Boolean x29 = x27 == 0;
              java.lang.Integer ifres28;
              if (x29)
              {
                ifres28 = 0;
              }
              else
              {
                final java.lang.Boolean x31 = x27 == 1;
                java.lang.Integer ifres30;
                if (x31)
                {
                  ifres30 = 1;
                }
                else
                {
                  f2j.Closure x33 = x8;
                  x33.x = 0;
                  x33.apply();
                  final f2j.Closure temp34 = (f2j.Closure) x33.out;
                  final java.lang.Integer x35 = x27 - 1;
                  f2j.Closure x32 = temp34;
                  x32.x = x35;
                  x32.apply();
                  final java.lang.Integer temp36 = (java.lang.Integer) x32.out;
                  ifres30 = temp36;
                }
                ifres28 = ifres30;
              }
              out = ifres28;
            }
            public f2j.Closure clone ()
            {
              f2j.Closure c = new Fun25();
              c.x = this.x;
              c.apply();
              return (f2j.Closure) c;
            }
          }
          f2j.Closure x25 = new Fun25();
          ifres11 = x25;
        }
        out = ifres11;
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
    final java.lang.Integer temp37 = (java.lang.Integer) x0.out;
    return temp37;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}