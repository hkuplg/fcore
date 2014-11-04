public class Apply
{
  static int apply ()
  {
    class Fun2 extends f2j.Closure
    {
      f2j.Closure x3 = this;
      {
        class Fun5 extends f2j.Closure
        {
          f2j.Closure x6 = this;
          {
            class Fun8 extends f2j.Closure
            {
              f2j.Closure x9 = this;
              public void apply ()
              {
                final java.lang.Integer x10 = (java.lang.Integer) x9.x;
                final java.lang.Integer x7 = (java.lang.Integer) x6.x;
                final f2j.Closure x4 = (f2j.Closure) x3.x;
                final java.lang.Integer x13 = x7;
                final java.lang.Integer x14 = x13;
                f2j.Closure x12 = x4;
                x12.x = x14;
                final f2j.Closure temp15 = (f2j.Closure) x12.out;
                final java.lang.Integer x16 = x10;
                final java.lang.Integer x17 = x16;
                f2j.Closure x11 = temp15;
                x11.x = x17;
                x11.apply();
                final java.lang.Integer temp18 = (java.lang.Integer) x11.out;
                out = temp18;
              }
              public f2j.Closure clone ()
              {
                f2j.Closure c = new Fun8();
                c.x = this.x;
                c.apply();
                return (f2j.Closure) c;
              }
            }
            f2j.Closure x8 = new Fun8();
            out = x8;
          }
          public void apply ()
          {
          }
          public f2j.Closure clone ()
          {
            f2j.Closure c = new Fun5();
            c.x = this.x;
            c.apply();
            return (f2j.Closure) c;
          }
        }
        f2j.Closure x5 = new Fun5();
        out = x5;
      }
      public void apply ()
      {
      }
      public f2j.Closure clone ()
      {
        f2j.Closure c = new Fun2();
        c.x = this.x;
        c.apply();
        return (f2j.Closure) c;
      }
    }
    f2j.Closure x2 = new Fun2();
    final f2j.Closure x0 = x2;
    class Fun24 extends f2j.Closure
    {
      f2j.Closure x25 = this;
      {
        class Fun27 extends f2j.Closure
        {
          f2j.Closure x28 = this;
          public void apply ()
          {
            final java.lang.Integer x29 = (java.lang.Integer) x28.x;
            final java.lang.Integer x26 = (java.lang.Integer) x25.x;
            final java.lang.Integer x30 = x26 + x29;
            out = x30;
          }
          public f2j.Closure clone ()
          {
            f2j.Closure c = new Fun27();
            c.x = this.x;
            c.apply();
            return (f2j.Closure) c;
          }
        }
        f2j.Closure x27 = new Fun27();
        out = x27;
      }
      public void apply ()
      {
      }
      public f2j.Closure clone ()
      {
        f2j.Closure c = new Fun24();
        c.x = this.x;
        c.apply();
        return (f2j.Closure) c;
      }
    }
    f2j.Closure x24 = new Fun24();
    final f2j.Closure x22 = x24;
    class Fun31 extends f2j.Closure
    {
      f2j.Closure x32 = this;
      public void apply ()
      {
        final java.lang.Integer x33 = (java.lang.Integer) x32.x;
        final java.lang.Integer x37 = x33;
        final java.lang.Integer x38 = x37;
        f2j.Closure x36 = x22;
        x36.x = x38;
        final f2j.Closure temp39 = (f2j.Closure) x36.out;
        final f2j.Closure x34 = temp39;
        class Fun40 extends f2j.Closure
        {
          f2j.Closure x41 = this;
          public void apply ()
          {
            final java.lang.Integer x42 = (java.lang.Integer) x41.x;
            final java.lang.Integer x46 = x42;
            final java.lang.Integer x47 = x46;
            f2j.Closure x45 = x34;
            x45.x = x47;
            x45.apply();
            final java.lang.Integer temp48 = (java.lang.Integer) x45.out;
            final java.lang.Integer x43 = temp48;
            final java.lang.Integer x44 = x43;
            out = x44;
          }
          public f2j.Closure clone ()
          {
            f2j.Closure c = new Fun40();
            c.x = this.x;
            c.apply();
            return (f2j.Closure) c;
          }
        }
        f2j.Closure x40 = new Fun40();
        final f2j.Closure x35 = x40;
        out = x35;
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
    final f2j.Closure x23 = x31;
    f2j.Closure x21 = x0;
    x21.x = x23;
    final f2j.Closure temp49 = (f2j.Closure) x21.out;
    f2j.Closure x20 = temp49;
    x20.x = 1;
    final f2j.Closure temp50 = (f2j.Closure) x20.out;
    f2j.Closure x19 = temp50;
    x19.x = 1;
    x19.apply();
    final java.lang.Integer temp51 = (java.lang.Integer) x19.out;
    final java.lang.Integer x1 = temp51;
    return x1;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}