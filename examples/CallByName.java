public class CallByName
{
  static Object apply ()
  {
    class Fun2 extends f2j.Closure
    {
      f2j.Closure x3 = this;
      public void apply ()
      {
        final java.lang.Integer x4 = (java.lang.Integer) x3.x;
        final java.io.PrintStream x5 = (java.io.PrintStream) java.lang.System.out;
        x5.<java.lang.String>println("calling something");
        out = 1;
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
    class Fun8 extends f2j.Closure
    {
      f2j.Closure x9 = this;
      public void apply ()
      {
        final java.lang.Integer x10 = (java.lang.Integer) x9.x;
        final java.io.PrintStream x11 = (java.io.PrintStream) java.lang.System.out;
        x11.<java.lang.Integer>println(x10);
        final java.io.PrintStream x12 = (java.io.PrintStream) java.lang.System.out;
        x12.<java.lang.Integer>println(x10);
        out = x12.<java.lang.Integer>println(x10);
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
    final f2j.Closure x6 = x8;
    class Fun15 extends f2j.Closure
    {
      f2j.Closure x16 = this;
      public void apply ()
      {
        final f2j.Closure x17 = (f2j.Closure) x16.x;
        f2j.Closure x18 = x17;
        x18.x = 0;
        x18.apply();
        final java.lang.Integer temp19 = (java.lang.Integer) x18.out;
        final java.io.PrintStream x20 = (java.io.PrintStream) java.lang.System.out;
        x20.<java.lang.Integer>println(temp19);
        f2j.Closure x21 = x17;
        x21.x = 0;
        x21.apply();
        final java.lang.Integer temp22 = (java.lang.Integer) x21.out;
        final java.io.PrintStream x23 = (java.io.PrintStream) java.lang.System.out;
        x23.<java.lang.Integer>println(temp22);
        out = x23.<java.lang.Integer>println(temp22);
      }
      public f2j.Closure clone ()
      {
        f2j.Closure c = new Fun15();
        c.x = this.x;
        c.apply();
        return (f2j.Closure) c;
      }
    }
    f2j.Closure x15 = new Fun15();
    final f2j.Closure x13 = x15;
    class Fun26 extends f2j.Closure
    {
      f2j.Closure x27 = this;
      public void apply ()
      {
        final java.lang.Integer x28 = (java.lang.Integer) x27.x;
        out = x28;
      }
      public f2j.Closure clone ()
      {
        f2j.Closure c = new Fun26();
        c.x = this.x;
        c.apply();
        return (f2j.Closure) c;
      }
    }
    f2j.Closure x26 = new Fun26();
    class Fun31 extends f2j.Closure
    {
      f2j.Closure x32 = this;
      public void apply ()
      {
        final java.lang.Integer x33 = (java.lang.Integer) x32.x;
        out = x33;
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
    f2j.Closure x30 = x31;
    x30.x = 0;
    x30.apply();
    final java.lang.Integer temp34 = (java.lang.Integer) x30.out;
    f2j.Closure x29 = x0;
    x29.x = temp34;
    x29.apply();
    final java.lang.Integer temp35 = (java.lang.Integer) x29.out;
    f2j.Closure x25 = x26;
    x25.x = temp35;
    x25.apply();
    final java.lang.Integer temp36 = (java.lang.Integer) x25.out;
    f2j.Closure x24 = x6;
    x24.x = temp36;
    x24.apply();
    final java.lang.Void temp37 = (java.lang.Void) x24.out;
    class Fun40 extends f2j.Closure
    {
      f2j.Closure x41 = this;
      public void apply ()
      {
        final f2j.Closure x42 = (f2j.Closure) x41.x;
        class Fun44 extends f2j.Closure
        {
          f2j.Closure x45 = this;
          public void apply ()
          {
            final java.lang.Integer x46 = (java.lang.Integer) x45.x;
            out = x46;
          }
          public f2j.Closure clone ()
          {
            f2j.Closure c = new Fun44();
            c.x = this.x;
            c.apply();
            return (f2j.Closure) c;
          }
        }
        f2j.Closure x44 = new Fun44();
        f2j.Closure x43 = x44;
        x43.x = x42;
        x43.apply();
        final java.lang.Integer temp47 = (java.lang.Integer) x43.out;
        out = temp47;
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
    class Fun48 extends f2j.Closure
    {
      f2j.Closure x49 = this;
      public void apply ()
      {
        final java.lang.Integer x50 = (java.lang.Integer) x49.x;
        class Fun53 extends f2j.Closure
        {
          f2j.Closure x54 = this;
          public void apply ()
          {
            final java.lang.Integer x55 = (java.lang.Integer) x54.x;
            out = x55;
          }
          public f2j.Closure clone ()
          {
            f2j.Closure c = new Fun53();
            c.x = this.x;
            c.apply();
            return (f2j.Closure) c;
          }
        }
        f2j.Closure x53 = new Fun53();
        f2j.Closure x52 = x53;
        x52.x = 0;
        x52.apply();
        final java.lang.Integer temp56 = (java.lang.Integer) x52.out;
        f2j.Closure x51 = x0;
        x51.x = temp56;
        x51.apply();
        final java.lang.Integer temp57 = (java.lang.Integer) x51.out;
        out = temp57;
      }
      public f2j.Closure clone ()
      {
        f2j.Closure c = new Fun48();
        c.x = this.x;
        c.apply();
        return (f2j.Closure) c;
      }
    }
    f2j.Closure x48 = new Fun48();
    f2j.Closure x39 = x40;
    x39.x = x48;
    x39.apply();
    final java.lang.Integer temp58 = (java.lang.Integer) x39.out;
    f2j.Closure x38 = x13;
    x38.x = temp58;
    x38.apply();
    final java.lang.Void temp59 = (java.lang.Void) x38.out;
    final java.lang.Void x14 = temp59;
    final java.lang.Void x7 = x14;
    final java.lang.Void x1 = x7;
    return x1;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}