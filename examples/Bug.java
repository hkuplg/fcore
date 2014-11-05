public class Bug
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
                class Fun14 extends f2j.Closure
                {
                  f2j.Closure x15 = this;
                  public void apply ()
                  {
                    final java.lang.Integer x16 = (java.lang.Integer) x15.x;
                    out = x16;
                  }
                  public f2j.Closure clone ()
                  {
                    f2j.Closure c = new Fun14();
                    c.x = this.x;
                    c.apply();
                    return (f2j.Closure) c;
                  }
                }
                f2j.Closure x14 = new Fun14();
                f2j.Closure x13 = x14;
                x13.x = x7;
                x13.apply();
                final java.lang.Integer temp17 = (java.lang.Integer) x13.out;
                f2j.Closure x12 = x4;
                x12.x = temp17;
                final f2j.Closure temp18 = (f2j.Closure) x12.out;
                class Fun20 extends f2j.Closure
                {
                  f2j.Closure x21 = this;
                  public void apply ()
                  {
                    final java.lang.Integer x22 = (java.lang.Integer) x21.x;
                    out = x22;
                  }
                  public f2j.Closure clone ()
                  {
                    f2j.Closure c = new Fun20();
                    c.x = this.x;
                    c.apply();
                    return (f2j.Closure) c;
                  }
                }
                f2j.Closure x20 = new Fun20();
                f2j.Closure x19 = x20;
                x19.x = x10;
                x19.apply();
                final java.lang.Integer temp23 = (java.lang.Integer) x19.out;
                f2j.Closure x11 = temp18;
                x11.x = temp23;
                x11.apply();
                final java.lang.Integer temp24 = (java.lang.Integer) x11.out;
                out = temp24;
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
    class Fun29 extends f2j.Closure
    {
      f2j.Closure x30 = this;
      {
        class Fun32 extends f2j.Closure
        {
          f2j.Closure x33 = this;
          public void apply ()
          {
            final java.lang.Integer x34 = (java.lang.Integer) x33.x;
            final f2j.Closure x31 = (f2j.Closure) x30.x;
            class Fun36 extends f2j.Closure
            {
              f2j.Closure x37 = this;
              {
                class Fun39 extends f2j.Closure
                {
                  f2j.Closure x40 = this;
                  public void apply ()
                  {
                    final java.lang.Integer x41 = (java.lang.Integer) x40.x;
                    final f2j.Closure x38 = (f2j.Closure) x37.x;
                    class Fun43 extends f2j.Closure
                    {
                      f2j.Closure x44 = this;
                      public void apply ()
                      {
                        final java.lang.Integer x45 = (java.lang.Integer) x44.x;
                        out = x45;
                      }
                      public f2j.Closure clone ()
                      {
                        f2j.Closure c = new Fun43();
                        c.x = this.x;
                        c.apply();
                        return (f2j.Closure) c;
                      }
                    }
                    f2j.Closure x43 = new Fun43();
                    class Fun48 extends f2j.Closure
                    {
                      f2j.Closure x49 = this;
                      public void apply ()
                      {
                        final java.lang.Integer x50 = (java.lang.Integer) x49.x;
                        out = x50;
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
                    f2j.Closure x47 = x48;
                    x47.x = x41;
                    x47.apply();
                    final java.lang.Integer temp51 = (java.lang.Integer) x47.out;
                    f2j.Closure x46 = x38;
                    x46.x = temp51;
                    x46.apply();
                    final java.lang.Integer temp52 = (java.lang.Integer) x46.out;
                    f2j.Closure x42 = x43;
                    x42.x = temp52;
                    x42.apply();
                    final java.lang.Integer temp53 = (java.lang.Integer) x42.out;
                    out = temp53;
                  }
                  public f2j.Closure clone ()
                  {
                    f2j.Closure c = new Fun39();
                    c.x = this.x;
                    c.apply();
                    return (f2j.Closure) c;
                  }
                }
                f2j.Closure x39 = new Fun39();
                out = x39;
              }
              public void apply ()
              {
              }
              public f2j.Closure clone ()
              {
                f2j.Closure c = new Fun36();
                c.x = this.x;
                c.apply();
                return (f2j.Closure) c;
              }
            }
            f2j.Closure x36 = new Fun36();
            class Fun56 extends f2j.Closure
            {
              f2j.Closure x57 = this;
              public void apply ()
              {
                final java.lang.Integer x58 = (java.lang.Integer) x57.x;
                out = x58;
              }
              public f2j.Closure clone ()
              {
                f2j.Closure c = new Fun56();
                c.x = this.x;
                c.apply();
                return (f2j.Closure) c;
              }
            }
            f2j.Closure x56 = new Fun56();
            f2j.Closure x55 = x56;
            x55.x = x34;
            x55.apply();
            final java.lang.Integer temp59 = (java.lang.Integer) x55.out;
            f2j.Closure x54 = x31;
            x54.x = temp59;
            final f2j.Closure temp60 = (f2j.Closure) x54.out;
            f2j.Closure x35 = x36;
            x35.x = temp60;
            final f2j.Closure temp61 = (f2j.Closure) x35.out;
            out = temp61;
          }
          public f2j.Closure clone ()
          {
            f2j.Closure c = new Fun32();
            c.x = this.x;
            c.apply();
            return (f2j.Closure) c;
          }
        }
        f2j.Closure x32 = new Fun32();
        out = x32;
      }
      public void apply ()
      {
      }
      public f2j.Closure clone ()
      {
        f2j.Closure c = new Fun29();
        c.x = this.x;
        c.apply();
        return (f2j.Closure) c;
      }
    }
    f2j.Closure x29 = new Fun29();
    class Fun62 extends f2j.Closure
    {
      f2j.Closure x63 = this;
      {
        class Fun65 extends f2j.Closure
        {
          f2j.Closure x66 = this;
          public void apply ()
          {
            final java.lang.Integer x67 = (java.lang.Integer) x66.x;
            final java.lang.Integer x64 = (java.lang.Integer) x63.x;
            final java.lang.Integer x68 = x64 + x67;
            out = x68;
          }
          public f2j.Closure clone ()
          {
            f2j.Closure c = new Fun65();
            c.x = this.x;
            c.apply();
            return (f2j.Closure) c;
          }
        }
        f2j.Closure x65 = new Fun65();
        out = x65;
      }
      public void apply ()
      {
      }
      public f2j.Closure clone ()
      {
        f2j.Closure c = new Fun62();
        c.x = this.x;
        c.apply();
        return (f2j.Closure) c;
      }
    }
    f2j.Closure x62 = new Fun62();
    f2j.Closure x28 = x29;
    x28.x = x62;
    final f2j.Closure temp69 = (f2j.Closure) x28.out;
    f2j.Closure x27 = x0;
    x27.x = temp69;
    final f2j.Closure temp70 = (f2j.Closure) x27.out;
    class Fun72 extends f2j.Closure
    {
      f2j.Closure x73 = this;
      public void apply ()
      {
        final java.lang.Integer x74 = (java.lang.Integer) x73.x;
        out = x74;
      }
      public f2j.Closure clone ()
      {
        f2j.Closure c = new Fun72();
        c.x = this.x;
        c.apply();
        return (f2j.Closure) c;
      }
    }
    f2j.Closure x72 = new Fun72();
    f2j.Closure x71 = x72;
    x71.x = 1;
    x71.apply();
    final java.lang.Integer temp75 = (java.lang.Integer) x71.out;
    f2j.Closure x26 = temp70;
    x26.x = temp75;
    final f2j.Closure temp76 = (f2j.Closure) x26.out;
    class Fun78 extends f2j.Closure
    {
      f2j.Closure x79 = this;
      public void apply ()
      {
        final java.lang.Integer x80 = (java.lang.Integer) x79.x;
        out = x80;
      }
      public f2j.Closure clone ()
      {
        f2j.Closure c = new Fun78();
        c.x = this.x;
        c.apply();
        return (f2j.Closure) c;
      }
    }
    f2j.Closure x78 = new Fun78();
    f2j.Closure x77 = x78;
    x77.x = 1;
    x77.apply();
    final java.lang.Integer temp81 = (java.lang.Integer) x77.out;
    f2j.Closure x25 = temp76;
    x25.x = temp81;
    x25.apply();
    final java.lang.Integer temp82 = (java.lang.Integer) x25.out;
    final java.lang.Integer x1 = temp82;
    return x1;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}