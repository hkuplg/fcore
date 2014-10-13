public class Bob
{
  static Object apply ()
  {
    class Fun1 extends hk.hku.cs.f2j.Closure
    {
      hk.hku.cs.f2j.Closure x2 = this;
      public void apply ()
      {
        final hk.hku.cs.f2j.tuples.Tuple2 x3 = (hk.hku.cs.f2j.tuples.Tuple2) x2.x;
        class Fun5 extends hk.hku.cs.f2j.Closure
        {
          hk.hku.cs.f2j.Closure x6 = this;
          public void apply ()
          {
            final hk.hku.cs.f2j.tuples.Tuple2 x7 = (hk.hku.cs.f2j.tuples.Tuple2) x6.x;
            class Fun9 extends hk.hku.cs.f2j.Closure
            {
              hk.hku.cs.f2j.Closure x10 = this;
              public void apply ()
              {
                final java.lang.String x11 = (java.lang.String) x10.x;
                out = x11;
              }
              public hk.hku.cs.f2j.Closure clone ()
              {
                hk.hku.cs.f2j.Closure c = new Fun9();
                c.x = this.x;
                c.apply();
                return (hk.hku.cs.f2j.Closure) c;
              }
            }
            hk.hku.cs.f2j.Closure x9 = new Fun9();
            java.lang.String x12 = (java.lang.String) x7._2;
            hk.hku.cs.f2j.Closure x8 = x9;
            x8.x = x12;
            x8.apply();
            final java.lang.String temp13 = (java.lang.String) x8.out;
            out = temp13;
          }
          public hk.hku.cs.f2j.Closure clone ()
          {
            hk.hku.cs.f2j.Closure c = new Fun5();
            c.x = this.x;
            c.apply();
            return (hk.hku.cs.f2j.Closure) c;
          }
        }
        hk.hku.cs.f2j.Closure x5 = new Fun5();
        hk.hku.cs.f2j.Closure x4 = x5;
        x4.x = x3;
        x4.apply();
        final java.lang.String temp14 = (java.lang.String) x4.out;
        out = temp14;
      }
      public hk.hku.cs.f2j.Closure clone ()
      {
        hk.hku.cs.f2j.Closure c = new Fun1();
        c.x = this.x;
        c.apply();
        return (hk.hku.cs.f2j.Closure) c;
      }
    }
    hk.hku.cs.f2j.Closure x1 = new Fun1();
    class Fun16 extends hk.hku.cs.f2j.Closure
    {
      hk.hku.cs.f2j.Closure x17 = this;
      public void apply ()
      {
        final hk.hku.cs.f2j.tuples.Tuple2 x18 = (hk.hku.cs.f2j.tuples.Tuple2) x17.x;
        class Fun20 extends hk.hku.cs.f2j.Closure
        {
          hk.hku.cs.f2j.Closure x21 = this;
          public void apply ()
          {
            final hk.hku.cs.f2j.tuples.Tuple2 x22 = (hk.hku.cs.f2j.tuples.Tuple2) x21.x;
            class Fun24 extends hk.hku.cs.f2j.Closure
            {
              hk.hku.cs.f2j.Closure x25 = this;
              public void apply ()
              {
                final hk.hku.cs.f2j.tuples.Tuple2 x26 = (hk.hku.cs.f2j.tuples.Tuple2) x25.x;
                class Fun28 extends hk.hku.cs.f2j.Closure
                {
                  hk.hku.cs.f2j.Closure x29 = this;
                  public void apply ()
                  {
                    final hk.hku.cs.f2j.tuples.Tuple2 x30 = (hk.hku.cs.f2j.tuples.Tuple2) x29.x;
                    java.lang.String x31 = (java.lang.String) x30._1;
                    out = x31;
                  }
                  public hk.hku.cs.f2j.Closure clone ()
                  {
                    hk.hku.cs.f2j.Closure c = new Fun28();
                    c.x = this.x;
                    c.apply();
                    return (hk.hku.cs.f2j.Closure) c;
                  }
                }
                hk.hku.cs.f2j.Closure x28 = new Fun28();
                hk.hku.cs.f2j.tuples.Tuple2 x32 = (hk.hku.cs.f2j.tuples.Tuple2) x26._1;
                hk.hku.cs.f2j.Closure x27 = x28;
                x27.x = x32;
                x27.apply();
                final java.lang.String temp33 = (java.lang.String) x27.out;
                out = temp33;
              }
              public hk.hku.cs.f2j.Closure clone ()
              {
                hk.hku.cs.f2j.Closure c = new Fun24();
                c.x = this.x;
                c.apply();
                return (hk.hku.cs.f2j.Closure) c;
              }
            }
            hk.hku.cs.f2j.Closure x24 = new Fun24();
            hk.hku.cs.f2j.Closure x23 = x24;
            x23.x = x22;
            x23.apply();
            final java.lang.String temp34 = (java.lang.String) x23.out;
            class Fun36 extends hk.hku.cs.f2j.Closure
            {
              hk.hku.cs.f2j.Closure x37 = this;
              public void apply ()
              {
                final hk.hku.cs.f2j.tuples.Tuple2 x38 = (hk.hku.cs.f2j.tuples.Tuple2) x37.x;
                class Fun40 extends hk.hku.cs.f2j.Closure
                {
                  hk.hku.cs.f2j.Closure x41 = this;
                  public void apply ()
                  {
                    final hk.hku.cs.f2j.tuples.Tuple2 x42 = (hk.hku.cs.f2j.tuples.Tuple2) x41.x;
                    java.lang.Integer x43 = (java.lang.Integer) x42._2;
                    out = x43;
                  }
                  public hk.hku.cs.f2j.Closure clone ()
                  {
                    hk.hku.cs.f2j.Closure c = new Fun40();
                    c.x = this.x;
                    c.apply();
                    return (hk.hku.cs.f2j.Closure) c;
                  }
                }
                hk.hku.cs.f2j.Closure x40 = new Fun40();
                hk.hku.cs.f2j.tuples.Tuple2 x44 = (hk.hku.cs.f2j.tuples.Tuple2) x38._1;
                hk.hku.cs.f2j.Closure x39 = x40;
                x39.x = x44;
                x39.apply();
                final java.lang.Integer temp45 = (java.lang.Integer) x39.out;
                out = temp45;
              }
              public hk.hku.cs.f2j.Closure clone ()
              {
                hk.hku.cs.f2j.Closure c = new Fun36();
                c.x = this.x;
                c.apply();
                return (hk.hku.cs.f2j.Closure) c;
              }
            }
            hk.hku.cs.f2j.Closure x36 = new Fun36();
            hk.hku.cs.f2j.Closure x35 = x36;
            x35.x = x22;
            x35.apply();
            final java.lang.Integer temp46 = (java.lang.Integer) x35.out;
            hk.hku.cs.f2j.tuples.Tuple2 x47 = new hk.hku.cs.f2j.tuples.Tuple2(temp34, temp46);
            out = x47;
          }
          public hk.hku.cs.f2j.Closure clone ()
          {
            hk.hku.cs.f2j.Closure c = new Fun20();
            c.x = this.x;
            c.apply();
            return (hk.hku.cs.f2j.Closure) c;
          }
        }
        hk.hku.cs.f2j.Closure x20 = new Fun20();
        hk.hku.cs.f2j.Closure x19 = x20;
        x19.x = x18;
        x19.apply();
        final hk.hku.cs.f2j.tuples.Tuple2 temp48 = (hk.hku.cs.f2j.tuples.Tuple2) x19.out;
        class Fun50 extends hk.hku.cs.f2j.Closure
        {
          hk.hku.cs.f2j.Closure x51 = this;
          public void apply ()
          {
            final hk.hku.cs.f2j.tuples.Tuple2 x52 = (hk.hku.cs.f2j.tuples.Tuple2) x51.x;
            class Fun54 extends hk.hku.cs.f2j.Closure
            {
              hk.hku.cs.f2j.Closure x55 = this;
              public void apply ()
              {
                final hk.hku.cs.f2j.tuples.Tuple2 x56 = (hk.hku.cs.f2j.tuples.Tuple2) x55.x;
                java.lang.String x57 = (java.lang.String) x56._1;
                out = x57;
              }
              public hk.hku.cs.f2j.Closure clone ()
              {
                hk.hku.cs.f2j.Closure c = new Fun54();
                c.x = this.x;
                c.apply();
                return (hk.hku.cs.f2j.Closure) c;
              }
            }
            hk.hku.cs.f2j.Closure x54 = new Fun54();
            hk.hku.cs.f2j.tuples.Tuple2 x58 = (hk.hku.cs.f2j.tuples.Tuple2) x52._1;
            hk.hku.cs.f2j.Closure x53 = x54;
            x53.x = x58;
            x53.apply();
            final java.lang.String temp59 = (java.lang.String) x53.out;
            out = temp59;
          }
          public hk.hku.cs.f2j.Closure clone ()
          {
            hk.hku.cs.f2j.Closure c = new Fun50();
            c.x = this.x;
            c.apply();
            return (hk.hku.cs.f2j.Closure) c;
          }
        }
        hk.hku.cs.f2j.Closure x50 = new Fun50();
        hk.hku.cs.f2j.Closure x49 = x50;
        x49.x = x18;
        x49.apply();
        final java.lang.String temp60 = (java.lang.String) x49.out;
        hk.hku.cs.f2j.tuples.Tuple2 x61 = new hk.hku.cs.f2j.tuples.Tuple2(temp48, temp60);
        out = x61;
      }
      public hk.hku.cs.f2j.Closure clone ()
      {
        hk.hku.cs.f2j.Closure c = new Fun16();
        c.x = this.x;
        c.apply();
        return (hk.hku.cs.f2j.Closure) c;
      }
    }
    hk.hku.cs.f2j.Closure x16 = new Fun16();
    hk.hku.cs.f2j.tuples.Tuple2 x62 = new hk.hku.cs.f2j.tuples.Tuple2("Bob", 30);
    hk.hku.cs.f2j.tuples.Tuple2 x63 = new hk.hku.cs.f2j.tuples.Tuple2(x62, "Haskell");
    hk.hku.cs.f2j.Closure x15 = x16;
    x15.x = x63;
    x15.apply();
    final hk.hku.cs.f2j.tuples.Tuple2 temp64 = (hk.hku.cs.f2j.tuples.Tuple2) x15.out;
    hk.hku.cs.f2j.Closure x0 = x1;
    x0.x = temp64;
    x0.apply();
    final java.lang.String temp65 = (java.lang.String) x0.out;
    return temp65;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}