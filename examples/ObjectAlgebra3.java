public class ObjectAlgebra3
{
  static int apply ()
  {
    class Fun1 extends f2j.Closure
    {
      f2j.Closure x2 = this;
      public void apply ()
      {
        final java.lang.Integer x3 = (java.lang.Integer) x2.arg;
        res = x3;
      }
    }
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
            final java.lang.Integer x10 = x6 + x9;
            res = x10;
          }
        }
        res = new Fun7();
      }
    }
    final f2j.tuples.Tuple2 x11 = new f2j.tuples.Tuple2(new Fun1(), new Fun4());
    final f2j.tuples.Tuple2 x12 = x11;
    class Fun13 extends f2j.Closure
    {
      f2j.Closure x14 = this;
      public void apply ()
      {
        final f2j.tuples.Tuple2 x15 = (f2j.tuples.Tuple2) x14.arg;
        final f2j.tuples.Tuple2 x16 = x15;
        final f2j.Closure x17 = (f2j.Closure) x16._1;
        final f2j.Closure x18 = x17;
        final java.lang.Integer x19 = 5;
        f2j.Closure x20 = x18;
        x20.arg = x19;
        x20.apply();
        final Object x21 = x20.res;
        res = x21;
      }
    }
    class Fun22 extends f2j.Closure
    {
      f2j.Closure x23 = this;
      public void apply ()
      {
        final f2j.tuples.Tuple2 x24 = (f2j.tuples.Tuple2) x23.arg;
        final f2j.tuples.Tuple2 x25 = x24;
        final f2j.Closure x26 = (f2j.Closure) x25._2;
        final f2j.Closure x27 = x26;
        final f2j.tuples.Tuple2 x62 = x24;
        final f2j.Closure x63 = (f2j.Closure) x62._1;
        final f2j.Closure x64 = x63;
        final java.lang.Integer x65 = 2;
        f2j.Closure x66 = x64;
        x66.arg = x65;
        x66.apply();
        final Object x67 = x66.res;
        final Object x68 = x67;
        final f2j.tuples.Tuple2 x28 = x24;
        final f2j.tuples.Tuple2 x29 = x28;
        final f2j.Closure x30 = (f2j.Closure) x29._1;
        final f2j.Closure x31 = x30;
        class Fun32 extends f2j.Closure
        {
          f2j.Closure x33 = this;
          public void apply ()
          {
            final java.lang.Integer x34 = (java.lang.Integer) x33.arg;
            final java.lang.Integer x35 = x34;
            f2j.Closure x36 = x31;
            x36.arg = x35;
            x36.apply();
            final Object x37 = x36.res;
            final Object x38 = x37;
            res = x38;
          }
        }
        final f2j.tuples.Tuple2 x39 = x28;
        final f2j.Closure x40 = (f2j.Closure) x39._2;
        final f2j.Closure x41 = x40;
        class Fun42 extends f2j.Closure
        {
          f2j.Closure x43 = this;
          public void apply ()
          {
            final Object x44 = x43.arg;
            final Object x45 = x44;
            f2j.Closure x46 = x41;
            x46.arg = x45;
            x46.apply();
            final f2j.Closure x47 = (f2j.Closure) x46.res;
            final f2j.Closure x48 = x47;
            class Fun49 extends f2j.Closure
            {
              f2j.Closure x50 = this;
              public void apply ()
              {
                final Object x51 = x50.arg;
                final Object x52 = x51;
                f2j.Closure x53 = x48;
                x53.arg = x52;
                x53.apply();
                final Object x54 = x53.res;
                final Object x55 = x54;
                res = x55;
              }
            }
            res = new Fun49();
          }
        }
        final f2j.tuples.Tuple2 x56 = new f2j.tuples.Tuple2(new Fun32(), new Fun42());
        f2j.Closure x57 = new Fun13();
        x57.arg = x56;
        x57.apply();
        final Object x58 = x57.res;
        final Object x59 = x58;
        f2j.Closure x60 = x27;
        x60.arg = x59;
        x60.apply();
        final f2j.Closure x61 = (f2j.Closure) x60.res;
        f2j.Closure x69 = x61;
        x69.arg = x68;
        x69.apply();
        final Object x70 = x69.res;
        res = x70;
      }
    }
    final f2j.tuples.Tuple2 x71 = x12;
    final f2j.tuples.Tuple2 x72 = x71;
    final f2j.Closure x73 = (f2j.Closure) x72._1;
    final f2j.Closure x74 = x73;
    class Fun75 extends f2j.Closure
    {
      f2j.Closure x76 = this;
      public void apply ()
      {
        final java.lang.Integer x77 = (java.lang.Integer) x76.arg;
        final java.lang.Integer x78 = x77;
        f2j.Closure x79 = x74;
        x79.arg = x78;
        x79.apply();
        final java.lang.Integer x80 = (java.lang.Integer) x79.res;
        final java.lang.Integer x81 = x80;
        res = x81;
      }
    }
    final f2j.tuples.Tuple2 x82 = x71;
    final f2j.Closure x83 = (f2j.Closure) x82._2;
    final f2j.Closure x84 = x83;
    class Fun85 extends f2j.Closure
    {
      f2j.Closure x86 = this;
      public void apply ()
      {
        final java.lang.Integer x87 = (java.lang.Integer) x86.arg;
        final java.lang.Integer x88 = x87;
        f2j.Closure x89 = x84;
        x89.arg = x88;
        x89.apply();
        final f2j.Closure x90 = (f2j.Closure) x89.res;
        final f2j.Closure x91 = x90;
        class Fun92 extends f2j.Closure
        {
          f2j.Closure x93 = this;
          public void apply ()
          {
            final java.lang.Integer x94 = (java.lang.Integer) x93.arg;
            final java.lang.Integer x95 = x94;
            f2j.Closure x96 = x91;
            x96.arg = x95;
            x96.apply();
            final java.lang.Integer x97 = (java.lang.Integer) x96.res;
            final java.lang.Integer x98 = x97;
            res = x98;
          }
        }
        res = new Fun92();
      }
    }
    final f2j.tuples.Tuple2 x99 = new f2j.tuples.Tuple2(new Fun75(), new Fun85());
    f2j.Closure x100 = new Fun22();
    x100.arg = x99;
    x100.apply();
    final java.lang.Integer x101 = (java.lang.Integer) x100.res;
    return x101;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}