public class ObjectAlgebra
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
        final java.lang.Integer x15 = (java.lang.Integer) x14.arg;
        class Fun16 extends f2j.Closure
        {
          f2j.Closure x17 = this;
          public void apply ()
          {
            final java.lang.Integer x18 = (java.lang.Integer) x17.arg;
            final java.lang.Integer x19 = x15 + x18;
            res = x19;
          }
        }
        res = new Fun16();
      }
    }
    final f2j.tuples.Tuple2 x20 = new f2j.tuples.Tuple2(x12, new Fun13());
    final f2j.tuples.Tuple2 x21 = x20;
    class Fun22 extends f2j.Closure
    {
      f2j.Closure x23 = this;
      public void apply ()
      {
        final f2j.tuples.Tuple2 x24 = (f2j.tuples.Tuple2) x23.arg;
        final f2j.tuples.Tuple2 x25 = x24;
        final f2j.Closure x26 = (f2j.Closure) x25._1;
        final f2j.Closure x27 = x26;
        final java.lang.Integer x28 = 5;
        f2j.Closure x29 = x27;
        x29.arg = x28;
        x29.apply();
        final Object x30 = x29.res;
        res = x30;
      }
    }
    class Fun31 extends f2j.Closure
    {
      f2j.Closure x32 = this;
      public void apply ()
      {
        final f2j.tuples.Tuple2 x33 = (f2j.tuples.Tuple2) x32.arg;
        final f2j.tuples.Tuple2 x34 = x33;
        final f2j.Closure x35 = (f2j.Closure) x34._2;
        final f2j.Closure x36 = x35;
        final f2j.tuples.Tuple2 x75 = x33;
        final f2j.tuples.Tuple2 x76 = (f2j.tuples.Tuple2) x75._1;
        final f2j.tuples.Tuple2 x77 = x76;
        final f2j.Closure x78 = (f2j.Closure) x77._1;
        final f2j.Closure x79 = x78;
        final java.lang.Integer x80 = 2;
        f2j.Closure x81 = x79;
        x81.arg = x80;
        x81.apply();
        final Object x82 = x81.res;
        final Object x83 = x82;
        final f2j.tuples.Tuple2 x37 = x33;
        final f2j.tuples.Tuple2 x38 = x37;
        final f2j.tuples.Tuple2 x39 = (f2j.tuples.Tuple2) x38._1;
        final f2j.tuples.Tuple2 x40 = x39;
        final f2j.Closure x41 = (f2j.Closure) x40._1;
        final f2j.Closure x42 = x41;
        class Fun43 extends f2j.Closure
        {
          f2j.Closure x44 = this;
          public void apply ()
          {
            final java.lang.Integer x45 = (java.lang.Integer) x44.arg;
            final java.lang.Integer x46 = x45;
            f2j.Closure x47 = x42;
            x47.arg = x46;
            x47.apply();
            final Object x48 = x47.res;
            final Object x49 = x48;
            res = x49;
          }
        }
        final f2j.tuples.Tuple2 x50 = x37;
        final f2j.tuples.Tuple2 x51 = (f2j.tuples.Tuple2) x50._1;
        final f2j.tuples.Tuple2 x52 = x51;
        final f2j.Closure x53 = (f2j.Closure) x52._2;
        final f2j.Closure x54 = x53;
        class Fun55 extends f2j.Closure
        {
          f2j.Closure x56 = this;
          public void apply ()
          {
            final Object x57 = x56.arg;
            final Object x58 = x57;
            f2j.Closure x59 = x54;
            x59.arg = x58;
            x59.apply();
            final f2j.Closure x60 = (f2j.Closure) x59.res;
            final f2j.Closure x61 = x60;
            class Fun62 extends f2j.Closure
            {
              f2j.Closure x63 = this;
              public void apply ()
              {
                final Object x64 = x63.arg;
                final Object x65 = x64;
                f2j.Closure x66 = x61;
                x66.arg = x65;
                x66.apply();
                final Object x67 = x66.res;
                final Object x68 = x67;
                res = x68;
              }
            }
            res = new Fun62();
          }
        }
        final f2j.tuples.Tuple2 x69 = new f2j.tuples.Tuple2(new Fun43(), new Fun55());
        f2j.Closure x70 = new Fun22();
        x70.arg = x69;
        x70.apply();
        final Object x71 = x70.res;
        final Object x72 = x71;
        f2j.Closure x73 = x36;
        x73.arg = x72;
        x73.apply();
        final f2j.Closure x74 = (f2j.Closure) x73.res;
        f2j.Closure x84 = x74;
        x84.arg = x83;
        x84.apply();
        final Object x85 = x84.res;
        res = x85;
      }
    }
    final f2j.tuples.Tuple2 x86 = x21;
    final f2j.tuples.Tuple2 x87 = x86;
    final f2j.tuples.Tuple2 x88 = x87;
    final f2j.tuples.Tuple2 x89 = (f2j.tuples.Tuple2) x88._1;
    final f2j.tuples.Tuple2 x90 = x89;
    final f2j.Closure x91 = (f2j.Closure) x90._1;
    final f2j.Closure x92 = x91;
    class Fun93 extends f2j.Closure
    {
      f2j.Closure x94 = this;
      public void apply ()
      {
        final java.lang.Integer x95 = (java.lang.Integer) x94.arg;
        final java.lang.Integer x96 = x95;
        f2j.Closure x97 = x92;
        x97.arg = x96;
        x97.apply();
        final java.lang.Integer x98 = (java.lang.Integer) x97.res;
        final java.lang.Integer x99 = x98;
        res = x99;
      }
    }
    final f2j.tuples.Tuple2 x100 = x87;
    final f2j.tuples.Tuple2 x101 = (f2j.tuples.Tuple2) x100._1;
    final f2j.tuples.Tuple2 x102 = x101;
    final f2j.Closure x103 = (f2j.Closure) x102._2;
    final f2j.Closure x104 = x103;
    class Fun105 extends f2j.Closure
    {
      f2j.Closure x106 = this;
      public void apply ()
      {
        final java.lang.Integer x107 = (java.lang.Integer) x106.arg;
        final java.lang.Integer x108 = x107;
        f2j.Closure x109 = x104;
        x109.arg = x108;
        x109.apply();
        final f2j.Closure x110 = (f2j.Closure) x109.res;
        final f2j.Closure x111 = x110;
        class Fun112 extends f2j.Closure
        {
          f2j.Closure x113 = this;
          public void apply ()
          {
            final java.lang.Integer x114 = (java.lang.Integer) x113.arg;
            final java.lang.Integer x115 = x114;
            f2j.Closure x116 = x111;
            x116.arg = x115;
            x116.apply();
            final java.lang.Integer x117 = (java.lang.Integer) x116.res;
            final java.lang.Integer x118 = x117;
            res = x118;
          }
        }
        res = new Fun112();
      }
    }
    final f2j.tuples.Tuple2 x119 = new f2j.tuples.Tuple2(new Fun93(), new Fun105());
    final f2j.tuples.Tuple2 x120 = x86;
    final f2j.Closure x121 = (f2j.Closure) x120._2;
    final f2j.Closure x122 = x121;
    class Fun123 extends f2j.Closure
    {
      f2j.Closure x124 = this;
      public void apply ()
      {
        final java.lang.Integer x125 = (java.lang.Integer) x124.arg;
        final java.lang.Integer x126 = x125;
        f2j.Closure x127 = x122;
        x127.arg = x126;
        x127.apply();
        final f2j.Closure x128 = (f2j.Closure) x127.res;
        final f2j.Closure x129 = x128;
        class Fun130 extends f2j.Closure
        {
          f2j.Closure x131 = this;
          public void apply ()
          {
            final java.lang.Integer x132 = (java.lang.Integer) x131.arg;
            final java.lang.Integer x133 = x132;
            f2j.Closure x134 = x129;
            x134.arg = x133;
            x134.apply();
            final java.lang.Integer x135 = (java.lang.Integer) x134.res;
            final java.lang.Integer x136 = x135;
            res = x136;
          }
        }
        res = new Fun130();
      }
    }
    final f2j.tuples.Tuple2 x137 = new f2j.tuples.Tuple2(x119, new Fun123());
    f2j.Closure x138 = new Fun31();
    x138.arg = x137;
    x138.apply();
    final java.lang.Integer x139 = (java.lang.Integer) x138.res;
    return x139;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}