public class Pairs
{
  static int apply ()
  {
    class Fun1 extends f2j.Closure
    {
      f2j.Closure x2 = this;
      public void apply ()
      {
        final java.lang.Integer x3 = (java.lang.Integer) x2.arg;
        class Fun4 extends f2j.Closure
        {
          f2j.Closure x5 = this;
          public void apply ()
          {
            final java.lang.Integer x6 = (java.lang.Integer) x5.arg;
            final java.lang.Integer x7 = x3 + x6;
            res = x7;
          }
        }
        res = new Fun4();
      }
    }
    class Fun8 extends f2j.Closure
    {
      f2j.Closure x9 = this;
      public void apply ()
      {
        final java.lang.Integer x10 = (java.lang.Integer) x9.arg;
        class Fun11 extends f2j.Closure
        {
          f2j.Closure x12 = this;
          public void apply ()
          {
            final java.lang.Integer x13 = (java.lang.Integer) x12.arg;
            final java.lang.Integer x14 = x10 * x13;
            res = x14;
          }
        }
        res = new Fun11();
      }
    }
    class Fun15 extends f2j.Closure
    {
      f2j.Closure x16 = this;
      public void apply ()
      {
        final f2j.Closure x17 = (f2j.Closure) x16.arg;
        class Fun18 extends f2j.Closure
        {
          f2j.Closure x19 = this;
          public void apply ()
          {
            final f2j.tuples.Tuple2 x20 = (f2j.tuples.Tuple2) x19.arg;
            final f2j.tuples.Tuple2 x42 = (f2j.tuples.Tuple2) x20._2;
            final java.lang.Integer x43 = (java.lang.Integer) x42._2;
            final java.lang.Integer x44 = x43;
            final f2j.tuples.Tuple2 x34 = (f2j.tuples.Tuple2) x20._2;
            final java.lang.Integer x35 = (java.lang.Integer) x34._1;
            final java.lang.Integer x36 = x35;
            final f2j.tuples.Tuple2 x26 = (f2j.tuples.Tuple2) x20._1;
            final java.lang.Integer x27 = (java.lang.Integer) x26._2;
            final java.lang.Integer x28 = x27;
            final f2j.tuples.Tuple2 x21 = (f2j.tuples.Tuple2) x20._1;
            final java.lang.Integer x22 = (java.lang.Integer) x21._1;
            final java.lang.Integer x23 = x22;
            f2j.Closure x24 = x17;
            x24.arg = x23;
            x24.apply();
            final f2j.Closure x25 = (f2j.Closure) x24.res;
            f2j.Closure x29 = x25;
            x29.arg = x28;
            x29.apply();
            final java.lang.Integer x30 = (java.lang.Integer) x29.res;
            final java.lang.Integer x31 = x30;
            f2j.Closure x32 = x17;
            x32.arg = x31;
            x32.apply();
            final f2j.Closure x33 = (f2j.Closure) x32.res;
            f2j.Closure x37 = x33;
            x37.arg = x36;
            x37.apply();
            final java.lang.Integer x38 = (java.lang.Integer) x37.res;
            final java.lang.Integer x39 = x38;
            f2j.Closure x40 = x17;
            x40.arg = x39;
            x40.apply();
            final f2j.Closure x41 = (f2j.Closure) x40.res;
            f2j.Closure x45 = x41;
            x45.arg = x44;
            x45.apply();
            final java.lang.Integer x46 = (java.lang.Integer) x45.res;
            res = x46;
          }
        }
        res = new Fun18();
      }
    }
    final f2j.tuples.Tuple2 x63 = new f2j.tuples.Tuple2(1, 2);
    final f2j.tuples.Tuple2 x64 = new f2j.tuples.Tuple2(3, 4);
    final f2j.tuples.Tuple2 x65 = new f2j.tuples.Tuple2(x63, x64);
    final f2j.tuples.Tuple2 x66 = x65;
    final f2j.tuples.Tuple2 x67 = (f2j.tuples.Tuple2) x66._1;
    final f2j.tuples.Tuple2 x68 = x67;
    final java.lang.Integer x69 = (java.lang.Integer) x68._1;
    final java.lang.Integer x70 = x69;
    final java.lang.Integer x71 = (java.lang.Integer) x68._2;
    final java.lang.Integer x72 = x71;
    final f2j.tuples.Tuple2 x73 = new f2j.tuples.Tuple2(x70, x72);
    final f2j.tuples.Tuple2 x74 = (f2j.tuples.Tuple2) x66._2;
    final f2j.tuples.Tuple2 x75 = x74;
    final java.lang.Integer x76 = (java.lang.Integer) x75._1;
    final java.lang.Integer x77 = x76;
    final java.lang.Integer x78 = (java.lang.Integer) x75._2;
    final java.lang.Integer x79 = x78;
    final f2j.tuples.Tuple2 x80 = new f2j.tuples.Tuple2(x77, x79);
    final f2j.tuples.Tuple2 x81 = new f2j.tuples.Tuple2(x73, x80);
    class Fun47 extends f2j.Closure
    {
      f2j.Closure x48 = this;
      public void apply ()
      {
        final java.lang.Integer x49 = (java.lang.Integer) x48.arg;
        final java.lang.Integer x50 = x49;
        f2j.Closure x51 = new Fun1();
        x51.arg = x50;
        x51.apply();
        final f2j.Closure x52 = (f2j.Closure) x51.res;
        final f2j.Closure x53 = x52;
        class Fun54 extends f2j.Closure
        {
          f2j.Closure x55 = this;
          public void apply ()
          {
            final java.lang.Integer x56 = (java.lang.Integer) x55.arg;
            final java.lang.Integer x57 = x56;
            f2j.Closure x58 = x53;
            x58.arg = x57;
            x58.apply();
            final java.lang.Integer x59 = (java.lang.Integer) x58.res;
            final java.lang.Integer x60 = x59;
            res = x60;
          }
        }
        res = new Fun54();
      }
    }
    f2j.Closure x61 = new Fun15();
    x61.arg = new Fun47();
    x61.apply();
    final f2j.Closure x62 = (f2j.Closure) x61.res;
    f2j.Closure x82 = x62;
    x82.arg = x81;
    x82.apply();
    final java.lang.Integer x83 = (java.lang.Integer) x82.res;
    return x83;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}