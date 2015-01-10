public class Pair
{
  static int apply ()
  {
    class Fun1 extends f2j.Closure
    {
      f2j.Closure x2 = this;
      public void apply ()
      {
        final f2j.tuples.Tuple2 x3 = (f2j.tuples.Tuple2) x2.arg;
        final f2j.tuples.Tuple2 x4 = (f2j.tuples.Tuple2) x3._1;
        final java.lang.Integer x5 = (java.lang.Integer) x4._1;
        final f2j.tuples.Tuple2 x6 = (f2j.tuples.Tuple2) x3._1;
        final java.lang.Integer x7 = (java.lang.Integer) x6._2;
        final java.lang.Integer x8 = x5 + x7;
        final f2j.tuples.Tuple2 x9 = (f2j.tuples.Tuple2) x3._2;
        final java.lang.Integer x10 = (java.lang.Integer) x9._1;
        final java.lang.Integer x11 = x8 + x10;
        final f2j.tuples.Tuple2 x12 = (f2j.tuples.Tuple2) x3._2;
        final java.lang.Integer x13 = (java.lang.Integer) x12._2;
        final java.lang.Integer x14 = x11 + x13;
        res = x14;
      }
    }
    final f2j.tuples.Tuple2 x15 = new f2j.tuples.Tuple2(1, 2);
    final f2j.tuples.Tuple2 x16 = new f2j.tuples.Tuple2(3, 4);
    final f2j.tuples.Tuple2 x17 = new f2j.tuples.Tuple2(x15, x16);
    final f2j.tuples.Tuple2 x18 = x17;
    final f2j.tuples.Tuple2 x19 = (f2j.tuples.Tuple2) x18._1;
    final f2j.tuples.Tuple2 x20 = x19;
    final java.lang.Integer x21 = (java.lang.Integer) x20._1;
    final java.lang.Integer x22 = x21;
    final java.lang.Integer x23 = (java.lang.Integer) x20._2;
    final java.lang.Integer x24 = x23;
    final f2j.tuples.Tuple2 x25 = new f2j.tuples.Tuple2(x22, x24);
    final f2j.tuples.Tuple2 x26 = (f2j.tuples.Tuple2) x18._2;
    final f2j.tuples.Tuple2 x27 = x26;
    final java.lang.Integer x28 = (java.lang.Integer) x27._1;
    final java.lang.Integer x29 = x28;
    final java.lang.Integer x30 = (java.lang.Integer) x27._2;
    final java.lang.Integer x31 = x30;
    final f2j.tuples.Tuple2 x32 = new f2j.tuples.Tuple2(x29, x31);
    final f2j.tuples.Tuple2 x33 = new f2j.tuples.Tuple2(x25, x32);
    f2j.Closure x34 = new Fun1();
    x34.arg = x33;
    x34.apply();
    final java.lang.Integer x35 = (java.lang.Integer) x34.res;
    return x35;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}