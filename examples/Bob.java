public class Bob
{
  static Object apply ()
  {
    final f2j.tuples.Tuple2 x1 = new f2j.tuples.Tuple2("Bob", 30);
    final f2j.tuples.Tuple2 x2 = new f2j.tuples.Tuple2(x1, "Haskell");
    final f2j.tuples.Tuple2 x3 = x2;
    final f2j.tuples.Tuple2 x4 = x3;
    final java.lang.String x5 = (java.lang.String) x4._2;
    final java.lang.String x6 = x5;
    return x6;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}