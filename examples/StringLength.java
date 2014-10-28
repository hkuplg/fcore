public class StringLength
{
  static int apply ()
  {
    class Fun2 extends f2j.Closure
    {
      f2j.Closure x3 = this;
      public void apply ()
      {
        final java.lang.String x4 = (java.lang.String) x3.x;
        final java.lang.Integer x5 = x4.length();
        out = x5;
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
    f2j.Closure x6 = x0;
    x6.x = "abcd";
    x6.apply();
    final java.lang.Integer temp7 = (java.lang.Integer) x6.out;
    final java.lang.Integer x1 = temp7;
    return x1;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}