public class Issue_152
{
  static int apply ()
  {
    class Fun3 extends f2j.Closure
    {
      f2j.Closure x4 = this;
      public void apply ()
      {
        final Object x5 = x4.x;
        out = x5;
      }
      public f2j.Closure clone ()
      {
        f2j.Closure c = new Fun3();
        c.x = this.x;
        c.apply();
        return (f2j.Closure) c;
      }
    }
    f2j.Closure x3 = new Fun3();
    final f2j.Closure x0 = x3;
    final java.lang.Integer x1 = 1;
    return x1;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}