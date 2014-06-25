abstract class Closure
{
  Object x;
  Object out;
  abstract void apply ()
  ;
  public abstract Closure clone ()
  ;
}
public class Fact
{
  static int apply ()
  {
    class Fun2 extends Closure
    {
      Closure x1 = this;
      void apply ()
      {
        Integer temp4 = (Integer) x1.x;
        Object ifres3;
        if (temp4 == 0)
        {
           ifres3 = 1;
        }
        else
        {
          Closure x5 = (Closure) x1;
          x5.x = temp4 - 1;
          x5.apply();
          Integer temp6 = (Integer) x5.out;
          Integer temp7 = (Integer) temp6;
           ifres3 = temp4 * temp7;
        }
        out = ifres3;
      }
      public Closure clone ()
      {
        Closure c = new Fun2();
        c.x = this.x;
        c.apply();
        return (Closure) c;
      }
    }
    Closure x2 = new Fun2();
    Closure x0 = (Closure) x2;
    x0.x = 10;
    x0.apply();
    Integer temp8 = (Integer) x0.out;
    return temp8;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}