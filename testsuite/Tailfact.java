abstract class Closure
{
  Object x;
  Object out;
  abstract void apply ()
  ;
  public abstract Closure clone ()
  ;
}
public class Tailfact
{
  static int apply ()
  {
    class Fun3 extends Closure
    {
      Closure x4 = this;
      void apply ()
      {
        class Fun5 extends Closure
        {
          Closure x2 = this;
          void apply ()
          {
            Integer temp7 = (Integer) x4.x;
            Object ifres6;
            if (temp7 == 0)
            {
               ifres6 = x2.x;
            }
            else
            {
              Closure x9 = ((Closure) x2).clone();
              x9.x = temp7 - 1;
              x9.apply();
              Closure temp10 = (Closure) x9.out;
              Integer temp11 = (Integer) x2.x;
              Closure x8 = (Closure) temp10;
              x8.x = temp11 * temp7;
              x8.apply();
              Closure temp12 = (Closure) x8.out;
               ifres6 = temp12;
            }
            out = ifres6;
          }
          public Closure clone ()
          {
            Closure c = new Fun5();
            c.x = this.x;
            c.apply();
            return (Closure) c;
          }
        }
        Closure x5 = new Fun5();
        out = x5;
      }
      public Closure clone ()
      {
        Closure c = new Fun3();
        c.x = this.x;
        c.apply();
        return (Closure) c;
      }
    }
    Closure x3 = new Fun3();
    Closure x1 = ((Closure) x3).clone();
    x1.x = 10;
    x1.apply();
    Closure temp13 = (Closure) x1.out;
    Closure x0 = (Closure) temp13;
    x0.x = 1;
    x0.apply();
    Integer temp14 = (Integer) x0.out;
    return temp14;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}