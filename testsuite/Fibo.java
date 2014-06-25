abstract class Closure
{
  Object x;
  Object out;
  abstract void apply ()
  ;
  public abstract Closure clone ()
  ;
}
public class Fibo
{
  static int apply ()
  {
    class Fun2 extends Closure
    {
      Closure x1 = this;
      void apply ()
      {
        Integer temp4 = (Integer) x1.x;
        Integer temp5 = (Integer) (temp4 - 2);
        Object ifres3;
        if (temp5 == 0)
        {
           ifres3 = 1;
        }
        else
        {
          Integer temp7 = (Integer) (temp4 - 1);
          Object ifres6;
          if (temp7 == 0)
          {
             ifres6 = 1;
          }
          else
          {
            Closure x8 = (Closure) x1;
            x8.x = temp7;
            x8.apply();
            Integer temp9 = (Integer) x8.out;
            Closure x11 = (Closure) x1;
            x11.x = temp5;
            x11.apply();
            Integer temp12 = (Integer) x11.out;
            Integer temp10 = (Integer) temp9;
            Integer temp13 = (Integer) temp12;
             ifres6 = temp10 + temp13;
          }
           ifres3 = ifres6;
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
    Integer temp14 = (Integer) x0.out;
    return temp14;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}