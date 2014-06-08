abstract class Closure
{
  Object x;
  Object out;
  abstract void apply ()
  ;
  public abstract Closure clone ()
  ;
}
public class Interfact
{
  static int apply ()
  {
    class Fun3 extends Closure
    {
      Closure x2 = this;
      void apply ()
      {
        class Fun4 extends Closure
        {
          Closure x5 = this;
          void apply ()
          {
            Integer temp7 = (Integer) x2.x;
            Object ifres6;
            if (temp7 == 0)
            {
               ifres6 = 1;
            }
            else
            {
              Integer temp9 = (Integer) x5.x;
              Object ifres8;
              if (temp9 == 0)
              {
                Closure x11 = ((Closure) x2).clone();
                x11.x = temp7;
                x11.apply();
                Closure temp12 = (Closure) x11.out;
                Closure x10 = (Closure) temp12;
                x10.x = 500;
                x10.apply();
                Integer temp13 = (Integer) x10.out;
                 ifres8 = temp13;
              }
              else
              {
                Closure x15 = ((Closure) x2).clone();
                x15.x = temp7 - 1;
                x15.apply();
                Closure temp16 = (Closure) x15.out;
                Closure x14 = (Closure) temp16;
                x14.x = temp9 - 1;
                x14.apply();
                Integer temp17 = (Integer) x14.out;
                 ifres8 = temp17;
              }
               ifres6 = ifres8;
            }
            out = ifres6;
          }
          public Closure clone ()
          {
            Closure c = new Fun4();
            c.x = this.x;
            c.apply();
            return (Closure) c;
          }
        }
        Closure x4 = new Fun4();
        out = x4;
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
    Closure temp18 = (Closure) x1.out;
    Closure x0 = (Closure) temp18;
    x0.x = 500;
    x0.apply();
    Integer temp19 = (Integer) x0.out;
    return temp19;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}