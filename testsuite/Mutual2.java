abstract class Closure
{
  Object x;
  Object out;
  abstract void apply ()
  ;
  public abstract Closure clone ()
  ;
}
public class Mutual2
{
  static int apply ()
  {
    class Fun1 extends Closure
    {
      Closure x2 = this;
      void apply ()
      {
        class Fun5 extends Closure
        {
          Closure x6 = this;
          void apply ()
          {
            class Fun7 extends Closure
            {
              Closure x8 = this;
              void apply ()
              {
                Integer temp10 = (Integer) x8.x;
                Object ifres9;
                if (temp10 == 0)
                {
                   ifres9 = 1;
                }
                else
                {
                  Closure x11 = (Closure) x6.x;
                  x11.x = temp10 - 1;
                  x11.apply();
                  Integer temp12 = (Integer) x11.out;
                  Integer temp13 = (Integer) temp12;
                   ifres9 = temp10 * temp13;
                }
                out = ifres9;
              }
              public Closure clone ()
              {
                Closure c = new Fun7();
                c.x = this.x;
                c.apply();
                return (Closure) c;
              }
            }
            Closure x7 = new Fun7();
            out = x7;
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
        Closure x4 = ((Closure) x2.x).clone();
        x4.x = x5;
        x4.apply();
        Closure temp14 = (Closure) x4.out;
        Closure x3 = (Closure) temp14;
        x3.x = 10;
        x3.apply();
        Integer temp15 = (Integer) x3.out;
        out = temp15;
      }
      public Closure clone ()
      {
        Closure c = new Fun1();
        c.x = this.x;
        c.apply();
        return (Closure) c;
      }
    }
    Closure x1 = new Fun1();
    class Fun17 extends Closure
    {
      Closure x16 = this;
      void apply ()
      {
        Closure x19 = (Closure) x16;
        x19.x = x16.x;
        x19.apply();
        Closure temp20 = (Closure) x19.out;
        Closure x18 = ((Closure) x16.x).clone();
        x18.x = temp20;
        x18.apply();
        Closure temp21 = (Closure) x18.out;
        out = temp21;
      }
      public Closure clone ()
      {
        Closure c = new Fun17();
        c.x = this.x;
        c.apply();
        return (Closure) c;
      }
    }
    Closure x17 = new Fun17();
    Closure x0 = (Closure) x1;
    x0.x = x17;
    x0.apply();
    Integer temp22 = (Integer) x0.out;
    return temp22;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}