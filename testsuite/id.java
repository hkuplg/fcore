abstract class Closure
{
  Object x;
  Object out;
  abstract void apply ()
  ;
  public abstract Closure clone ()
  ;
}
public class Id
{
  static Closure apply ()
  {
    class Fun1 extends Closure
    {
      Closure x2 = this;
      void apply ()
      {
        class Fun4 extends Closure
        {
          Closure x5 = this;
          void apply ()
          {
            out = x5.x;
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
        class Fun6 extends Closure
        {
          Closure x7 = this;
          void apply ()
          {
            class Fun8 extends Closure
            {
              Closure x9 = this;
              void apply ()
              {
                out = x7.x;
              }
              public Closure clone ()
              {
                Closure c = new Fun8();
                c.x = this.x;
                c.apply();
                return (Closure) c;
              }
            }
            Closure x8 = new Fun8();
            out = x8;
          }
          public Closure clone ()
          {
            Closure c = new Fun6();
            c.x = this.x;
            c.apply();
            return (Closure) c;
          }
        }
        Closure x6 = new Fun6();
        Closure x3 = (Closure) x4;
        x3.x = x6;
        x3.apply();
        Closure temp10 = (Closure) x3.out;
        out = temp10;
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
    class Fun11 extends Closure
    {
      Closure x12 = this;
      void apply ()
      {
        out = x12.x;
      }
      public Closure clone ()
      {
        Closure c = new Fun11();
        c.x = this.x;
        c.apply();
        return (Closure) c;
      }
    }
    Closure x11 = new Fun11();
    Closure x0 = (Closure) x1;
    x0.x = x11;
    x0.apply();
    Closure temp13 = (Closure) x0.out;
    return temp13;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}