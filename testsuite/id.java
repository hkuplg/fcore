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
  static int apply ()
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
            Closure x8 = (Closure) x2.x;
            x8.x = 3;
            x8.apply();
            Integer temp9 = (Integer) x8.out;
            Closure x7 = ((Closure) x5.x).clone();
            x7.x = temp9;
            x7.apply();
            Closure temp10 = (Closure) x7.out;
            Closure x6 = (Closure) temp10;
            x6.x = 4;
            x6.apply();
            Integer temp11 = (Integer) x6.out;
            out = temp11;
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
        class Fun12 extends Closure
        {
          Closure x13 = this;
          void apply ()
          {
            class Fun14 extends Closure
            {
              Closure x15 = this;
              void apply ()
              {
                out = x13.x;
              }
              public Closure clone ()
              {
                Closure c = new Fun14();
                c.x = this.x;
                c.apply();
                return (Closure) c;
              }
            }
            Closure x14 = new Fun14();
            out = x14;
          }
          public Closure clone ()
          {
            Closure c = new Fun12();
            c.x = this.x;
            c.apply();
            return (Closure) c;
          }
        }
        Closure x12 = new Fun12();
        Closure x3 = (Closure) x4;
        x3.x = x12;
        x3.apply();
        Integer temp16 = (Integer) x3.out;
        out = temp16;
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
      Closure x18 = this;
      void apply ()
      {
        out = x18.x;
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
    Integer temp19 = (Integer) x0.out;
    return temp19;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}