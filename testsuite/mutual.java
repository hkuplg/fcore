abstract class Closure
{
  Object x;
  Object out;
  abstract void apply ()
  ;
  public abstract Closure clone ()
  ;
}
public class Mutual
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
            class Fun8 extends Closure
            {
              Closure x9 = this;
              void apply ()
              {
                class Fun10 extends Closure
                {
                  Closure x11 = this;
                  void apply ()
                  {
                    Integer temp13 = (Integer) x11.x;
                    Object ifres12;
                    if (temp13 == 0)
                    {
                       ifres12 = 1;
                    }
                    else
                    {
                      Closure x15 = ((Closure) x9.x).clone();
                      x15.x = 0;
                      x15.apply();
                      Closure temp16 = (Closure) x15.out;
                      Closure x14 = (Closure) temp16;
                      x14.x = temp13 - 1;
                      x14.apply();
                      Integer temp17 = (Integer) x14.out;
                      Integer temp18 = (Integer) temp17;
                       ifres12 = temp13 * temp18;
                    }
                    out = ifres12;
                  }
                  public Closure clone ()
                  {
                    Closure c = new Fun10();
                    c.x = this.x;
                    c.apply();
                    return (Closure) c;
                  }
                }
                Closure x10 = new Fun10();
                out = x10;
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
            Closure x7 = (Closure) x5.x;
            x7.x = x8;
            x7.apply();
            Closure temp19 = (Closure) x7.out;
            Closure x6 = (Closure) temp19;
            x6.x = 10;
            x6.apply();
            Integer temp20 = (Integer) x6.out;
            out = temp20;
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
        class Fun22 extends Closure
        {
          Closure x21 = this;
          void apply ()
          {
            class Fun24 extends Closure
            {
              Closure x25 = this;
              void apply ()
              {
                Closure x26 = (Closure) x21;
                x26.x = x21.x;
                x26.apply();
                Object temp27 = x26.out;
                out = temp27;
              }
              public Closure clone ()
              {
                Closure c = new Fun24();
                c.x = this.x;
                c.apply();
                return (Closure) c;
              }
            }
            Closure x24 = new Fun24();
            Closure x23 = (Closure) x21.x;
            x23.x = x24;
            x23.apply();
            Object temp28 = x23.out;
            out = temp28;
          }
          public Closure clone ()
          {
            Closure c = new Fun22();
            c.x = this.x;
            c.apply();
            return (Closure) c;
          }
        }
        Closure x22 = new Fun22();
        Closure x3 = (Closure) x4;
        x3.x = x22;
        x3.apply();
        Integer temp29 = (Integer) x3.out;
        out = temp29;
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
    class Fun31 extends Closure
    {
      Closure x30 = this;
      void apply ()
      {
        Closure x33 = (Closure) x30;
        x33.x = x30.x;
        x33.apply();
        Closure temp34 = (Closure) x33.out;
        Closure x32 = ((Closure) x30.x).clone();
        x32.x = temp34;
        x32.apply();
        Closure temp35 = (Closure) x32.out;
        out = temp35;
      }
      public Closure clone ()
      {
        Closure c = new Fun31();
        c.x = this.x;
        c.apply();
        return (Closure) c;
      }
    }
    Closure x31 = new Fun31();
    Closure x0 = (Closure) x1;
    x0.x = x31;
    x0.apply();
    Integer temp36 = (Integer) x0.out;
    return temp36;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}