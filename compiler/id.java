abstract class Closure
{
  Object x;
  Object out;
  abstract void apply ()
  ;
}
class MyClosure extends Closure
{
  void apply ()
  {
    Closure x1 = new Closure()
                 {
                   Closure x2 = this;
                   void apply ()
                   {
                     Closure x4 = new Closure()
                                  {
                                    Closure x5 = this;
                                    void apply ()
                                    {
                                      out = x5.x;
                                    }
                                  };
                     Closure x7 = new Closure()
                                  {
                                    Closure x8 = this;
                                    {
                                      out = new Closure()
                                            {
                                              Closure x10 = this;
                                              void apply ()
                                              {
                                                out = x8.x;
                                              }
                                            };
                                    }
                                    void apply ()
                                    {
                                    }
                                  };
                     Closure x3 = (Closure) x4;
                     x3.x = x7;
                     x3.apply();
                     out = x3.out;
                   }
                 };
    Closure x12 = new Closure()
                  {
                    Closure x13 = this;
                    void apply ()
                    {
                      out = x13.x;
                    }
                  };
    Closure x0 = (Closure) x1;
    x0.x = x12;
    x0.apply();
    out = x0.out;
  }
}