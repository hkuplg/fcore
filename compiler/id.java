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
                     out = x2.x;
                   }
                 };
    out = x1;
  }
}