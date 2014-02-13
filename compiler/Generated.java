package translation;

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
    Closure x2 = new Closure()
                 {
                   Closure x3 = this;
                   {
                	   out =  new Closure()
                       {
                           Closure x5 = this;
                           void apply ()
                           {
                             Closure x6 = (Closure) x3.x;
                             x6.x = x5.x;
                             x6.apply();
                             out = x6.out;
                           }
                         };
                   }
                   void apply ()
                   {
                   }
                 };
    Closure x3 = new Closure()
                 {
                   Closure x4 = this;
                   void apply ()
                   {
                     out = x4.x;
                   }
                 };
    Closure x1 = (Closure) x2;
    x1.x = x3;
    x1.apply();
    out = x1.out;
  }
}

public class Test {
	public static void main(String args[]) {
		MyClosure c = new MyClosure();
		c.apply();
		Closure c1 = (Closure) c.out;
		c1.x = 300;
		c1.apply();
		System.out.println(c1.out);
	}
}