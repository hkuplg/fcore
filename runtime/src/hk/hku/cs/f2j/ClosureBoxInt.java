package hk.hku.cs.f2j;

public abstract class ClosureBoxInt extends Closure
{
  public Object x;
  public int out;
  public abstract void apply ()
  ;
  public ClosureBoxInt clone () {
      return (ClosureBoxInt) ((Object) this.clone());
  }
}
