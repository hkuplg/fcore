package f2j.unbox;

public abstract class ClosureBoxBox implements Closure
{
  public Object x;
  public Object out;
  public abstract void apply ()
  ;
  public ClosureBoxBox clone () {
      return (ClosureBoxBox) ((Object) this.clone());
  }
}
