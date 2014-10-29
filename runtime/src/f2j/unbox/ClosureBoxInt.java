package f2j.unbox;

public abstract class ClosureBoxInt implements Closure
{
  public Object x;
  public long out;
  public abstract void apply ()
  ;
  public ClosureBoxInt clone () {
      return (ClosureBoxInt) ((Object) this.clone());
  }
}
