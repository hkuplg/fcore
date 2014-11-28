package f2j.unbox;

public abstract class ClosureBoxBox implements Closure
{
  public Object arg;
  public Object res;
  public boolean hasApply = true;
  public abstract void apply ();
  public ClosureBoxBox clone () {
      return (ClosureBoxBox) ((Object) this.clone());
  }
}
