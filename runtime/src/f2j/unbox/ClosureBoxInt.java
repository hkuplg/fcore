package f2j.unbox;

public abstract class ClosureBoxInt implements Closure
{
  public Object arg;
  public int res;
  public boolean hasApply = true;
  public abstract void apply () ;
  public ClosureBoxInt clone () {
      return (ClosureBoxInt) ((Object) this.clone());
  }
}
