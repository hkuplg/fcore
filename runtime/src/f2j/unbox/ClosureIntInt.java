package f2j.unbox;

public abstract class ClosureIntInt implements Closure
{
  public int x;
  public int out;
  public boolean hasApply = true;
  public abstract void apply () ;
  public ClosureIntInt clone () {
      return (ClosureIntInt) ((Object) this.clone());
  }
}
