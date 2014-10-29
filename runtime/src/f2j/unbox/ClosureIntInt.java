package f2j.unbox;

public abstract class ClosureIntInt implements Closure
{
  public long x;
  public long out;
  public abstract void apply ()
  ;
  public ClosureIntInt clone () {
      return (ClosureIntInt) ((Object) this.clone());
  }
}
