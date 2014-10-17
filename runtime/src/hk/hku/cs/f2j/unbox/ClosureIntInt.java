package hk.hku.cs.f2j.unbox;

public abstract class ClosureIntInt implements Closure
{
  public int x;
  public int out;
  public abstract void apply ()
  ;
  public ClosureIntInt clone () {
      return (ClosureIntInt) ((Object) this.clone());
  }
}
