package hk.hku.cs.f2j;

public abstract class ClosureIntInt
{
  public int x;
  public int out;
  public abstract void apply ()
  ;
  public ClosureIntInt clone () {
      return (ClosureIntInt) ((Object) this.clone());
  }
}
