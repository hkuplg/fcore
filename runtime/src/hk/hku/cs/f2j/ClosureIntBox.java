package hk.hku.cs.f2j;

public abstract class ClosureIntBox
{
  public int x;
  public Object out;
  public abstract void apply ()
  ;
  public ClosureIntBox clone () {
      return (ClosureIntBox) ((Object) this.clone());
  }
}
