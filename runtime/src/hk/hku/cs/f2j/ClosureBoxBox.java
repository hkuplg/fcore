package hk.hku.cs.f2j;

public abstract class ClosureBoxBox
{
  public Object x;
  public Object out;
  public abstract void apply ()
  ;
  public ClosureBoxBox clone () {
      return (ClosureBoxBox) ((Object) this.clone());
  }
}
