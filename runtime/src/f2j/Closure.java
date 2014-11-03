package f2j;

public abstract class Closure
{
  public Object x;
  public Object out;
  public boolean hasApply = true;
  public abstract void apply ();
  public Closure clone () {
      return (Closure) ((Object) this.clone());
  }
}
