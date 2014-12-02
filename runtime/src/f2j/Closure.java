package f2j;

public abstract class Closure
{
  public Object arg;
  public Object res;
  public boolean hasApply = true;
  public abstract void apply ();
  public Closure clone () {
      return (Closure) ((Object) this.clone());
  }
}
