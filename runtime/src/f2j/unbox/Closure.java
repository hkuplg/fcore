package f2j.unbox;

public abstract class Closure
{
  public Object oarg;
  public Object ores;
  public long larg;
  public long lres;  
  public boolean hasApply = true;
  public abstract void apply ();
  public Closure clone () {
      return (Closure) ((Object) this.clone());
  }
}
