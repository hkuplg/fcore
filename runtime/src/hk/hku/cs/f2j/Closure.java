package hk.hku.cs.f2j;

public abstract class Closure
{
  Object x;
  Object out;
  abstract void apply ()
  ;
  public Closure clone () {
      return (Closure) ((Object) this.clone());
  }
}