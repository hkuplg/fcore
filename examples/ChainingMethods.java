public class ChainingMethods
{
  static boolean apply ()
  {
    final java.text.StringCharacterIterator x2 = new <java.lang.String> java.text.StringCharacterIterator("");
    final java.text.StringCharacterIterator x0 = x2;
    final java.lang.Character x3 = (java.lang.Character) java.text.CharacterIterator.DONE;
    final java.lang.Character x4 = x0.current();
    final java.lang.Boolean x5 = x4.<java.lang.Character>equals(x3);
    final java.lang.Boolean x1 = x5;
    return x1;
  }
  public static void main (String[] args)
  {
    System.out.println(apply());
  }
}