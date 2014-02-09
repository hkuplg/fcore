import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit._
import systemf.RecF._
import systemf.Translation

class TranslationTest extends AssertionsForJUnit {
  
  @Before
  def reset() {
    Translation.resetCounter
  }
  
  @Test
  def freeVariableTest() {
    
    // \x: Y -> X . x y with y: Y
    // == rec NEW(x: Y -> X): (Y -> X) -> X . x y

    val xtype = TypeFun(TypeVar("Y"), TypeVar("X"))
    val body = TermFApp(TermVar("x"), TermVar("y"))

    val t = TypeFun(xtype, TypeVar("X"))
    val exp = TermRec(TermVar(""), t, TermVar("x"), TypeFun(TypeVar("Y"), TypeVar("X")), body)
    
    val translated = "new C0<X,Y>(y)"
    val classdef = """class C0<X,Y> extends Arrow<Arrow<Y,X>,X> {Y y; public C0(Y y) { this.y = y; } public override X app(Arrow<Y,X> x) { return x.app(this.y); }}"""
      val (tr, cd) = Translation.translate(exp)
    assert(translated === tr)
    assert(cd("C0") === classdef)
  }

  @Test
  def recursionTest() {
    // rec y(x:X):X . y(x)
    val body = TermFApp(TermVar("y"), TermVar("x"))
    val exp = TermRec(TermVar("y"), TypeVar("X"), TermVar("x"), TypeVar("X"), body)
    
    val translated = "new C0<X>()"
    val classdef = """class C0<X> extends Arrow<X,X> { public override X app(X x) { return x.app(x); }}"""
      val (tr, cd) = Translation.translate(exp)
    assert(translated === tr)
    assert(cd("C0") === classdef)    
    
  }

  @Test
  def polymorphism1Test() {  
        // forall X . X -> X
    val translated = "All_X_XtoX"
    val classdef = """interface All_X_XtoX {
      public Arrow<X,X> tyapp<X>();
  }"""    
      val exp = TypeForAll(TypeVar("X"), TypeFun(TypeVar("X"),TypeVar("X")))
    val (tr, cd) = Translation.translate(exp)
    assert("All" === tr)
  }
  
  @Test
  def polymorphism2Test() {  
        // forall X . X -> Y
    val translated = "All_X_XtoY<Y>"
    val classdef = """interface All_X_XtoY<Y> {
      public Arrow<X,Y> tyapp<X>();
  }"""
    val exp = TypeForAll(TypeVar("X"), TypeFun(TypeVar("X"),TypeVar("Y")))
    val (tr, cd) = Translation.translate(exp)
    assert("All" === tr)    
  }  

  @Test
  def polymorphism3Test() {  
        // /\X . \x:X . y with y:Y
    val translated = "new C0<Y>(y)"
    val classdef0 = """interface All_X_XtoY<Y> {
      public Arrow<X,Y> tyapp<X>();
  }"""      
    val classdef1 = """class C0<Y> extends All_X_XtoY<Y> {
      Y y;
      public C0(Y y) { this.y = y; }
      public override Arrow<X,Y> tyapp<X>() { return new C1<X,Y>(this.y); }
  }"""
    val classdef2 = """class C1<X,Y> extends Arrow<X,Y> {
      Y y;
      public C1(Y y) { this.y = y; }
      public override Y app(X x) { return y; }
  }""" 
    
    // ???
    val fun = TermRec(TermVar(""), TypeVar("X"), TermVar("y"), TypeVar("Y"), TermVar("y"))    
    
    val exp = TermCapLambda(TypeVar("X"), fun)
    val (tr, cd) = Translation.translate(exp)
    assert(translated === tr)    
  }    

  @Test
  def polymorphism4Test() {  
        // forall X . X -> forall Z . Z
    val translated = "All_X_XtoY<All_X_XtoX>"
    val classdef0 = """interface All_X_XtoY<Y> {
      public Arrow<X,Y> tyapp<X>();
  }"""
    val classdef1 = """interface All_X_XtoX {
      public Arrow<X,X> tyapp<X>();
  }"""   

    val exp = TypeForAll(TypeVar("X"), TypeFun(TypeVar("X"), TypeForAll(TypeVar("Z"), TypeVar("Z"))))
    val (tr, cd) = Translation.translate(exp)
    assert("All" === tr)
      
  }    
  
}
