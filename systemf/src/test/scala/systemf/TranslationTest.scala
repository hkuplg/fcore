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
    
    // FIX NEW(x1: Y -> X): (Y -> X) -> X . x1 y

  // notation used in System F -> C# paper: y: TermVar, yType: Type, x: TermVar, xType: Type, m: Term
  // in Morrisett: fix x (x1: tau1): tau2 . e
  //case class TermRec(x: TermVar, tau2: Type, x1: TermVar, tau1: Type, e: Term) extends Term {    
    
    val xtype = TypeFun(TypeVar("Y"), TypeVar("X"))
    val body = TermFApp(TermVar("x"), TermVar("y"))

    val t = TypeFun(xtype, TypeVar("X"))
    val exp = TermRec(TermVar("f"), t, TermVar("x"), TypeFun(TypeVar("Y"), TypeVar("X")), body)
    
    val translated = "new C0<X,Y>(this.y)"
    val classdef = """class C0<X,Y> implements Arrow<Arrow<Y,X>,X> {Y y; public C0(Y y) { this.y = y; } public X app(Arrow<Y,X> x) { return x.app(this.y); }}"""
      val (tr, cd) = Translation.translate(exp)
    assert(translated === tr)
    assert(cd("C0") === classdef)
  }

  @Test
  def recursionTest() {
    // rec y(x:X):X . y(x)
    val body = TermFApp(TermVar("y"), TermVar("x"))
    val exp = TermRec(TermVar("y"), TypeVar("X"), TermVar("x"), TypeVar("X"), body)
    
    val translated = "(new C0<X>())"
    val classdef = """class C0<X> implements Arrow<X,X> { public X app(X x) { return x.app(x); }}"""
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
      
    // type-preserving stuff
    val classdef0 = """interface All_X_XtoY<Y> {
      public Arrow<X,Y> tyapp<X>();
  }""" 
    val classdef1P = """class C0<Y> implements All_X_XtoY<Y> {
      Y y;
      public C0(Y y) { this.y = y; }
      public <X> Arrow<X,Y> tyapp() { return new C1<X,Y>(this.y); }
  }"""
    val classdef2P = """class C1<X,Y> implements Arrow<X,Y> {
      Y y;
      public C1(Y y) { this.y = y; }
      public Y app(X x) { return y; }
  }"""
    
    // partial preserving
    val classdef1 = """class C0<Y> implements All {Y y; public C0(Y y) { this.y = y; }public <X> Arrow<X,Y> tyapp() { return new C1<X,Y>(this.y); } }"""
    val classdef2 = """class C1<X,Y> implements Arrow<X,Y> {Y y; public C1(Y y) { this.y = y; } public Y app(X x) { return this.y; }}"""      
 
      
      // /\X . \x:X . y with y:Y
      // :: Y -> (X -> Y) -> Y ????
     val translated = "new C0<Y>(this.y)"
    val fun = TermRec(TermVar(""), TypeFun(TypeVar("X"), TypeVar("Y")), TermVar("x"), TypeVar("Y"), TermVar("y"))    
    
    val exp = TermCapLambda(TypeVar("X"), fun)
    val (tr, cd) = Translation.translate(exp)
    assert(translated === tr)
    assert(cd("C0") === classdef1) 
    assert(cd("C1") === classdef2)    
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
  
  @Test
  def factorialTest() {
    //FIX f(n:INT):INT. IF0(n,1,n * f(n - 1)) 6
    
    val rec = TermPrimitiveOperation(TermVar("n"), Mult, TermFApp(TermVar("f"),TermPrimitiveOperation(TermVar("n"),Minus,TermInt(1))))
    val body = TermIF0(TermVar("n"), TermInt(1), rec)
    val fun = TermRec(TermVar("f"), TypeInt, TermVar("n"), TypeInt, body)
    val exp = TermFApp(fun, TermInt(6))
    
    val translated = "(new C0()).app(6)"
    val classdef = """class C0 implements Arrow<Integer,Integer> { public Integer app(Integer n) { if (n == 0) { return 1; } else { return n * this.app(n - 1); }; }}"""
    val (tr, cd) = Translation.translate(exp)
    assert(translated === tr)
    assert(cd("C0") === classdef)    
    
  }
  
}
