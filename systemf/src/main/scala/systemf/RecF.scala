package systemf

object RecF {

  // identifies
  type Idn = String

  // operations
  sealed trait PrimitiveOperation
  case object Mult extends PrimitiveOperation
  case object Plus extends PrimitiveOperation
  case object Minus extends PrimitiveOperation  
  
  sealed abstract class FExpr {
    // free variables
    def FV: Set[TermVar]
    // free type variables
    def FTV: Set[TypeVar]
  }

  // types
  sealed abstract class Type extends FExpr {
    override def FV = Set[TermVar]()
  }
  case class TypeVar(x: Idn) extends Type {
    override def FTV = Set[TypeVar](this)
    override def toString() = x
  }
  case class TypeFun(tauA: Type, tauB: Type) extends Type {
    override def FTV = tauA.FTV ++ tauB.FTV
  }
  case class TypeForAll(x: TypeVar, tauA: Type) extends Type {
    override def FTV = tauA.FTV - x
  }
  object TypeInt extends TypeVar("INT")
  
  case class TypeTuple(taus: List[Type]) extends Type {
    override def FTV = taus.flatMap(x => x.FTV).toSet
  }

  // terms
  sealed abstract class Term extends FExpr {
    // explicit typing
    val tau: Type
  }
  case class TermVar(x: Idn) extends Term {
    // no inference yet
    val tau: TypeVar = TypeVar(x.toUpperCase())
    
    override def toString() = x

    override def FV = Set[TermVar](this)
    override def FTV = Set[TypeVar](tau)
  }
  case class TermFApp(m: Term, n: Term) extends Term {
    val tau = n.tau
    override def FV = m.FV ++ n.FV
    override def FTV = m.FTV ++ n.FTV
  }
    
  // notation used in System F -> C# paper: y: TermVar, yType: Type, x: TermVar, xType: Type, m: Term
  // in Morrisett: fix x (x1: tau1): tau2 . e
  case class TermRec(x: TermVar, tau2: Type, x1: TermVar, tau1: Type, e: Term) extends Term {
    val tau = tau2
    override def FV = (e.FV - x1) - x
    override def FTV = (e.FTV -- tau1.FTV) -- tau2.FTV
  }
  case class TermTypeApp(m: Term, a: Type) extends Term {
    val tau = m.tau match {
      case TypeForAll(x, b) => substitute(a, x, b)
    }
    override def FV = m.FV
    override def FTV = m.FTV ++ a.FTV
  }
  case class TermCapLambda(x: TypeVar, m: Term) extends Term { // type abstraction
    val tau = TypeForAll(x, m.tau)
    override def FV = m.FV
    override def FTV = m.FTV
  }
  case class TermInt(x: Integer) extends Term {
    // no inference yet
    val tau: TypeVar = TypeInt

    override def FV = Set[TermVar]()
    override def FTV = Set[TypeVar](tau)
  }
  case class TermTuple(es: List[Term]) extends Term {
    val tau: TypeTuple = TypeTuple(es.map(e => e.tau))

    override def FV = es.flatMap(e => e.FV).toSet
    override def FTV = es.flatMap(e => e.FTV).toSet 
  }
  // pi
  case class TermProjection(index: Integer, e: Term) extends Term {
    val tau: Type = e.tau match {case TypeTuple(taus) => taus(index)}
    override def FV = e.FV
    override def FTV = e.FTV    
  }
  case class TermPrimitiveOperation(e1: Term, op: PrimitiveOperation, e2: Term) extends Term {
    val tau: TypeVar = TypeInt
    override def FV = e1.FV ++ e2.FV
    override def FTV = e2.FTV ++ e2.FTV
  }
  case class TermIF0(e1: Term, e2: Term, e3: Term) extends Term {
    val tau = e2.tau
    override def FV = e1.FV ++ e2.FV ++ e3.FV
    override def FTV = e2.FTV ++ e2.FTV ++ e3.FTV
  }
  
  // helper functions
  def substitute(what: Type, forwhat: Type, in: Type): Type = {
    if (forwhat == in) {
      what
    } else {
      in match {
        case TypeVar(_) => in
        case TypeFun(a, b) => TypeFun(substitute(what, forwhat, a), substitute(what, forwhat, b))
        case TypeForAll(x, a) => TypeForAll(x, substitute(what, forwhat, a))
      }
    }
  }
  
  def setToString[A](s: Set[A], mapper: A => String = {x: A => x.toString}): String = if (s.isEmpty) "" else s.map(mapper).toList.sorted.reduce((x, y) => x + "," + y)
  
}
