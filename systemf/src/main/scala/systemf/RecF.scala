package systemf

object RecF {

  // identifies
  type Idn = String

  abstract class FExpr {
    // free variables
    def FV: Set[TermVar]
    // free type variables
    def FTV: Set[TypeVar]
  }

  // types
  abstract class Type extends FExpr {
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
  
  def setToString[A](s: Set[A]): String = s.map(x => x.toString).toList.sorted.reduce((x, y) => x + "," + y)

  // terms
  abstract class Term extends FExpr {
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
  case class TermRec(y: TermVar, yType: Type, x: TermVar, xType: Type, m: Term) extends Term {
    val tau = yType
    override def FV = (m.FV - x) - y
    override def FTV = (m.FTV -- xType.FTV) -- yType.FTV
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
  // values
  type Value = Either[TermRec, TermCapLambda]

  // typing environment - gamma
  type TypeContext = Set[Type]
  type ValueContext = Map[Term, Type]
  type Environment = Tuple2[TypeContext, ValueContext]

}
