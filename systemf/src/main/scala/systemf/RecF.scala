package systemf

object RecF {

  // identifies
  type Idn = String
  
  abstract class FExpr
  
  // types
  abstract class Type extends FExpr
  case class TypeVar(x: Idn) extends Type
  case class TypeFun(tauA: Type, tauB: Type) extends Type
  case class TypeForAll(x: TypeVar, tauA: Type) extends Type
  
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
  
  // terms
  abstract class Term extends FExpr
  case class TermVar(x: Idn) extends Term
  case class TermFApp(m: Term, n: Term) extends Term
  case class TermRec(y: Term, yType: Type, x: Term, xType: Type, m: Term) extends Term
  case class TermTypeApp(m: Term, a: Type) extends Term
  case class TermCapLambda(x: TypeVar, m: Term) extends Term // type abstraction
  
  // values
  type Value = Either[TermRec, TermCapLambda]

  // typing environment - gamma
  type TypeContext = Set[Type]
  type ValueContext = Map[Term, Type]
  type Environment = Tuple2[TypeContext, ValueContext]

}
