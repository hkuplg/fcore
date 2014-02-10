package systemf
import RecF._

object Translation {

  // global class definitions
  abstract class Global

  case class Arrow(x: Type, y: Type) extends Global //interface Arrow<X, Y> { public Y app(X x); }

  case class All(x: Type, t: Type) extends Global // interface All_X_XtoFREEVARS(T)-X<FREEVARS(T)-X> { public T tyapp<X>();}
  // public interface All { public <X> Object tyapp();}

  def translateTypes(t: Type): String = {
    t match {
      case TypeVar(x) => x
      case TypeFun(tauA, tauB) => "Arrow<" + translateTypes(tauA) + "," + translateTypes(tauB) + ">"
      case TypeForAll(x, a) => "All" // non-preserving
    }
  }

  type ClassDef = Map[String, String]

  private var nameCounter = -1L
  val namePrefix = "C"

  def resetCounter: Unit = nameCounter = -1L

  def generateName: String = {
    nameCounter = nameCounter + 1
    namePrefix + nameCounter
  }

  def generateFields(in: Set[String]): String = if (in.isEmpty) "" else in.map(x => x + "; ").reduce((x, y) => x + y)

  def generateConstructor(name: String, fv: Set[String], t: Term): String = {
    if (fv.isEmpty) ""
    else {
      "public " + name + "(" + fv.reduce((x, y) => x + "," + y) + ") { " +
        t.FV.map(x => "this." + x.toString + " = " + x + ";").reduce((x, y) => " ") + " }"
    }
  }

  def translateTerms(t: Term, arg: Boolean = false): Tuple2[String, ClassDef] = {
    t match {
      case TermVar(x: Idn) => {
        val tType = t.tau
        tType match {
          case TypeFun(_, _) => if (arg) (x, Map()) else ("this", Map())
          case TypeVar(_) => if (arg) (x, Map()) else ("this." + x, Map())
        }
      }
      case TermFApp(m: Term, n: Term) => {
        val (e, d) = translateTerms(m, true)
        val (eP, dP) = translateTerms(n)
        (e + ".app(" + eP + ")", d ++ dP)

      }
      case TermRec(y: Term, yType: Type, x: Term, xType: Type, m: Term) => {
        val tType = t.tau
        tType match {
          case TypeFun(a, b) => {
            val newName = generateName

            val fType = translateTypes(tType)
            val aType = translateTypes(a)
            val bType = translateTypes(b)
            val (e, d) = translateTerms(m, true)

            val freeTypeVars = setToString(tType.FTV)
            val freeVars = t.FV.map(x => x.tau.toString + " " + x.toString)

            val classDef = "class " + newName + "<" + freeTypeVars + "> implements " + fType +
              " {" + generateFields(freeVars) + generateConstructor(newName, freeVars, t) +
              " public " + bType + " app(" + aType + " " + x.toString + ") { return " + e + "; }}"

            ("new " + newName + "<" + freeTypeVars + ">(" + setToString(t.FV) + ")", d + (newName -> classDef))
          }
          case TypeVar(xTau) => {
            val newName = generateName

            val freeTypeVars = setToString(tType.FTV)
            val fType = translateTypes(TypeFun(tType, tType))
            val (e, d) =
              m match {
                case TermFApp(mP: Term, n: Term) => {
                  val (ePP, dPP) = translateTerms(m, true)
                  val (eP, dP) = translateTerms(n, true)
                  (eP + ".app(" + eP + ")", dPP ++ dP)
                }
              }

            val freeVars = t.FV.map(x => x.tau.toString + " " + x.toString)

            val classDef = "class " + newName + "<" + freeTypeVars + "> implements " + fType +
              " {" + generateFields(freeVars) + generateConstructor(newName, freeVars, t) +
              " public " + tType + " app(" + tType + " " + x.toString + ") { return " + e + "; }}"

            ("new " + newName + "<" + freeTypeVars + ">(" + setToString(t.FV) + ")", d + (newName -> classDef))
          }
        }
      }
      case TermTypeApp(m: Term, a: Type) => {
        val mType = m.tau
        mType match {
          case TypeForAll(x, b) => {
            val (e, d) = translateTerms(m)
            val cast = translateTypes(substitute(a, x, b))
            ("(" + cast + ")" + e + ".tyapp()", d)
          }
        }
      }
      case TermCapLambda(x: TypeVar, m: Term) => {
        def generateBound(a: Set[TypeVar], b: Set[TypeVar]): String = {
          val diff = a -- b
          if (diff.isEmpty) ""
          else {
            "<" + setToString(diff) + "> "
          }
        }
        
        val tType = t.tau
        tType match {
          case TypeForAll(x, a) => {
            val newName = generateName

            val fType = translateTypes(tType)
            val aType = translateTypes(a)
            val (e, d) = translateTerms(m, true)

            val freeTypeVars = setToString(tType.FTV)
            val freeVars = t.FV.map(x => x.tau.toString + " " + x.toString)
            
            val classDef = "class " + newName + "<" + freeTypeVars + "> implements " + fType +
              " {" + generateFields(freeVars) + generateConstructor(newName, freeVars, t) +
              "public " + generateBound(a.FTV, tType.FTV) + aType + " tyapp() { return " + e + "; } }"

            ("new " + newName + "<" + freeTypeVars + ">(" + setToString(t.FV) + ")", d + (newName -> classDef))
          }
        }
      }
    }
  }

  def translate(exp: FExpr): Tuple2[String, ClassDef] = {
    if (exp.isInstanceOf[Type]) {
      (translateTypes(exp.asInstanceOf[Type]), Map())
    } else {
      translateTerms(exp.asInstanceOf[Term])
    }
  }

}
