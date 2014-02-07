package systemf
import RecF._

object Translation {
	
  // global class definitions
  abstract class Global
  
  case class Arrow(x: Type, y: Type) extends Global //interface Arrow<X, Y> { public Y app(X x); }
  
  case class All(x: Type, t: Type) extends Global // interface All_X_XtoFREEVARS(T)-X<FREEVARS(T)-X> { public T tyapp<X>();}
  // public interface All<X> { public Object tyapp();}
  
  def translateTypes(t: Type): String = {
    t match {
      case TypeVar(x) => x
      case TypeFun(tauA, tauB) => "Arrow<" + translateTypes(tauA) + "," + translateTypes(tauB) + ">"
      case TypeForAll(x, a) => "All" // non-preserving
    }
  }
  
  type ClassDef = Map[String, String]
  
  var nameCounter = -1L
  val namePrefix = "C"
    
  def generateName: String = {
    nameCounter = nameCounter + 1
    namePrefix + nameCounter
  }
  
  def translateTerms(t: Term, env: Environment, arg: Boolean = false): Tuple2[String, ClassDef] = {
    t match {
      case TermVar(x: Idn) => {
        val tType = env._2(t)
        tType match {
          case TypeFun(_, _) => ("this", Map())
          case TypeVar(_) => if (arg) (x, Map()) else ("this." + x, Map())
        }
      }
      case TermFApp(m: Term, n: Term) => {
        val (e, d) = translateTerms(m, env)
        val (eP, dP) = translateTerms(n, env, true)
        
        (e + ".app(" + eP + ")", d ++ dP)
      }
      case TermRec(y: Term, yType: Type, x: Term, xType: Type, m: Term) => {
        val tType = env._2(t)
        tType match {
          case TypeFun(a, b) => {
            val newName = generateName
            
            val fType = translateTypes(tType)
            val aType = translateTypes(a)
            val bType = translateTypes(b)
            val (e, d) = translateTerms(m, env)
            
            val classDef = "class " + newName + "<???> extends " + fType + 
            	" {\n FREEVAR??? x;\n" + "public " + newName + "(FREEVAR??? x) { this.x = x;}" +
            	"public override " + bType + "app(" + aType + "x) { return " + e + "; } \n }"
            	
            ("new " + newName + "<FREEVAR??>(e???)", d + (newName -> classDef))
          } 
        }
      }
      case TermTypeApp(m: Term, a: Type) => {
        val mType = env._2(m)
        mType match {
          case TypeForAll(x, b) => {
            val (e, d) = translateTerms(m, env)
            val cast = translateTypes(substitute(a, x, b))
            ("(" + cast + ")" + e + ".tyapp()", d)
          }
        }
      }
      case TermCapLambda(x: TypeVar, m: Term) => {
        val tType = env._2(t)
        tType match {
          case TypeForAll(x, a) => {
            val newName = generateName
            
            val fType = translateTypes(tType)
            val aType = translateTypes(a)
            val (e, d) = translateTerms(m, env)
            
            val classDef = "class " + newName + "<???> extends " + fType + 
            	" {\n FREEVAR??? x;\n" + "public " + newName + "(FREEVAR??? x) { this.x = x;}" +
            	"public override " + aType + "tyapp() { return " + e + "; } \n }"
            	
            ("new " + newName + "<FREEVAR??>(e???)", d + (newName -> classDef))            
          }
        }
      }
    }
  }
  
  def getEnvironment(exp: FExpr): Environment = (Set(), Map())
  
  def translate(exp: FExpr): Tuple2[String, ClassDef] = {
    if (exp.isInstanceOf[Type]) {
      (translateTypes(exp.asInstanceOf[Type]), Map())
    } else {
      translateTerms(exp.asInstanceOf[Term], getEnvironment(exp))
    }
  }
  
}
