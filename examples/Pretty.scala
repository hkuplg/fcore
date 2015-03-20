object Pretty {
  trait Doc[A] {
    def concat(x: A, y: A): A
    def nil: A
    def text(x: String): A
  }

  object PrintAlg extends Doc[String] {
    def concat(x: String, y: String) = x concat y
    def nil = ""
    def text(x: String) = x
  }

  import PrintAlg._

  def parens[A](f: Doc[A], doc: A) = f.concat (f.concat (f.text("("), doc), f.text(")"))

  def hello[A](f: Doc[A]) = f.concat (f.text("Hello"), f.text(", world!"))

  def main(args: Array[String]) {
    println(parens [String] (PrintAlg, hello [String] (PrintAlg)))
  }
}
