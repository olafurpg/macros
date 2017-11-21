package scala.macros.tests

import scala.macros._

// Adapted from https://docs.scala-lang.org/overviews/macros/overview.html
object FormatInterpolator {
  def printf(format: String, arg: Any*): Unit = macro {
    println(arg)
    var evals = List.empty[Defn]
    def precompute(value: Term, tpe: TypeTree): Term = {
      val freshName = Term.fresh("eval$")
      evals = Defn.Val(Nil, freshName, Some(tpe), value) :: evals
      Term.Name(freshName)
    }

    val TypeInt = Type.typeRef("scala.Int").toTypeTree
    val TypeString = Type.typeRef("java.lang.String").toTypeTree

    def p(term: Term) =
      Term.Name("_root_").select("scala").select("Predef").select("print").apply(term :: Nil)

    format match {
      case Lit.String(s_format) =>
        var paramsStack: List[Term] = arg.map(tpd2splice).toList
        def pop(): Term = {
          val head :: tail = paramsStack
          paramsStack = tail
          head
        }
        val refs = s_format.split("(?<=%[\\w%])|(?=%[\\w%])").map {
          case "%d" => precompute(pop(), TypeInt)
          case "%s" => precompute(pop(), TypeString)
          case "%%" => Lit.String("%")
          case part => Lit.String(part)
        }
        //        pprint.log(arg.syntax)
        Term.Block(evals ++ refs.map(p))
    }
  }
}
