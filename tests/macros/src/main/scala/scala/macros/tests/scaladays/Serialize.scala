package scala.macros.tests
package scaladays

import scala.macros._

trait Serialize[T] {
  def apply(x: T): String
}

object Serialize {
  def apply[T](fn: T => String) = new Serialize[T] { def apply(x: T) = fn(x) }
  implicit def int: Serialize[Int] = Serialize { x =>
    x.toString
  }
  implicit def string: Serialize[String] = Serialize { x =>
    "\"" + x + "\""
  }

  def materialize[T]: Serialize[T] = macro {
    val instance = Term.fresh("instance")
    val param = Term.fresh("x")
    val buf = Term.fresh("buf")
    val fieldSerialization = {
      val serializerss = T.vals.filter(_.isCase).map { f =>
        val namePart = Lit.String("\"" + f.name.value + "\": ")
        val appendName = Term.Apply(Term.Select(buf, Term.Name("++=")), namePart :: Nil)
        val valueRef = Term.Select(param, Term.Name(f.sym))
        val valuePart =
          Term.Apply(
            Term.Select(
              Term.ApplyType(
                Term.Select(
                  Term.Select(
                    Term.Select(Term.Name("_root_"), Term.Name("scala")),
                    Term.Name("Predef")
                  ),
                  Term.Name("implicitly")
                ),
                List(Type.Apply(Type.Name("Serialize"), List(f.info)))
              ),
              Term.Name("apply")
            ),
            List(valueRef)
          )
        val appendValue = Term.Apply(Term.Select(buf, Term.Name("++=")), valuePart :: Nil)
        List(appendName, appendValue)
      }
      val separators = serializerss.map(_ => q"""$buf ++= ", """")
      serializerss.zip(separators).map({ case (ss, s) => ss :+ s }).flatten.dropRight(1)
    }
    val defnObject = Defn.Object(
      List(Mod.Implicit()),
      instance,
      Template(
        Nil,
        List(Init(Type.Apply(Type.Name("Serialize"), List(T)), Name(""), Nil)),
        Self(Name(""), None),
        List(
          Defn.Def(
            Nil,
            Term.Name("apply"),
            Nil,
            List(List(Term.Param(Nil, param, Some(T), None))),
            Some(
              Type.Select(
                Term.Select(Term.Select(Term.Name("_root_"), Term.Name("java")), Term.Name("lang")),
                Type.Name("String")
              )
            ),
            Term.Block(
              Defn.Val(
                Nil,
                List(Pat.Var(buf)),
                None,
                Term.New(
                  Init(
                    Type.Select(
                      Term.Select(Term.Name("_root_"), Term.Name("scala")),
                      Type.Name("StringBuilder")
                    ),
                    Name(""),
                    Nil
                  )
                )
              ) ::
                Term.ApplyInfix(buf, Term.Name("++="), Nil, List(Lit.String("{ "))) ::
                (fieldSerialization ++ (Term
                .ApplyInfix(buf, Term.Name("++="), Nil, List(Lit.String(" }"))) ::
                Term.Select(buf, Term.Name("toString")) ::
                Nil))
            )
          )
        )
      )
    )
    Term.Block(
      defnObject ::
        instance ::
        Nil
    )
  }
}
