package scala.macros.tests.utest

import scala.macros._

case class TestValue(name: String, tpe: String, value: Any) {
  override def toString: String = s"$name: $tpe = $value"
}
case class AssertEntry[T](label: String, thunk: (TestValue => Unit) => T)

object Asserts {

  def assert(cond: Boolean): Unit = macro {
    val prefix = Tracer.root.select("Asserts" :: "assertImpl" :: Nil)
    Tracer.trace(prefix, cond)
  }

  def assertImpl(assertEntry: AssertEntry[Boolean]): Unit = {
    val logged = new StringBuilder
    val x = assertEntry.thunk { value =>
      logged.append('\n')
      logged.append("  ")
      logged.append(value.toString)
    }
    if (logged.nonEmpty) {
      logged.append("\n")
    }
    Predef.assert(x, assertEntry.label + logged.toString())
  }

}

object Tracer {
  def test[T](expr: T): (T, List[TestValue]) = macro {
    val prefix = Tracer.root.select("Tracer" :: "testImpl" :: Nil)
    Tracer.trace(prefix, expr)
  }

  def testImpl[T](entries: AssertEntry[T]): (T, List[TestValue]) = {
    val buf = List.newBuilder[TestValue]
    val result = entries.thunk(value => buf += value)
    result -> buf.result()
  }

  def root = Term.Name("_root_").select("scala" :: "macros" :: "tests" :: "utest" :: Nil)

  def trace(prefix: Term, expr: Term)(implicit m: Mirror): Term = {
    val loggerName = Term.fresh("log")
    val arg = expr.transform {
      case term @ Term.Name(name) =>
        val T = term.tpe.widen
        val tmp = Term.fresh(name)
        val bind = Defn.Val(Nil, tmp, Some(T), {
          // NOTE(olafur) Danger! Here we corrupt the owner chain by embedding
          // term under a new fresh definition tmp.
          term
        })
        val testValue = root
          .select("TestValue")
          .apply(
            Lit.String(name) ::
              Lit.String(T.syntax) ::
              tmp ::
              Nil
          )
        Term.Block(
          bind ::
            loggerName.apply(testValue :: Nil) ::
            tmp ::
            Nil
        )
    }
    val thunk = Term.Function(
      Term.Param(
        Nil,
        loggerName,
        None,
        None
      ) :: Nil,
      arg
    )
    val logEntry = root
      .select("AssertEntry")
      .apply(
        Lit.String(expr.pos.lineContent.trim) ::
          thunk ::
          Nil
      )
    prefix.apply(logEntry :: Nil)
  }
}
