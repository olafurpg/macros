package scala.macros.tests.scaladays

import scala.macros._

class Optional[A >: Null](val value: A) extends AnyVal {
  def isEmpty = value == null
}

object OptionalM {
  def getOrElse[A >: Null, B >: A](o: Optional[A], alt: => B): B = macro {
    //    val prefix: Term = this
    val ifTerm = Term.If(
      o.select("isEmpty"),
      alt,
      o.select("value")
    )
    ifTerm
  }
}
