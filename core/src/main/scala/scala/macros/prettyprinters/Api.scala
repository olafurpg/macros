package scala.macros
package prettyprinters

import scala.macros.internal.prettyprinters._

private[macros] trait Api {
  implicit class XtensionSyntax[T](x: T) {
    def syntax(implicit syntax: Syntax[T]): String = {
      val p = new Prettyprinter
      syntax.render(p, x)
      p.toString
    }
  }

  implicit class XtensionStructure[T: Structure](x: T) {
    def structure(implicit structure: Structure[T]): String = {
      val p = new Prettyprinter
      structure.render(p, x)
      p.toString
    }
  }
}

private[macros] trait Aliases {
  // NOTE: We don't expose any definitions in this package
  // as part of the package's public API that will show up in scala.macros.
}
