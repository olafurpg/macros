package scala.macros.internal
package engines.scalac

import scala.macros.core.Macro
import scala.tools.nsc.Global
import scala.macros.internal.engines.scalac.semantic._
import scala.macros.internal.engines.scalac.trees._
import scala.reflect.macros.whitebox

case class Universe(val g: Global)
    extends scala.macros.Universe
    with Semantic
    with Trees
    with Expansions {
  type Abstracts = TreeAbstracts with MirrorAbstracts with ExpansionAbstracts
  object abstracts extends TreeAbstracts with MirrorAbstracts with ExpansionAbstracts
}

case class ScalacMacro(val g: Global) extends Macro {
  import g._
  case class Mirror(c: whitebox.Context)
  case class Expansion(c: whitebox.Context)
  type Term = Tree
  type TermRef = Tree

  type TermName = c.TermName
  def termNameApply(value: String): TermName = new c.TermName(value)
  def termNameUnapply(arg: Any): Option[String] = arg match {
    case name: c.TermName => Some(name.value)
    case _ => None
  }

  type TermSelect = g.Select
  def termSelectApply(qual: TermRef, name: TermName): TermSelect =
    Select(qual, name.name.toTermName)
  def termSelectUnapply(arg: Any): Option[(TermRef, TermName)] = arg match {
    case Select(qual, name) => Some(qual -> new c.TermName(name.decoded))
    case _ => None
  }

  type TermApply = g.Apply
  def termApplyApply(fun: Term, args: List[Term]): TermApply = Apply(fun, args)
  def termApplyUnapply(arg: Any): Option[(Term, List[Term])] = arg match {
    case Apply(fun, args) => Some(fun -> args)
    case _ => None
  }

  val c: customTrees.type = customTrees
  object customTrees {
    sealed trait Name extends g.RefTree {
      def value: String
      def qualifier: g.Tree = g.EmptyTree
      def name: g.Name = {
        if (this.isInstanceOf[TypeName]) g.TypeName(value).encode
        else g.TermName(value).encode
      }
    }

    class TermName(val value: String) extends g.Ident(g.TermName(value).encode) with Name {
      override def qualifier: g.Tree = super[Name].qualifier
      override val name: g.Name = super[Name].name
    }

  }
}
