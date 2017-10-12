package scala.macros.internal
package engines.scalac

import scala.macros.core.Universe
import scala.tools.nsc.Global
import scala.reflect.macros.whitebox

case class ScalacUniverse(g: Global) extends Universe { untpd =>
  import g._
  case class Mirror(c: whitebox.Context)
  case class Expansion(c: whitebox.Context)
  override type Tree = g.Tree
  override type Term = g.Tree
  override type TermRef = g.Tree

  override type TermName = c.TermName
  override def termNameApply(value: String): TermName = new c.TermName(value)

  override type TermSelect = g.Select
  override def termSelectApply(qual: TermRef, name: TermName): TermSelect =
    Select(qual, name.name.toTermName)

  override type TermApply = g.Apply
  override def termApplyApply(fun: Term, args: List[Term]): TermApply = Apply(fun, args)

  override val tpd: TypedImpl.type = TypedImpl
  object TypedImpl extends TypedImpl {
    override type Tree = g.Tree
    override type Term = g.Tree
    override type TermName = c.TermName
    override def termNameUnapply(arg: Any): Option[String] = arg match {
      case name: c.TermName => Some(name.value)
      case _ => None
    }

    override type TermSelect = g.Select
    override def termSelectUnapply(arg: Any): Option[(TermRef, TermName)] = arg match {
      case Select(qual, name) => Some(qual -> new c.TermName(name.decoded))
      case _ => None
    }

    override type TermApply = g.Apply
    override def termApplyUnapply(arg: Any): Option[(Term, List[Term])] = arg match {
      case Apply(fun, args) => Some(fun -> args)
      case _ => None
    }

    override type TermSplice = g.Tree
    override def termSpliceApply(tree: tpd.Term): untpd.Term = tree
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
