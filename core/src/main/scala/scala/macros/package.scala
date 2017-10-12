package scala

import scala.language.implicitConversions

package object macros
    extends scala.macros.config.Api
    with scala.macros.config.Aliases
    with scala.macros.inputs.Api
    with scala.macros.inputs.Aliases
    with scala.macros.prettyprinters.Api
    with scala.macros.prettyprinters.Aliases
    with scala.macros.trees.Api
    with scala.macros.trees.Aliases
    with scala.macros.semantic.Api
    with scala.macros.semantic.Aliases {
  private[macros] val universe = new ThreadLocal[core.Universe]
  private[macros] def makro = {
    if (universe.get == null) sys.error("this API can only be called in a macro expansion")
    universe.get
  }
  private implicit class XtensionBang[A](val a: A) extends AnyVal {
    @inline def unary_![B]: B = a.asInstanceOf[B]
  }
  type Expansion
  type Mirror
  type Tree
  type Term
  implicit class XtensionTermRef(val term: Term) extends AnyVal {
    def select(name: String): Term.Select = Term.Select(term, Term.Name(name))
    def apply(args: List[Term]): Term.Apply = Term.Apply(term, args)
  }
  object Term {
    type Ref <: Term
    type Name <: Term.Ref
    object Name {
      def apply(value: String): Term.Name = !makro.termNameApply(!value)
    }
    type Select <: Term.Ref
    object Select {
      def apply(qual: Term, name: Term.Name): Term.Select = !makro.termSelectApply(!qual, !name)
    }
    type Apply <: Term
    object Apply {
      def apply(fun: Term, args: List[Term]): Term.Apply = !makro.termApplyApply(!fun, !args)
    }
  }

  trait Iso[A, B] {
    type In = A
    type Out = B
  }
  object Iso {
    def iso[A, B]: Iso[A, B] = new Iso[A, B] {}
    implicit val treeIso: Iso[tpd.Tree, Tree] = iso
    implicit val termIso: Iso[tpd.Term, Term] = iso
    implicit val termRefIso: Iso[tpd.Term.Ref, Term.Ref] = iso
  }

  implicit class XtensionTreeIso[A](val tree: A) extends AnyVal {
    def untyped[B](implicit iso: Iso[A, B]): iso.Out = !makro.tpd.termSpliceApply(!tree)
  }

  object tpd {
    type Tree
    type Term <: Tree
    object Term {
      type Ref <: Term
      type Name <: Term.Ref
      object Name {
        def unapply(arg: Any): Option[String] =
          !makro.tpd.termNameUnapply(arg)
      }
      type Select <: Term.Ref
      object Select {
        def unapply(arg: Any): Option[(tpd.Term.Ref, tpd.Term.Name)] =
          !makro.tpd.termSelectUnapply(arg)
      }
      type Apply <: Term
      object Apply {
        def unapply(arg: Any): Option[(tpd.Term.Ref, List[tpd.Term])] =
          !makro.tpd.termApplyUnapply(arg)
      }
    }

  }
}
