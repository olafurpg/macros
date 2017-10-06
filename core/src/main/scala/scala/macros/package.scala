package scala

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
  private[macros] val universe = new ThreadLocal[core.Macro]
  private[macros] def makro = {
    if (universe.get == null) sys.error("this API can only be called in a macro expansion")
    universe.get
  }
  private implicit class XtensionBang[A](val a: A) extends AnyVal {
    @inline def unary_![B]: B = a.asInstanceOf[B]
  }
  type Expansion
  type Mirror
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
      def unapply(arg: Any): Option[String] = !makro.termNameUnapply(arg)
    }
    type Select <: Term.Ref
    object Select {
      def apply(qual: Term, name: Term.Name): Term.Select = !makro.termSelectApply(!qual, !name)
      def unapply(arg: Any): Option[(Term.Ref, Term.Name)] = !makro.termSelectUnapply(arg)
    }
    type Apply <: Term
    object Apply {
      def apply(fun: Term, args: List[Term]): Term.Apply = !makro.termApplyApply(!fun, !args)
      def unapply(arg: Any): Option[(Term.Ref, List[Term])] = !makro.termApplyUnapply(arg)
    }
  }
}
