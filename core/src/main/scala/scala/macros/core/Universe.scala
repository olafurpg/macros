package scala.macros.core

import Utils._

object Utils {
  def unsupported = throw new UnsupportedOperationException()
}

// This is the API to be implemented by compiler engines.
// This api is not macro-author facing.
trait Universe { untpd =>

  // =============
  // Semantic
  // =============
  type Mirror

  // =============
  // Trees
  // =============
  type Tree

  type Term
  type TermRef

  type TermName
  def termNameApply(value: String): TermName = unsupported

  type TermSelect
  def termSelectApply(qual: Term, name: TermName): TermSelect = unsupported

  type TermApply
  def termApplyApply(fun: Term, args: List[Term]): TermApply = unsupported

  type LitBoolean
  def litBooleanApply(value: Boolean): LitBoolean = unsupported

  type LitInt
  def litIntApply(value: Int): LitBoolean = unsupported

  type LitString
  def litStringApply(value: String): LitBoolean = unsupported

  // =============
  // Typed Trees
  // =============
  val tpd: TypedImpl = new TypedImpl {}
  trait TypedImpl {
    type Tree
    type Term
    type TermName
    def termNameUnapply(arg: Any): Option[String] = unsupported

    type TermSelect
    def termSelectUnapply(arg: Any): Option[(TermRef, TermName)] = unsupported

    type TermApply
    def termApplyUnapply(arg: Any): Option[(Term, List[Term])] = unsupported

    type TermSplice
    def termSpliceApply(tree: tpd.Term): untpd.Term = unsupported
  }
}
