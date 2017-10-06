package scala.macros.core

// This is the API to be implemented by compiler engines.
// This api is not macro-author facing.
trait Macro {
  type Mirror

  type Term
  type TermRef

  type TermName
  def termNameApply(value: String): TermName
  def termNameUnapply(arg: Any): Option[String]

  type TermSelect
  def termSelectApply(qual: Term, name: TermName): TermSelect
  def termSelectUnapply(arg: Any): Option[(TermRef, TermName)]

  type TermApply
  def termApplyApply(fun: Term, args: List[Term]): TermApply
  def termApplyUnapply(arg: Any): Option[(Term, List[Term])]
}
