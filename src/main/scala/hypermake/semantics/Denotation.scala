package hypermake.semantics

trait ContextualDenotation[C, S, +D] {
  def defaultContext: C
  def denotation(form: S, ctx: C = defaultContext): D
}

/**
 * The syntax-semantics interface.
 * @tparam S Surface form
 * @tparam D Denotation
 */
trait Denotation[S, +D] {
  def denotation(form: S): D
}
