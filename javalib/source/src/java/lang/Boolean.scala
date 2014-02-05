package java.lang

class Boolean(private val value: scala.Boolean) {
  def booleanValue(): scala.Boolean = value

  override def equals(that: Any) =
    that.isInstanceOf[Boolean] && (value == that.asInstanceOf[Boolean].value)

  override def toString: String = if (value) "true" else "false"

  override def hashCode = if (value) 1231 else 1237

  /*
   * Methods on scala.Boolean
   * The following methods are only here to properly support reflective calls
   * on boxed primitive values. YOU WILL NOT BE ABLE TO USE THESE METHODS, since
   * we use the true javalib to lookup symbols, this file contains only
   * implementations.
   */
    /**
   * Negates a Boolean expression.
   *
   * - `!a` results in `false` if and only if `a` evaluates to `true` and
   * - `!a` results in `true` if and only if `a` evaluates to `false`.
   *
   * @return the negated expression
   */
  def unary_! : Boolean = !value

  /**
    * Compares two Boolean expressions and returns `true` if one or both of them evaluate to true.
    *
    * `a || b` returns `true` if and only if
    *  - `a` is `true` or
    *  - `b` is `true` or
    *  - `a` and `b` are `true`.
    *
    * @note This method uses 'short-circuit' evaluation and
    *       behaves as if it was declared as `def ||(x: => Boolean): Boolean`.
    *       If `a` evaluates to `true`, `true` is returned without evaluating `b`.
    */
  def ||(x: Boolean): Boolean = value || x

  /**
    * Compares two Boolean expressions and returns `true` if both of them evaluate to true.
    *
    * `a && b` returns `true` if and only if
    *  - `a` and `b` are `true`.
    *
    * @note This method uses 'short-circuit' evaluation and
    *       behaves as if it was declared as `def &&(x: => Boolean): Boolean`.
    *       If `a` evaluates to `false`, `false` is returned without evaluating `b`.
    */
  def &&(x: Boolean): Boolean = value && x

  // Compiler won't build with these seemingly more accurate signatures
  // def ||(x: => Boolean): Boolean
  // def &&(x: => Boolean): Boolean

  /**
    * Compares two Boolean expressions and returns `true` if one or both of them evaluate to true.
    *
    * `a | b` returns `true` if and only if
    *  - `a` is `true` or
    *  - `b` is `true` or
    *  - `a` and `b` are `true`.
    *
    * @note This method evaluates both `a` and `b`, even if the result is already determined after evaluating `a`.
    */
  def |(x: Boolean): Boolean = value | x

  /**
    * Compares two Boolean expressions and returns `true` if both of them evaluate to true.
    *
    * `a & b` returns `true` if and only if
    *  - `a` and `b` are `true`.
    *
    * @note This method evaluates both `a` and `b`, even if the result is already determined after evaluating `a`.
    */
  def &(x: Boolean): Boolean = value & x

  /**
    * Compares two Boolean expressions and returns `true` if they evaluate to a different value.
    *
    * `a ^ b` returns `true` if and only if
    *  - `a` is `true` and `b` is `false` or
    *  - `a` is `false` and `b` is `true`.
    */
  def ^(x: Boolean): Boolean = value ^ x

}

object Boolean {
  val TYPE = classOf[scala.Boolean]
  val TRUE = new Boolean(true)
  val FALSE = new Boolean(false)

  def valueOf(booleanValue: scala.Boolean) = if (booleanValue) TRUE else FALSE

  def toString(b: scala.Boolean) = if (b) "true" else "false"
}
