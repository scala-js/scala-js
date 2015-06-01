package sbt.testing

import java.util.Arrays
import java.io.Serializable

/** A bundle of information used to request a <code>Task</code> from a test
 *  framework.
 *
 *  An array of <code>TaskDef</code> is passed to
 *  <a href="Runner.html"><code>Runner</code></a>'s <code>tasks</code> method,
 *  which returns an array of <code>Tasks</code>.  Each returned task, when
 *  executed, will run tests and suites determined by the test class name,
 *  fingerprints, "explicitly specified" field, and selectors of one of the
 *  passed <code>TaskDef</code>s.
 *
 *  The "Explicitly specified" field means the user supplied a complete fully
 *  qualified test name, such as with the command:
 *
 *  {{{
 *  &gt; test-only com.mycompany.myproject.WholeNameSpec
 *  }}}
 *
 *  as opposed to commands like:
 *
 *  {{{
 *  &gt; test-only *WholeNameSpec
 *  }}}
 *
 *  or simply:
 *
 *  {{{
 *  &gt; test
 *  }}}
 *
 *  The <code>explicitlySpecified</code> field will be true for in the first
 *  case, and false in the last two cases, because only in the first case was
 *  the fully qualified test class name completely specified by the user. The
 *  test framework can use this information to decide whether to ignore an
 *  annotation requesting a class not be discovered.
 *
 *  The <code>fingerprint</code> parameter indicates how the test suite was
 *  identified as a test suite. This <code>tasks</code> method may be called
 *  with <code>TaskDef</code>s containing the same value for
 *  <code>testClassName</code> but different fingerprints. For example, if both
 *  a class and its companion object were test classes, the <code>tasks</code>
 *  method could be passed an array containing <code>TaskDef</code>s with the
 *  same name but with a different value for <code>fingerprint.isModule</code>.
 *
 *  A test framework may "reject" a requested task by returning no
 *  <code>Task</code> for that <code>TaskDef</code>.
 *
 *  @param fullyQualifiedName the fully qualified name of the test class to be
 *            run by the requested task
 *  @param fingerprint indicates how the test suite was identified as a test
 *            suite
 *  @param explicitlySpecified indicates whether the test class was explicitly
 *            specified by user.
 *  @param selectors a possibly empty array of <code>Selectors</code>
 *            determining suites and tests to run
 */
final class TaskDef(_fullyQualifiedName: String, _fingerprint: Fingerprint,
    _explicitlySpecified: Boolean,
    _selectors: Array[Selector]) extends Serializable {

  if (_fullyQualifiedName == null)
    throw new NullPointerException("fullyQualifiedName was null");
  if (_fingerprint == null)
    throw new NullPointerException("fingerprint was null");
  if (_selectors == null)
    throw new NullPointerException("selectors was null");

  /** The fully qualified name of the test class requested by this
   *  <code>TaskDef</code>.
   */
  def fullyQualifiedName(): String = _fullyQualifiedName

  /** The fingerprint that the test class requested by this
   *  <code>TaskDef</code> matches.
   */
  def fingerprint(): Fingerprint = _fingerprint

  /** Indicates whether or not the test class requested by this
   *  <code>TaskDef</code> was "explicitly specified."
   *
   *  For more information on what explicitly specified means, see the main
   *  documentation for this class.
   */
  def explicitlySpecified(): Boolean = _explicitlySpecified

  /** One to many selectors describing the nature of the <code>Task</code>
   *  requested by this <code>TaskDef</code>.
   *
   *  A <code>Selector</code> can indicate a direct, such as command-line,
   *  request from the user or a "rerun" of previously run tests. In the latter
   *  case, the <code>Selectors</code> would be taken from those passed in
   *  events of a previous run or runs.
   */
  def selectors(): Array[Selector] = _selectors

  override def equals(that: Any): Boolean = that match {
    case that: TaskDef =>
      this.fullyQualifiedName  == that.fullyQualifiedName &&
      this.fingerprint         == that.fingerprint &&
      this.explicitlySpecified == that.explicitlySpecified &&
      Arrays.equals(
          this.selectors.asInstanceOf[Array[AnyRef]],
          that.selectors.asInstanceOf[Array[AnyRef]])
    case _ => false
  }

  override def hashCode(): Int = {
    var retVal = 17
    retVal = 31 * retVal + _fullyQualifiedName.hashCode()
    retVal = 31 * retVal + _fingerprint.hashCode()
    retVal = 31 * retVal + (if (_explicitlySpecified) 1 else 0)
    retVal = 31 * retVal + Arrays.hashCode(
        _selectors.asInstanceOf[Array[AnyRef]])
    retVal
  }

  override def toString(): String = {
    "TaskDef(" + _fullyQualifiedName + ", " + _fingerprint + ", " +
    _explicitlySpecified + ", " + _selectors.mkString("[", ", ", "]") + ")"
  }
}
