package sbt.testing

/** Information in addition to a test class name that identifies the suite or
 *  test about which an event was fired.
 *
 *  This class has five subtypes:
 *
 *  - <code>SuiteSelector</code> - indicates an event is about an entire suite
 *    of tests whose class was reported as <code>fullyQualifiedName</code> in
 *    the <code>Event</code>
 *  - <code>TestSelector</code> - indicates an event is about a single test
 *    directly contained in the suite whose class was reported as
 *    <code>fullyQualifiedName</code> in the <code>Event</code>
 *  - <code>NestedSuiteSelector</code> - indicates an event is about an entire
 *    nested suite of tests whose top-level, "nesting" class was reported as
 *    <code>fullyQualifiedName</code> in the <code>Event</code>
 *  - <code>NestedTestSelector</code> - indicates an event is about a single
 *    test contained in a nested suite whose top-level, "nesting" class was
 *    reported as <code>fullyQualifiedName</code> in the <code>Event</code>
 *  - <code>TestWildcardSelector</code> - indicates an event is about zero to
 *    many tests directly contained in the suite whose class was reported as
 *    <code>fullyQualifiedName</code> in the <code>Event</code>
 */
abstract sealed class Selector

/** Indicates an event was about the entire suite whose class had the fully
 *  qualified name specified as the <code>fullyQualifiedName</code> attribute
 *  the event.
 */
final class SuiteSelector extends Selector with Serializable {
  override def equals(o: Any): Boolean = o.isInstanceOf[SuiteSelector]
  override def hashCode(): Int = 29
  override def toString(): String = "SuiteSelector"
}

/** Information in addition to a test class name that identifies a test
 *  directly contained in the suite whose class had the fully qualified name
 *  specified as the <code>fullyQualifiedName</code> attribute passed to the
 *  event.
 */
final class TestSelector(_testName: String) extends Selector with Serializable {

  if (_testName == null)
    throw new NullPointerException("testName was null");

  /** The name of a test about which an event was fired.
   *
   *  @return the name of the test
   */
  def testName(): String = _testName

  override def equals(that: Any): Boolean = that match {
    case that: TestSelector => this.testName == that.testName
    case _ => false
  }

  override def hashCode(): Int = testName.hashCode()
  override def toString(): String = s"TestSelector($testName)"
}

/** Information in addition to a test class name that identifies a nested suite
 *  about which an event was fired.
 */
final class NestedSuiteSelector(_suiteId: String) extends Selector with Serializable {

  if (_suiteId == null)
    throw new NullPointerException("suiteId was null");

  /** An id that, in addition to a test class name, identifies a nested suite
   *  about which an event was fired.
   *
   *  @return the id of the nested suite
   */
  def suiteId(): String = _suiteId

  override def equals(that: Any): Boolean = that match {
    case that: NestedSuiteSelector => this.suiteId == that.suiteId
    case _ => false
  }

  override def hashCode(): Int = suiteId.hashCode()
  override def toString(): String = s"NestedSuiteSelector($suiteId)"
}

/** Information in addition to a test class name that identifies a test in a
 *  nested suite about which an event was fired.
 */
final class NestedTestSelector(_suiteId: String,
    _testName: String) extends Selector with Serializable {

  if (_suiteId == null)
    throw new NullPointerException("suiteId was null");
  if (_testName == null)
    throw new NullPointerException("testName was null");

  /** An id that, in addition to a test class name, identifies a nested suite
   *  that contains a test about which an event was fired.
   *
   *  @return the id of the nested suite containing the test
   */
  def suiteId(): String = _suiteId

  /** The name of the test in a nested suite about which an event was fired.
   *
   *  @return the name of the test in the nested suite identified by the id
   *          returned by <code>suiteId</code>.
   */
  def testName(): String = _testName

  override def equals(that: Any): Boolean = that match {
    case that: NestedTestSelector =>
      this.suiteId == that.suiteId && this.testName == that.testName
    case _ => false
  }

  override def hashCode(): Int = {
    var retVal = 17
    retVal = 31 * retVal + suiteId.hashCode()
    retVal = 31 * retVal + testName.hashCode()
    retVal
  }

  override def toString(): String = s"NestedTestSelector($suiteId, $testName)"
}

/** Information that identifies zero to many tests directly contained in a test
 *  class.
 *
 *  The <code>testWildcard</code> is a simple string, <em>i.e.</em>, not a glob
 *  or regular expression. Any test whose name includes the
 *  <code>testWildcard</code> string as a substring will be selected.
 */
final class TestWildcardSelector(
    _testWildcard: String) extends Selector with Serializable {

  if (_testWildcard == null)
    throw new NullPointerException("testWildcard was null");

  /** A test wildcard string used to select tests.
   *
   *  The <code>testWildcard</code> is a simple string, <em>i.e.</em>, not a
   *  glob or regular expression. Any test whose name includes the
   *  <code>testWildcard</code> string as a substring will be selected.
   *
   *  @return the test wildcard string used to select tests.
   */
  def testWildcard(): String = _testWildcard

  override def equals(that: Any): Boolean = that match {
    case that: TestWildcardSelector =>
      this.testWildcard == that.testWildcard
    case _ => false
  }

  override def hashCode(): Int = testWildcard.hashCode()

  override def toString(): String = s"TestWildcardSelector($testWildcard)"
}
