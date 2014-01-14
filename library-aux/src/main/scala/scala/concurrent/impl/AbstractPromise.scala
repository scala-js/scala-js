package scala.concurrent.impl

/**
 * JavaScript specific implementation of AbstractPromise
 *
 * This basically implements a "CAS" in Scala for JavaScript. Its
 * implementation is trivial because there is no multi-threading.
 *
 * @author Tobias Schlatter
 */
abstract class AbstractPromise {

  private var state: AnyRef = _

  protected final
  def updateState(oldState: AnyRef, newState: AnyRef): Boolean = {
    if (state eq oldState) {
      state = newState
      true
    } else false
  }

  protected final def getState: AnyRef = state

}

object AbstractPromise {
  protected def updater = ???
}
