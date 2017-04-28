package scala.concurrent.impl

import java.util.concurrent.atomic.AtomicReference

@Deprecated // Since 2.11.8. Extend java.util.concurrent.atomic.AtomicReference instead.
abstract class AbstractPromise extends AtomicReference[AnyRef] {
  protected final def updateState(oldState: AnyRef, newState: AnyRef): Boolean =
    compareAndSet(oldState, newState)

  protected final def getState(): AnyRef = get()
}
