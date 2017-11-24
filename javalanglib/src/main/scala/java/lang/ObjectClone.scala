package java.lang

import scala.scalajs.js

/** Implementation of `java.lang.Object.clone()`.
 *
 *  Called by the hard-coded IR of `java.lang.Object`.
 */
private[lang] object ObjectClone {
  /** Returns a new shallow clone of `o`.
   *
   *  This method does not test that `o` is an instance of `Cloneable`. The
   *  caller should do that themselves, although this `cloneObject` does not
   *  rely on that property for correctness.
   */
  def clone(o: Object): Object = {
    val fromDyn = o.asInstanceOf[js.Dynamic]
    val result = js.Dynamic.newInstance(fromDyn.constructor)()
    val fromDict = o.asInstanceOf[js.Dictionary[js.Any]]
    val resultDict = result.asInstanceOf[js.Dictionary[js.Any]]
    for (key <- fromDict.keys)
      resultDict(key) = fromDict(key)
    result
  }
}
