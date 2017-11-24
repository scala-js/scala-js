package java.lang

import scala.scalajs.js

/** Implementation of `java.lang.Object.clone()`.
 *
 *  Called by the hard-coded IR of `java.lang.Object`.
 */
private[lang] object ObjectClone {
  private val getOwnPropertyDescriptors: js.Function1[js.Object, js.Object] = {
    import js.Dynamic.{global, literal}

    // The val f = ...; f.asInstanceOf's are necessary for 2.10 not to crash

    // Fetch or polyfill Object.getOwnPropertyDescriptors
    if (js.typeOf(global.Object.getOwnPropertyDescriptors) == "function") {
      val f = global.Object.getOwnPropertyDescriptors
      f.asInstanceOf[js.Function1[js.Object, js.Object]]
    } else {
      // Fetch or polyfill Reflect.ownKeys
      type OwnKeysType = js.Function1[js.Object, js.Array[js.Any]]
      val ownKeysFun: OwnKeysType = {
        if (js.typeOf(global.Reflect) != "undefined" &&
            js.typeOf(global.Reflect.ownKeys) == "function") {
          val f = global.Reflect.ownKeys
          f.asInstanceOf[OwnKeysType]
        } else {
          // Fetch Object.getOwnPropertyNames
          val getOwnPropertyNames = {
            val f = global.Object.getOwnPropertyNames
            f.asInstanceOf[OwnKeysType]
          }

          // Fetch or polyfill Object.getOwnPropertySymbols
          val getOwnPropertySymbols: OwnKeysType = {
            if (js.typeOf(global.Object.getOwnPropertySymbols) == "function") {
              val f = global.Object.getOwnPropertySymbols
              f.asInstanceOf[OwnKeysType]
            } else {
              /* Polyfill for Object.getOwnPropertySymbols.
               * We assume that if that function does not exist, then symbols
               * do not exist at all. Therefore, the result is always an empty
               * array.
               */
              { (o: js.Object) =>
                js.Array[js.Any]()
              }
            }
          }

          // Polyfill for Reflect.ownKeys
          { (o: js.Object) =>
            getOwnPropertyNames(o) ++ getOwnPropertySymbols(o)
          }
        }
      }

      // Polyfill for Object.getOwnPropertyDescriptors
      { (o: js.Object) =>
        val ownKeys = ownKeysFun(o)
        val descriptors = new js.Object
        for (key <- ownKeys) {
          /* Almost equivalent to the JavaScript code
           *   descriptors[key] = Object.getOwnPropertyDescriptor(descriptors, key);
           * except that `defineProperty` will by-pass any existing setter for
           * the property `key` on `descriptors` or in its prototype chain.
           */
          global.Object.defineProperty(descriptors, key, literal(
              configurable = true,
              enumerable = true,
              writable = true,
              value = global.Object.getOwnPropertyDescriptor(o, key)
          ))
        }
        descriptors
      }
    }
  }

  /** Returns a new shallow clone of `o`.
   *
   *  This method does not test that `o` is an instance of `Cloneable`. The
   *  caller should do that themselves, although this `cloneObject` does not
   *  rely on that property for correctness.
   */
  def clone(o: Object): Object = {
    js.Object.create(js.Object.getPrototypeOf(o.asInstanceOf[js.Object]),
        getOwnPropertyDescriptors(o.asInstanceOf[js.Object]))
  }
}
