package scala.scalajs

import scala.annotation.tailrec

import scala.collection.GenTraversableOnce

package object runtime {

  def wrapJavaScriptException(e: Any): Throwable = e match {
    case e: Throwable => e
    case _            => js.JavaScriptException(e)
  }

  def unwrapJavaScriptException(th: Throwable): Any = th match {
    case js.JavaScriptException(e) => e
    case _                         => th
  }

  def toJSVarArgs[A](seq: Seq[A]): js.Array[A] = {
    seq match {
      case seq: js.WrappedArray[A] =>
        seq.array
      case _ =>
        val result = new js.Array[A]
        seq.foreach(x => result.push(x))
        result
    }
  }

  /** Dummy method used to preserve the type parameter of
   *  `js.constructorOf[T]` through erasure.
   *
   *  An early phase of the compiler reroutes calls to `js.constructorOf[T]`
   *  into `runtime.constructorOf(classOf[T])`.
   *
   *  The `clazz` parameter must be a literal `classOf[T]` constant such that
   *  `T` represents a class extending `js.Any` (not a trait nor an object).
   */
  def constructorOf(clazz: Class[_ <: js.Any]): js.Dynamic =
    throw new Error("stub")

  /** Public access to `new ConstructorTag` for the codegen of
   *  `js.ConstructorTag.materialize`.
   */
  def newConstructorTag[T <: js.Any](constructor: js.Dynamic): js.ConstructorTag[T] =
    new js.ConstructorTag[T](constructor)

  /** Dummy method used to preserve where and how an inner JS class should be
   *  created.
   *
   *  @param clazz `classOf` of the class to be created
   *  @param superClass JS class value of the super class
   */
  def createInnerJSClass(clazz: Class[_], superClass: AnyRef): AnyRef =
    throw new Error("stub")

  /** Dummy method used to preserve where and how a local JS class should be
   *  created.
   *
   *  @param clazz
   *    `classOf` of the class to be created
   *  @param superClass
   *    JavaScript class value of the super class
   *  @param fakeNewInstances
   *    Fake `New` instantiations used to retrieve actual capture params
   */
  def createLocalJSClass(clazz: Class[_], superClass: AnyRef,
      fakeNewInstances: Array[AnyRef]): AnyRef = {
    throw new Error("stub")
  }

  /** Dummy method used to preserve a JS class value term associated with an
   *  expression tree.
   *
   *  This is used for:
   *  - New instances of nested JS classes and objects
   *  - Super calls in nested JS classes
   *
   *  @param jsclass
   *    The contextual JS class value
   *  @param inner
   *    The original inner tree
   */
  def withContextualJSClassValue[A](jsclass: AnyRef, inner: A): A =
    throw new Error("stub")

  /** Returns an array of the enumerable properties in an object's prototype
   *  chain.
   *
   *  This is the implementation of [[js.Object.properties]].
   */
  def propertiesOf(obj: js.Any): js.Array[String] = {
    // See http://stackoverflow.com/questions/26445248/
    if (obj == null || js.isUndefined(obj)) {
      js.Array()
    } else {
      val result = new js.Array[String]
      val alreadySeen = js.Dictionary.empty[Boolean]

      @tailrec
      def loop(obj: js.Object): Unit = {
        if (obj != null) {
          // Add own enumerable properties that have not been seen yet
          val enumProps = js.Object.keys(obj)
          val enumPropsLen = enumProps.length
          var i = 0
          while (i < enumPropsLen) {
            val prop = enumProps(i)
            if (!alreadySeen.get(prop).isDefined)
              result.push(prop)
            i += 1
          }

          /* Add all own properties to the alreadySeen set, including
           * non-enumerable ones.
           */
          val allProps = js.Object.getOwnPropertyNames(obj)
          val allPropsLen = allProps.length
          var j = 0
          while (j < allPropsLen) {
            alreadySeen(allProps(j)) = true
            j += 1
          }

          // Continue with the next object in the prototype chain
          loop(js.Object.getPrototypeOf(obj))
        }
      }
      loop(js.Object(obj))

      result
    }
  }

  /** Information known at link-time, given the output configuration.
   *
   *  See [[LinkingInfo]] for details.
   */
  def linkingInfo: LinkingInfo = throw new Error("stub")

}
