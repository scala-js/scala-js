/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.linker.backend.emitter

import scala.annotation.{switch, tailrec}

import java.util.Comparator

import scala.collection.mutable
import scala.reflect.ClassTag

import org.scalajs.ir._
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Position._
import org.scalajs.ir.Printers.IRTreePrinter
import org.scalajs.ir.Transformers._
import org.scalajs.ir.Traversers._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.linker.interface._
import org.scalajs.linker.interface.CheckedBehavior._
import org.scalajs.linker.backend.javascript.{Trees => js}
import org.scalajs.linker.standard.{LinkedClass, ModuleSet}

import org.scalajs.logging.Logger

import EmitterNames._
import PolyfillableBuiltin._
import Transients._

private[emitter] final class NameCompressor private (
  entries: NameCompressor.EntryMap,
  ancestorEntries: NameCompressor.AncestorEntryMap
) {
  import NameCompressor._

  def allocatedFor(fieldName: FieldName): String =
    entries(fieldName).allocatedName

  def allocatedFor(methodName: MethodName): String =
    entries(methodName).allocatedName

  def allocatedFor(prop: ArrayClassProperty): String =
    entries(prop).allocatedName

  def allocatedForAncestor(ancestor: ClassName): String =
    ancestorEntries(ancestor).allocatedName
}

private[emitter] object NameCompressor {
  /** Base set of names that should be avoided when allocating property names
   *  in any namespace.
   *
   *  This set contains:
   *
   *  - the reserved JS identifiers (not technically invalid by spec, but JS
   *    minifiers tend to avoid them anyway: `foo.if` is playing with fire),
   *  - the `"then"` name, because it is used to identify `Thenable`s by
   *    spec and therefore lives in the same namespace as the properties of
   *    *all* objects,
   */
  private val BasePropertyNamesToAvoid: Set[String] =
    NameGen.ReservedJSIdentifierNames + "then"

  def compress(config: Emitter.Config, moduleSet: ModuleSet,
      logger: Logger): (NameCompressor, Set[String]) = {
    val traverser = new Traverser(config, moduleSet)

    logger.time("Name compressor: Collect names") {
      traverser.traverseModuleSet()
    }

    val entries = traverser.entries
    val ancestorEntries = traverser.ancestorEntries
    val dangerousGlobalRefs = traverser.dangerousGlobalRefs.toSet

    logger.time("Name compressor: Allocate property names") {
      allocatePropertyNames(entries, traverser.propertyNamesToAvoid)
    }

    logger.time("Name compressor: Allocate ancestor names") {
      allocatePropertyNames(ancestorEntries, BasePropertyNamesToAvoid)
    }

    val compressor = new NameCompressor(entries, ancestorEntries)
    (compressor, dangerousGlobalRefs)
  }

  private def allocatePropertyNames[K <: AnyRef, E <: BaseEntry with Comparable[E]](
      entries: mutable.AnyRefMap[K, E], namesToAvoid: collection.Set[String])(
      implicit ct: ClassTag[E]): Unit = {
    val comparator: Comparator[E] =
      Comparator.comparingInt[E](_.occurrences).reversed() // by decreasing order of occurrences
        .thenComparing(Comparator.naturalOrder[E]()) // tie-break

    val orderedEntries = entries.values.toArray
    java.util.Arrays.sort(orderedEntries, comparator)

    val generator = new NameGenerator(namesToAvoid)

    for (entry <- orderedEntries)
      entry.allocatedName = generator.nextString()
  }

  /** Keys of this map are `FieldName | MethodName | ArrayClassProperty`. */
  private type EntryMap = mutable.AnyRefMap[AnyRef, PropertyNameEntry]

  private type AncestorEntryMap = mutable.AnyRefMap[ClassName, AncestorNameEntry]

  private sealed abstract class BaseEntry {
    var occurrences: Int = 0
    var allocatedName: String = null

    def incOccurrences(): Unit =
      occurrences += 1
  }

  private sealed abstract class PropertyNameEntry
      extends BaseEntry with Comparable[PropertyNameEntry] {

    def compareTo(that: PropertyNameEntry): Int = (this, that) match {
      case (x: FieldNameEntry, y: FieldNameEntry) =>
        x.fieldName.compareTo(y.fieldName)

      case (x: MethodNameEntry, y: MethodNameEntry) =>
        x.methodName.compareTo(y.methodName)

      case (x: ArrayClassPropEntry, y: ArrayClassPropEntry) =>
        x.property.compareTo(y.property)

      case _ =>
        def ordinalFor(x: PropertyNameEntry): Int = x match {
          case _: FieldNameEntry      => 1
          case _: MethodNameEntry     => 2
          case _: ArrayClassPropEntry => 3
        }
        ordinalFor(this) - ordinalFor(that)
    }
  }

  private final class FieldNameEntry(val fieldName: FieldName)
      extends PropertyNameEntry {
    override def toString(): String = s"FieldNameEntry(${fieldName.nameString})"
  }

  private final class MethodNameEntry(val methodName: MethodName)
      extends PropertyNameEntry {
    override def toString(): String = s"MethodNameEntry(${methodName.nameString})"
  }

  private final class ArrayClassPropEntry(val property: ArrayClassProperty)
      extends PropertyNameEntry {
    override def toString(): String = s"ArrayClassPropEntry(${property.nonMinifiedName})"
  }

  private final class AncestorNameEntry(val ancestor: ClassName)
      extends BaseEntry with Comparable[AncestorNameEntry] {

    def compareTo(that: AncestorNameEntry): Int =
      this.ancestor.compareTo(that.ancestor)

    override def toString(): String = s"AncestorNameEntry(${ancestor.nameString})"
  }

  private final class Traverser(config: Emitter.Config, moduleSet: ModuleSet)
      extends org.scalajs.ir.Traversers.Traverser {

    import config.semantics._
    import config.esFeatures.esVersion

    private val useBigIntForLongs = config.esFeatures.allowBigIntsForLongs

    private val interfaceClassNames: Set[ClassName] = {
      (for {
        module <- moduleSet.modules
        linkedClass <- module.classDefs
        if linkedClass.kind == ClassKind.Interface
      } yield {
        linkedClass.name.name
      }).toSet
    }

    val dangerousGlobalRefs = mutable.Set.empty[String]

    /** Names that should be avoided when allocating property names for
     *  instance properties of Scala classes.
     *
     *  After traversing the module set, this set contains:
     *
     *  - the properties names that should be avoided in any namespace
     *    (see `BasePropertyNamesToAvoid`),
     *  - the `"name"` and `"cause"` names, because they are specified instance
     *    properties of `Error` (which our `Throwable` extends) and are
     *    short enough that they could in theory collide with generated names,
     *  - the names of *exported* methods and properties in Scala classes,
     *    because they live in the same namespace as the properties we will
     *    rename, so we must avoid collisions with them.
     */
    val propertyNamesToAvoid: mutable.Set[String] =
      mutable.Set.empty ++= BasePropertyNamesToAvoid ++= List("name", "cause")

    /** Keys are `FieldName`s, `MethodName`s or `ArrayClassProperty`s. */
    val entries: EntryMap = mutable.AnyRefMap.empty

    val ancestorEntries: AncestorEntryMap = mutable.AnyRefMap.empty

    private def countPropertyName(fieldName: FieldName): Unit =
      entries.getOrElseUpdate(fieldName, new FieldNameEntry(fieldName)).incOccurrences()

    private def countPropertyName(methodName: MethodName): Unit =
      entries.getOrElseUpdate(methodName, new MethodNameEntry(methodName)).incOccurrences()

    private def countPropertyName(prop: ArrayClassProperty): Unit =
      entries.getOrElseUpdate(prop, new ArrayClassPropEntry(prop)).incOccurrences()

    private def countAncestorName(ancestor: ClassName): Unit = {
      if (ancestor != ObjectClass)
        ancestorEntries.getOrElseUpdate(ancestor, new AncestorNameEntry(ancestor)).incOccurrences()
    }

    private def countGlobalRef(name: String): Unit = {
      if (GlobalRefUtils.isDangerousGlobalRef(name))
        dangerousGlobalRefs += name
    }

    def traverseModuleSet(): Unit = {
      countCoreJSLibPropertyNames()

      for (module <- moduleSet.modules) {
        for (linkedClass <- module.classDefs)
          traverseLinkedClass(linkedClass)
        for (topLevelExport <- module.topLevelExports)
          traverseTopLevelExportDef(topLevelExport.tree)
      }
    }

    private def countCoreJSLibPropertyNames(): Unit = {
      countPropertyName(cloneMethodName)

      if (nullPointers == CheckedBehavior.Unchecked) {
        // See CoreJSLib.defineObjectGetClassFunctions()
        countPropertyName(getClassMethodName)
        countPropertyName(getNameMethodName)
      }

      // ArrayClass properties

      countPropertyName(ArrayClassProperty.u)

      if (arrayIndexOutOfBounds != CheckedBehavior.Unchecked) {
        countPropertyName(ArrayClassProperty.get)
        countPropertyName(ArrayClassProperty.set)
      } else if (arrayStores != CheckedBehavior.Unchecked) {
        countPropertyName(ArrayClassProperty.set)
      }

      if (esVersion >= ESVersion.ES2015)
        countPropertyName(ArrayClassProperty.copyTo)

      // Ancestor names

      // One in `initArray`, and one in `initSpecializedArray`
      countAncestorName(CloneableClass)
      countAncestorName(CloneableClass)
      countAncestorName(SerializableClass)
      countAncestorName(SerializableClass)
    }

    def traverseLinkedClass(linkedClass: LinkedClass): Unit = {
      if (linkedClass.kind.isClass || linkedClass.kind == ClassKind.HijackedClass) {
        /* For Scala classes, we need to register the definition names of
         * fields and methods for allocation. We also need to register the
         * definition names of member exports as names to avoid, in order to
         * prevent collisions.
         *
         * Note: Methods of hijacked classes are referred to in a normal
         * `genApply` from the dispatchers generated by
         * `CoreJSLib.defineDispatchFunctions()`.
         */
        for (field <- linkedClass.fields)
          traverseAnyFieldDefInScalaClass(field)
        for (method <- linkedClass.methods)
          traverseMethodDefInScalaClass(method)
        for (exportedMember <- linkedClass.exportedMembers)
          traverseJSMethodPropDefInScalaClass(exportedMember)
      } else {
        /* For all other types, including Scala interfaces, we only need to
         * recurse inside the bodies. The definition names are not emitted.
         */
        for (field <- linkedClass.fields)
          traverseAnyFieldDef(field)
        for (method <- linkedClass.methods)
          traverseMethodDef(method)
        for (exportedMember <- linkedClass.exportedMembers)
          traverseJSMethodPropDef(exportedMember)
      }

      val className = linkedClass.className

      if (linkedClass.hasInstanceTests) {
        countAncestorName(className)
        if (linkedClass.kind == ClassKind.Interface)
          countAncestorName(className)
      }

      if (linkedClass.hasRuntimeTypeInfo) {
        countAncestorName(className)
        for (ancestor <- linkedClass.ancestors)
          countAncestorName(ancestor)
      }
    }

    def traverseAnyFieldDefInScalaClass(fieldDef: AnyFieldDef): Unit = {
      traverseAnyFieldDef(fieldDef)

      fieldDef match {
        case fieldDef: FieldDef =>
          if (!fieldDef.flags.namespace.isStatic)
            countPropertyName(fieldDef.name.name)
        case _: JSFieldDef =>
          ()
      }
    }

    def traverseMethodDefInScalaClass(methodDef: MethodDef): Unit = {
      traverseMethodDef(methodDef)

      if (methodDef.flags.namespace == MemberNamespace.Public)
        countPropertyName(methodDef.name.name)
    }

    def traverseJSMethodPropDefInScalaClass(jsMethodPropDef: JSMethodPropDef): Unit = {
      traverseJSMethodPropDef(jsMethodPropDef)

      jsMethodPropDef match {
        case JSMethodDef(_, StringLiteral(name), _, _, _) =>
          propertyNamesToAvoid += name
        case JSPropertyDef(_, StringLiteral(name), _, _) =>
          propertyNamesToAvoid += name
        case _ =>
          ()
      }
    }

    override def traverse(tree: Tree): Unit = {
      // scalastyle:off return

      tree match {
        case Assign(ArraySelect(array, index), rhs) =>
          if (FunctionEmitter.isArraySetChecked(config.semantics, array.tpe))
            countPropertyName(ArrayClassProperty.set)
          else
            countPropertyName(ArrayClassProperty.u)
          traverse(array)
          traverse(index)
          traverse(rhs)
          return // prevent `super.traverse(tree)`

        case Select(_, FieldIdent(fieldName)) =>
          countPropertyName(fieldName)

        case Apply(_, receiver, MethodIdent(methodName), _) =>
          // Ideally we should also ignore hijacked method calls
          if (receiver.tpe != AnyType || methodName.isReflectiveProxy)
            countPropertyName(methodName)

        case ApplyStatically(flags, _, className, MethodIdent(methodName), _) =>
          if (!flags.isConstructor && !flags.isPrivate && !interfaceClassNames.contains(className))
            countPropertyName(methodName)

        case UnaryOp(op, _) if !useBigIntForLongs =>
          import UnaryOp._
          (op: @switch) match {
            case IntToLong    => countPropertyName(LongImpl.fromInt)
            case LongToInt    => countPropertyName(LongImpl.toInt)
            case LongToDouble => countPropertyName(LongImpl.toDouble)
            case DoubleToLong => countPropertyName(LongImpl.fromDouble)
            case LongToFloat  => countPropertyName(LongImpl.toFloat)
            case _            => ()
          }

        case BinaryOp(op, lhs, _) if !useBigIntForLongs =>
          import BinaryOp._
          (op: @switch) match {
            case Long_+ => countPropertyName(LongImpl.+)
            case Long_- =>
              lhs match {
                case LongLiteral(0L) => countPropertyName(LongImpl.UNARY_-)
                case _               => countPropertyName(LongImpl.-)
              }
            case Long_* => countPropertyName(LongImpl.*)
            case Long_/ => countPropertyName(LongImpl./)
            case Long_% => countPropertyName(LongImpl.%)
            case Long_| => countPropertyName(LongImpl.|)
            case Long_& => countPropertyName(LongImpl.&)
            case Long_^ =>
              lhs match {
                case LongLiteral(-1L) => countPropertyName(LongImpl.UNARY_~)
                case _                => countPropertyName(LongImpl.^)
              }
            case Long_<<  => countPropertyName(LongImpl.<<)
            case Long_>>> => countPropertyName(LongImpl.>>>)
            case Long_>>  => countPropertyName(LongImpl.>>)
            case Long_==  => countPropertyName(LongImpl.===)
            case Long_!=  => countPropertyName(LongImpl.!==)
            case Long_<   => countPropertyName(LongImpl.<)
            case Long_<=  => countPropertyName(LongImpl.<=)
            case Long_>   => countPropertyName(LongImpl.>)
            case Long_>=  => countPropertyName(LongImpl.>=)
            case _        => ()
          }

        case ArrayLength(_) =>
          countPropertyName(ArrayClassProperty.u)

        case ArraySelect(_, _) =>
          if (arrayIndexOutOfBounds != CheckedBehavior.Unchecked)
            countPropertyName(ArrayClassProperty.get)

        case Clone(expr) =>
          if (expr.tpe.isInstanceOf[ArrayType])
            countPropertyName(cloneMethodName)

        case UnwrapFromThrowable(expr) =>
          countPropertyName(exceptionFieldName)

        case JSGlobalRef(name) =>
          countGlobalRef(name)

        case Transient(SystemArrayCopy(src, srcPos, dest, destPos, length)) =>
          if (esVersion >= ESVersion.ES2015 && nullPointers == CheckedBehavior.Unchecked) {
            val useCopyTo = src.tpe match {
              case _ if arrayStores == CheckedBehavior.Unchecked => true
              case ArrayType(ArrayTypeRef(_: PrimRef, 1))        => src.tpe == dest.tpe
              case _                                             => false
            }
            if (useCopyTo)
              countPropertyName(ArrayClassProperty.copyTo)
          }

        case Transient(ZeroOf(_)) =>
          countPropertyName(dataFieldName)

        case Transient(NativeArrayWrapper(elemClass, _)) =>
          if (!elemClass.isInstanceOf[ClassOf])
            countPropertyName(dataFieldName)

        case Transient(ArrayToTypedArray(_, _)) =>
          countPropertyName(ArrayClassProperty.u)

        case _ =>
          ()
      }

      super.traverse(tree)

      // scalastyle:on return
    }
  }

  // private[emitter] for tests
  private[emitter] final class NameGenerator(namesToAvoid: collection.Set[String]) {
    /* 6 because 52 * (62**5) > Int.MaxValue
     * i.e., to exceed this size we would need more than Int.MaxValue different names.
     */
    private val charArray = new Array[Char](6)
    charArray(0) = 'a'
    private var charCount = 1

    @tailrec
    private def incAtIndex(idx: Int): Unit = {
      (charArray(idx): @switch) match {
        case '9' =>
          charArray(idx) = 'a'
        case 'z' =>
          charArray(idx) = 'A'
        case 'Z' =>
          if (idx > 0) {
            charArray(idx) = '0'
            incAtIndex(idx - 1)
          } else {
            java.util.Arrays.fill(charArray, '0')
            charArray(0) = 'a'
            charCount += 1
          }
        case c =>
          charArray(idx) = (c + 1).toChar
      }
    }

    @tailrec
    final def nextString(): String = {
      val s = new String(charArray, 0, charCount)
      incAtIndex(charCount - 1)
      if (namesToAvoid.contains(s))
        nextString()
      else
        s
    }
  }
}
