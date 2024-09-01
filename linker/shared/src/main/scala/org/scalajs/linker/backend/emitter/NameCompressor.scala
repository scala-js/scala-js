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

import org.scalajs.ir.Names._
import org.scalajs.linker.backend.javascript.Trees.DelayedIdent.Resolver
import org.scalajs.linker.standard.ModuleSet
import org.scalajs.logging.Logger

private[emitter] final class NameCompressor(config: Emitter.Config) {
  import NameCompressor._

  private val entries: EntryMap = mutable.AnyRefMap.empty
  private val ancestorEntries: AncestorEntryMap = mutable.AnyRefMap.empty

  private var namesAllocated: Boolean = false

  def allocateNames(moduleSet: ModuleSet, logger: Logger): Unit = {
    assert(!namesAllocated, "Cannot allocate names a second time")

    val propertyNamesToAvoid = logger.time("Name compressor: Collect property names to avoid") {
      collectPropertyNamesToAvoid(moduleSet)
    }

    logger.time("Name compressor: Allocate property names") {
      allocatePropertyNames(entries, propertyNamesToAvoid)
    }

    logger.time("Name compressor: Allocate ancestor names") {
      allocatePropertyNames(ancestorEntries, BasePropertyNamesToAvoid)
    }

    namesAllocated = true
  }

  def genResolverFor(fieldName: FieldName): Resolver =
    entries.getOrElseUpdate(fieldName, new FieldNameEntry(fieldName)).genResolver()

  def genResolverFor(methodName: MethodName): Resolver =
    entries.getOrElseUpdate(methodName, new MethodNameEntry(methodName)).genResolver()

  def genResolverFor(prop: SyntheticProperty): Resolver =
    entries.getOrElseUpdate(prop, new SyntheticPropEntry(prop)).genResolver()

  def genResolverForAncestor(ancestor: ClassName): Resolver =
    ancestorEntries.getOrElseUpdate(ancestor, new AncestorNameEntry(ancestor)).genResolver()

  /** Collects the property names to avoid for Scala instance members.
   *
   *  We collect the names of exported members in Scala classes. These live in
   *  the same namespace as Scala methods and fields. Therefore, we must avoid
   *  them when allocating names for that namespace.
   */
  private def collectPropertyNamesToAvoid(moduleSet: ModuleSet): Set[String] = {
    import org.scalajs.ir.Trees._

    val builder = Set.newBuilder[String]

    builder ++= BasePropertyNamesToAvoid

    for {
      module <- moduleSet.modules
      linkedClass <- module.classDefs
      if linkedClass.kind.isClass
      exportedMember <- linkedClass.exportedMembers
    } {
      (exportedMember: @unchecked) match {
        case JSMethodDef(_, StringLiteral(name), _, _, _) =>
          builder += name
        case JSPropertyDef(_, StringLiteral(name), _, _) =>
          builder += name
      }
    }

    builder.result()
  }
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

  private def allocatePropertyNames[K <: AnyRef, E <: BaseEntry with Comparable[E]: ClassTag](
      entries: mutable.AnyRefMap[K, E], namesToAvoid: collection.Set[String]): Unit = {
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

    protected def debugString: String

    private object resolver extends Resolver {
      def resolve(): String = {
        if (allocatedName == null)
          throw new IllegalStateException(s"Cannot resolve name before it was allocated, for $this")
        allocatedName
      }

      def debugString: String = BaseEntry.this.debugString

      override def toString(): String = debugString
    }

    private def incOccurrences(): Unit = {
      if (allocatedName != null)
        throw new IllegalStateException(s"Cannot increase occurrences after name was allocated for $this")
      occurrences += 1
    }

    def genResolver(): Resolver = {
      incOccurrences()
      resolver
    }
  }

  private sealed abstract class PropertyNameEntry
      extends BaseEntry with Comparable[PropertyNameEntry] {

    def compareTo(that: PropertyNameEntry): Int = (this, that) match {
      case (x: FieldNameEntry, y: FieldNameEntry) =>
        x.fieldName.compareTo(y.fieldName)

      case (x: MethodNameEntry, y: MethodNameEntry) =>
        x.methodName.compareTo(y.methodName)

      case (x: SyntheticPropEntry, y: SyntheticPropEntry) =>
        x.property.compareTo(y.property)

      case _ =>
        def ordinalFor(x: PropertyNameEntry): Int = x match {
          case _: FieldNameEntry     => 1
          case _: MethodNameEntry    => 2
          case _: SyntheticPropEntry => 3
        }
        ordinalFor(this) - ordinalFor(that)
    }
  }

  private final class FieldNameEntry(val fieldName: FieldName)
      extends PropertyNameEntry {
    protected def debugString: String = fieldName.nameString

    override def toString(): String = s"FieldNameEntry(${fieldName.nameString})"
  }

  private final class MethodNameEntry(val methodName: MethodName)
      extends PropertyNameEntry {
    protected def debugString: String = methodName.nameString

    override def toString(): String = s"MethodNameEntry(${methodName.nameString})"
  }

  private final class SyntheticPropEntry(val property: SyntheticProperty)
      extends PropertyNameEntry {
    protected def debugString: String = property.nonMinifiedName

    override def toString(): String = s"SyntheticPropEntry(${property.nonMinifiedName})"
  }

  private final class AncestorNameEntry(val ancestor: ClassName)
      extends BaseEntry with Comparable[AncestorNameEntry] {

    def compareTo(that: AncestorNameEntry): Int =
      this.ancestor.compareTo(that.ancestor)

    protected def debugString: String = ancestor.nameString

    override def toString(): String = s"AncestorNameEntry(${ancestor.nameString})"
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
