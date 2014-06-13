/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.scalajs.js.annotation

/** Marks the annotated class, trait or object as a raw JavaScript type.
 *
 *  This annotation is added automatically by the compiler to all classes,
 *  traits and objects inheriting directly or indirectly from
 *  [[scala.scalajs.js.Any]]. It marks the annotated entity as being a raw
 *  JavaScript type, i.e., one that represents type information for an entity
 *  defined in JavaScript code.
 *
 *  Do not use this annotation yourself.
 */
class RawJSType extends scala.annotation.StaticAnnotation
