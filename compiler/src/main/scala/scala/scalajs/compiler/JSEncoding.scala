/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.scalajs.compiler

import scala.tools.nsc._

/** Encoding of symbol names for JavaScript
 *
 *  Some issues that this encoding solves:
 *  * Overloading: encode the full signature in the JS name
 *  * Same scope for fields and methods of a class
 *  * Global access to classes and modules (by their full name)
 *
 *  @author Sébastien Doeraene
 */
trait JSEncoding extends SubComponent { self: GenJSCode =>
  import global._
  import jsAddons._

  /** Outer separator string (between parameter types) */
  final val OuterSep = "__"

  /** Inner separator character (replace dots in full names) */
  final val InnerSep = "_"

  /** Name given to the local Scala.js environment variable */
  final val ScalaJSEnvironmentName = "ScalaJS"

  /** The current Scala.js environment */
  def environment(implicit pos: Position): js.Ident = {
    js.Ident(ScalaJSEnvironmentName, Some(ScalaJSEnvironmentName))
  }

  /** Select a given field of the current Scala.js environment */
  def envField(name: String)(implicit pos: Position): js.Tree = {
    js.DotSelect(environment, js.Ident(name, Some(name)))
  }

  def encodeLabelSym(sym: Symbol, freshName: Symbol => String)(
      implicit pos: Position): js.Ident = {
    require(sym.isLabel, "encodeLabelSym called with non-label symbol: " + sym)
    js.Ident(freshName(sym), Some(sym.originalName.decoded))
  }

  private lazy val allRefClasses: Set[Symbol] = {
    import definitions._
    (Set(ObjectRefClass, VolatileObjectRefClass) ++
        refClass.values ++ volatileRefClass.values)
  }

  def encodeFieldSym(sym: Symbol)(implicit pos: Position): js.Ident = {
    require(sym.owner.isClass && sym.isTerm && !sym.isMethod && !sym.isModule,
        "encodeFieldSym called with non-field symbol: " + sym)

    val name0 = sym.name.toString
    val name =
      if (name0.charAt(name0.length()-1) != ' ') name0
      else name0.substring(0, name0.length()-1)

    /* We have to special-case fields of Ref types (IntRef, ObjectRef, etc.)
     * because they are emitted as private by our .scala source files, but
     * they are considered public at use site since their symbols come from
     * Java-emitted .class files.
     */
    val idSuffix =
      if (sym.isPrivate || allRefClasses.contains(sym.owner))
        sym.owner.ancestors.count(!_.isInterface).toString
      else
        "f"

    val encodedName = name + "$" + idSuffix
    js.Ident(mangleJSName(encodedName), Some(sym.originalName.decoded))
  }

  def encodeMethodSym(sym: Symbol)(implicit pos: Position): js.Ident = {
    require(sym.isMethod, "encodeMethodSym called with non-method symbol: " + sym)
    val encodedName =
      if (sym.isClassConstructor)
        "init" + InnerSep
      else if (foreignIsImplClass(sym.owner))
        encodeClassFullName(sym.owner) + OuterSep + sym.name.toString
      else if (sym.isPrivate)
        mangleJSName(sym.name.toString) + OuterSep + "p" +
          sym.owner.ancestors.count(!_.isInterface).toString
      else
        mangleJSName(sym.name.toString)
    val paramsString = makeParamsString(sym)
    js.Ident(encodedName + paramsString,
        Some(sym.originalName.decoded + paramsString))
  }

  def encodeStaticMemberSym(sym: Symbol)(implicit pos: Position): js.Ident = {
    require(sym.isStaticMember,
        "encodeStaticMemberSym called with non-static symbol: " + sym)
    js.Ident(
        mangleJSName(sym.name.toString) +
        makeParamsString(List(internalName(sym.tpe))),
        Some(sym.originalName.decoded))
  }

  def encodeLocalSym(sym: Symbol, freshName: Symbol => String)(
      implicit pos: Position): js.Ident = {
    require(!sym.owner.isClass && sym.isTerm && !sym.isMethod && !sym.isModule,
        "encodeLocalSym called with non-local symbol: " + sym)
    js.Ident(mangleJSName(freshName(sym)), Some(sym.originalName.decoded))
  }

  def encodeClassSym(sym: Symbol)(implicit pos: Position): js.Tree = {
    require(sym.isClass, "encodeClassSym called with non-class symbol: " + sym)
    js.DotSelect(envField("c"), encodeClassFullNameIdent(sym))
  }

  def encodeClassOfType(tpe: Type)(implicit pos: Position): js.Tree = {
    js.ApplyMethod(encodeClassDataOfType(tpe), js.Ident("getClassOf"), Nil)
  }

  def encodeModuleSymInstance(sym: Symbol)(implicit pos: Position): js.Tree = {
    require(sym.isModuleClass,
        "encodeModuleSymInstance called with non-moduleClass symbol: " + sym)
    js.DotSelect(envField("moduleInstances"),
        encodeModuleFullNameIdent(sym))
  }

  def encodeModuleSym(sym: Symbol)(implicit pos: Position): js.Tree = {
    require(sym.isModuleClass,
        "encodeModuleSym called with non-moduleClass symbol: " + sym)

    if (foreignIsImplClass(sym))
      envField("impls")
    else
      js.Apply(js.DotSelect(envField("modules"),
          encodeModuleFullNameIdent(sym)), Nil)
  }

  private def foreignIsImplClass(sym: Symbol): Boolean =
    sym.isModuleClass && nme.isImplClassName(sym.name)

  def encodeIsInstanceOf(value: js.Tree, tpe: Type)(
      implicit pos: Position): js.Tree = {
    encodeIsAsInstanceOf("is")(value, tpe)
  }

  def encodeAsInstanceOf(value: js.Tree, tpe: Type)(
      implicit pos: Position): js.Tree = {
    encodeIsAsInstanceOf("as")(value, tpe)
  }

  private def encodeIsAsInstanceOf(prefix: String)(value: js.Tree, tpe: Type)(
      implicit pos: Position): js.Tree = {
    toTypeKind(tpe) match {
      case REFERENCE(ScalaRTMapped(rtSym)) =>
        encodeIsAsInstanceOf(prefix)(value, rtSym.tpe)
      case array : ARRAY =>
        val elemSym = array.elementKind.toType.typeSymbol match {
          case ScalaRTMapped(rtSym) => rtSym
          case x => x
        }
        js.ApplyMethod(envField(prefix+"ArrayOf"),
            encodeClassFullNameIdent(elemSym),
            List(value, js.IntLiteral(array.dimensions)))
      case _ =>
        js.ApplyMethod(envField(prefix),
            encodeClassFullNameIdent(tpe.typeSymbol), List(value))
    }
  }

  def encodeClassDataOfType(tpe: Type)(implicit pos: Position): js.Tree = {
    toTypeKind(tpe) match {
      case REFERENCE(ScalaRTMapped(rtSym)) =>
        encodeClassDataOfType(rtSym.tpe)
      case array : ARRAY =>
        var result = encodeClassDataOfType(array.elementKind.toType)
        for (i <- 0 until array.dimensions)
          result = js.ApplyMethod(result, js.Ident("getArrayOf"), Nil)
        result

      case _ => encodeClassDataOfSym(tpe.typeSymbol)
    }
  }

  private def encodeClassDataOfSym(sym: Symbol)(implicit pos: Position): js.Tree = {
    js.DotSelect(envField("data"), encodeClassFullNameIdent(sym))
  }

  def encodeClassFullNameIdent(sym: Symbol)(implicit pos: Position): js.Ident = {
    js.Ident(encodeClassFullName(sym), Some(sym.fullName))
  }

  def encodeModuleFullNameIdent(sym: Symbol)(implicit pos: Position): js.Ident = {
    js.Ident(encodeModuleFullName(sym), Some(sym.fullName))
  }

  def encodeClassFullName(sym: Symbol): String = {
    val base = encodeFullNameInternal(sym)
    if (sym.isModuleClass && !foreignIsImplClass(sym)) base + "$" else base
  }

  def encodeModuleFullName(sym: Symbol): String =
    encodeFullNameInternal(sym)

  private def encodeFullNameInternal(sym: Symbol): String = {
    val tmp = sym.fullName.replace("_", "$und").replace(".", "_")
    mangleJSName(tmp)
  }

  // Encoding of method signatures

  private def makeParamsString(sym: Symbol): String = {
    val tpe = sym.tpe
    val paramTypeNames = tpe.params map (p => internalName(p.tpe))
    makeParamsString(
        if (sym.isClassConstructor) paramTypeNames
        else paramTypeNames :+ internalName(tpe.resultType))
  }

  private def makeParamsString(paramAndResultTypeNames: List[String]) =
    paramAndResultTypeNames.mkString(OuterSep, OuterSep, "")

  /** Compute the internal name for a type
   *  The internal name is inspired by the encoding of the JVM, with some
   *  tweaks to use only valid JS identifier characters
   *  - I for Int, Z for Boolean, V for Unit, etc. for primitive types
   *  - Lclassname where classname is the full name of a class
   *  - Aelem for arrays
   *  and for further default compression in the context of Scala.js:
   *  - O for java.lang.Object and T for java.lang.String
   *
   *  It might be worth investigating other special cases for classes of the
   *  Scala language: Function types, Tuple types?
   */
  private def internalName(tpe: Type): String = internalName(toTypeKind(tpe))

  private def internalName(kind: TypeKind): String = kind match {
    case kind: ValueTypeKind => kind.primitiveCharCode
    case REFERENCE(cls) =>
      /* Give shorter names to classes used *very* often:
       * - Object, since it is the erasure of most type parameters
       * - String, if only for the ubiquitous toString()
       */
      cls match {
        case definitions.ObjectClass => "O"
        case definitions.StringClass => "T" // S is taken, use T for Text
        case _ => "L"+encodeClassFullName(cls)
      }
    case ARRAY(elem) => "A"+internalName(elem)
  }

  /** mangles names that are illegal in JavaScript by prepending a $
   *  also mangles names that would collide with these mangled names
   */
  private def mangleJSName(name: String) =
    if (js.isKeyword(name) || name(0).isDigit || name(0) == '$')
      "$" + name
    else name
}
