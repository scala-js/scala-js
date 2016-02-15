/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.linker.backend.emitter

import org.scalajs.core.ir._
import Position._
import Transformers._
import org.scalajs.core.ir.Trees._
import Types._

import org.scalajs.core.tools.sem._
import CheckedBehavior.Unchecked

import org.scalajs.core.tools.javascript.{Trees => js}
import org.scalajs.core.tools.linker.backend.OutputMode
import org.scalajs.core.tools.linker.{LinkedClass, LinkingUnit}

/** Defines methods to emit Scala.js classes to JavaScript code.
 *  The results are completely desugared.
 *
 *  The only reason this is not `private[emitter]` is because `RhinoJSEnv`
 *  needs it.
 */
private[scalajs] final class ScalaJSClassEmitter(
    private[emitter] val outputMode: OutputMode,
    internalOptions: InternalOptions,
    linkingUnit: LinkingUnit) {

  private val jsDesugaring = new JSDesugaring(internalOptions)

  import ScalaJSClassEmitter._
  import jsDesugaring._

  def this(outputMode: OutputMode, linkingUnit: LinkingUnit) =
    this(outputMode, InternalOptions(), linkingUnit)

  private[emitter] lazy val linkedClassByName: Map[String, LinkedClass] =
    linkingUnit.classDefs.map(c => c.encodedName -> c).toMap

  private[emitter] def isInterface(className: String): Boolean = {
    /* TODO In theory, there is a flaw in the incremental behavior about this.
     *
     * This method is used to desugar ApplyStatically nodes. Depending on
     * whether className is a class or an interface, the desugaring changes.
     * This means that the result of desugaring an ApplyStatically depends on
     * some global knowledge from the whole program (and not only of the method
     * being desugared). If, from one run to the next, className switches from
     * being a class to an interface or vice versa, there is nothing demanding
     * that the IR of the call site change, yet the desugaring should change.
     * In theory, this causes a flaw in the incremental behavior of the
     * Emitter, which will not invalidate its cache for this.
     *
     * In practice, this should not happen, though. A method can only be called
     * statically by subclasses and subtraits at the Scala *language* level.
     * We also know that when a class/trait changes, all its subclasses and
     * subtraits are recompiled by sbt's incremental compilation. This should
     * mean that the input IR is always changed in that case anyway.
     *
     * It would be good to fix thoroughly if the Emitter/ScalaJSClassEmitter
     * gets something for incremental whole-program updates, but for now, we
     * live with the theoretical flaw.
     */
    linkedClassByName(className).kind == ClassKind.Interface
  }

  private[emitter] def semantics: Semantics = linkingUnit.semantics

  private implicit def implicitOutputMode: OutputMode = outputMode

  def genDeclareTypeData(tree: LinkedClass): js.Tree = {
    implicit val pos = tree.pos
    envFieldDef("d", tree.encodedName, js.Null(), mutable = true)
  }

  def genDeclareModule(tree: LinkedClass): js.Tree = {
    implicit val pos = tree.pos
    if (tree.kind.hasModuleAccessor)
      envFieldDef("n", tree.encodedName, js.Undefined(), mutable = true)
    else
      js.Skip()
  }

  /** Desugar a Scala.js class into ECMAScript 5 constructs
   *
   *  @param tree The IR tree to emit to raw JavaScript
   *  @param ancestors Encoded names of the ancestors of the class (not only
   *                   parents), including the class itself.
   */
  def genClassDef(tree: LinkedClass): js.Tree = {
    implicit val pos = tree.pos
    val kind = tree.kind

    var reverseParts: List[js.Tree] = Nil

    reverseParts ::= genStaticMembers(tree)
    if (kind == ClassKind.Interface)
      reverseParts ::= genDefaultMethods(tree)
    if (kind.isAnyScalaJSDefinedClass && tree.hasInstances)
      reverseParts ::= genClass(tree)
    if (needInstanceTests(tree)) {
      reverseParts ::= genInstanceTests(tree)
      reverseParts ::= genArrayInstanceTests(tree)
    }
    if (tree.hasRuntimeTypeInfo)
      reverseParts ::= genTypeData(tree)
    if (kind.isClass && tree.hasInstances && tree.hasRuntimeTypeInfo)
      reverseParts ::= genSetTypeData(tree)
    if (kind.hasModuleAccessor)
      reverseParts ::= genModuleAccessor(tree)
    reverseParts ::= genClassExports(tree)

    js.Block(reverseParts.reverse)
  }

  def genStaticMembers(tree: LinkedClass): js.Tree = {
    val className = tree.name.name
    val staticMemberDefs =
      tree.staticMethods.map(m => genMethod(className, m.tree))
    js.Block(staticMemberDefs)(tree.pos)
  }

  def genDefaultMethods(tree: LinkedClass): js.Tree = {
    val className = tree.name.name
    val defaultMethodDefs =
      tree.memberMethods.map(m => genDefaultMethod(className, m.tree))
    js.Block(defaultMethodDefs)(tree.pos)
  }

  def genClass(tree: LinkedClass): js.Tree = {
    val className = tree.name.name
    val typeFunctionDef = genConstructor(tree)
    val memberDefs =
      tree.memberMethods.map(m => genMethod(className, m.tree))

    val exportedDefs = genExportedMembers(tree)

    val allDefsBlock =
      js.Block(typeFunctionDef +: memberDefs :+ exportedDefs)(tree.pos)

    outputMode match {
      case OutputMode.ECMAScript51Global | OutputMode.ECMAScript51Isolated =>
        allDefsBlock

      case OutputMode.ECMAScript6 =>
        val allDefs = allDefsBlock match {
          case js.Block(allDefs) => allDefs
          case js.Skip()         => Nil
          case oneDef            => List(oneDef)
        }
        genES6Class(tree, allDefs)
    }
  }

  /** Generates an ECMAScript 6 class for a linked class. */
  def genES6Class(tree: LinkedClass, members: List[js.Tree]): js.Tree = {
    require(outputMode == OutputMode.ECMAScript6)

    val className = tree.name.name
    val classIdent = encodeClassVar(className)(
        outputMode, tree.name.pos).asInstanceOf[js.VarRef].ident

    val parentVar = for (parentIdent <- tree.superClass) yield {
      implicit val pos = parentIdent.pos
      if (!tree.kind.isJSClass)
        encodeClassVar(parentIdent.name)
      else
        genRawJSClassConstructor(linkedClassByName(parentIdent.name))
    }

    js.ClassDef(Some(classIdent), parentVar, members)(tree.pos)
  }

  /** Generates the JS constructor for a class. */
  def genConstructor(tree: LinkedClass): js.Tree = {
    assert(tree.kind.isAnyScalaJSDefinedClass)
    assert(tree.superClass.isDefined || tree.name.name == Definitions.ObjectClass,
        s"Class ${tree.name.name} is missing a parent class")

    outputMode match {
      case OutputMode.ECMAScript51Global | OutputMode.ECMAScript51Isolated =>
        genES5Constructor(tree)

      case OutputMode.ECMAScript6 =>
        genES6Constructor(tree)
    }
  }

  /** Generates the JS constructor for a class, ES5 style. */
  private def genES5Constructor(tree: LinkedClass): js.Tree = {
    implicit val pos = tree.pos

    val className = tree.name.name
    val isJSClass = tree.kind.isJSClass

    def makeInheritableCtorDef(ctorToMimic: js.Tree) = {
      js.Block(
        js.DocComment("@constructor"),
        envFieldDef("h", className, js.Function(Nil, js.Skip())),
        js.Assign(envField("h", className).prototype, ctorToMimic.prototype)
      )
    }

    val ctorFun = if (!isJSClass) {
      val superCtorCall = tree.superClass.fold[js.Tree] {
        js.Skip()
      } { parentIdent =>
        js.Apply(
            js.DotSelect(encodeClassVar(parentIdent.name), js.Ident("call")),
            List(js.This()))
      }
      val fieldDefs = genFieldDefs(tree)
      js.Function(Nil, js.Block(superCtorCall :: fieldDefs))
    } else {
      genConstructorFunForJSClass(tree)
    }

    val typeVar = encodeClassVar(className)
    val docComment = js.DocComment("@constructor")
    val ctorDef = envFieldDef("c", className, ctorFun)

    val chainProto = tree.superClass.fold[js.Tree] {
      js.Skip()
    } { parentIdent =>
      val (inheritedCtorDef, inheritedCtorRef) = if (!isJSClass) {
        (js.Skip(), envField("h", parentIdent.name))
      } else {
        val superCtor = genRawJSClassConstructor(
            linkedClassByName(parentIdent.name))
        (makeInheritableCtorDef(superCtor), envField("h", className))
      }
      js.Block(
          inheritedCtorDef,
          js.Assign(typeVar.prototype, js.New(inheritedCtorRef, Nil)),
          genAddToPrototype(className, js.StringLiteral("constructor"), typeVar)
      )
    }

    val inheritableCtorDef =
      if (isJSClass) js.Skip()
      else makeInheritableCtorDef(typeVar)

    js.Block(docComment, ctorDef, chainProto, inheritableCtorDef)
  }

  /** Generates the JS constructor for a class, ES6 style. */
  private def genES6Constructor(tree: LinkedClass): js.Tree = {
    implicit val pos = tree.pos

    if (tree.kind.isJSClass) {
      val js.Function(params, body) = genConstructorFunForJSClass(tree)
      js.MethodDef(static = false, js.Ident("constructor"), params, body)
    } else {
      val fieldDefs = genFieldDefs(tree)
      if (fieldDefs.isEmpty && outputMode == OutputMode.ECMAScript6) {
        js.Skip()
      } else {
        val superCtorCall = tree.superClass.fold[js.Tree] {
          js.Skip()(tree.pos)
        } { parentIdent =>
          js.Apply(js.Super(), Nil)
        }
        js.MethodDef(static = false, js.Ident("constructor"), Nil,
            js.Block(superCtorCall :: fieldDefs))
      }
    }
  }

  private def genConstructorFunForJSClass(tree: LinkedClass): js.Function = {
    implicit val pos = tree.pos

    require(tree.kind.isJSClass)

    tree.exportedMembers.map(_.tree) collectFirst {
      case MethodDef(false, StringLiteral("constructor"), params, _, body) =>
        desugarToFunction(this, tree.encodedName,
            params, body, isStat = true)
    } getOrElse {
      throw new IllegalArgumentException(
          s"${tree.encodedName} does not have an exported constructor")
    }
  }

  /** Generates the creation of fields for a class. */
  private def genFieldDefs(tree: LinkedClass): List[js.Tree] = {
    val tpe = ClassType(tree.encodedName)
    for {
      field @ FieldDef(name, ftpe, mutable) <- tree.fields
    } yield {
      implicit val pos = field.pos
      val selectField = (name: @unchecked) match {
        case name: Ident => Select(This()(tpe), name)(ftpe)
      }
      desugarTree(this, tree.encodedName,
          Assign(selectField, zeroOf(ftpe)), isStat = true)
    }
  }

  /** Generates a method. */
  def genMethod(className: String, method: MethodDef): js.Tree = {
    implicit val pos = method.pos

    val methodFun0 = desugarToFunction(this, className,
        method.args, method.body, method.resultType == NoType)

    val methodFun = if (Definitions.isConstructorName(method.name.name)) {
      // init methods have to return `this` so that we can chain them to `new`
      js.Function(methodFun0.args, {
        implicit val pos = methodFun0.body.pos
        js.Block(
            methodFun0.body,
            js.Return(js.This()))
      })(methodFun0.pos)
    } else {
      methodFun0
    }

    if (method.static) {
      val Ident(methodName, origName) = method.name
      envFieldDef(
          "s", className + "__" + methodName, origName,
          methodFun)
    } else {
      outputMode match {
        case OutputMode.ECMAScript51Global | OutputMode.ECMAScript51Isolated =>
          genAddToPrototype(className, method.name, methodFun)

        case OutputMode.ECMAScript6 =>
          js.MethodDef(static = false, genPropertyName(method.name),
              methodFun.args, methodFun.body)
      }
    }
  }

  /** Generates a default method. */
  def genDefaultMethod(className: String, method: MethodDef): js.Tree = {
    implicit val pos = method.pos

    /* TODO The identifier `$thiz` cannot be produced by 0.6.x compilers due to
     * their name mangling, which guarantees that it is unique. We should find
     * a better way to do this in the future, though.
     */
    val thisIdent = js.Ident("$thiz", Some("this"))

    val methodFun0 = desugarToFunction(this, className, Some(thisIdent),
        method.args, method.body, method.resultType == NoType)

    val methodFun = js.Function(
        js.ParamDef(thisIdent, rest = false) :: methodFun0.args,
        methodFun0.body)(methodFun0.pos)

    val Ident(methodName, origName) = method.name

    envFieldDef(
        "f", className + "__" + methodName, origName,
        methodFun)
  }

  /** Generates a property. */
  def genProperty(className: String, property: PropertyDef): js.Tree = {
    outputMode match {
      case OutputMode.ECMAScript51Global | OutputMode.ECMAScript51Isolated =>
        genPropertyES5(className, property)
      case OutputMode.ECMAScript6 =>
        genPropertyES6(className, property)
    }
  }

  private def genPropertyES5(className: String,
      property: PropertyDef): js.Tree = {
    implicit val pos = property.pos

    // defineProperty method
    val defProp =
      genIdentBracketSelect(js.VarRef(js.Ident("Object")), "defineProperty")

    // class prototype
    val proto = encodeClassVar(className).prototype

    // property name
    val name = property.name match {
      case StringLiteral(value) =>
        js.StringLiteral(value)
      case id: Ident =>
        // We need to work around the closure compiler. Call propertyName to
        // get a string representation of the optimized name
        genCallHelper("propertyName",
            js.ObjectConstr(transformIdent(id) -> js.IntLiteral(0) :: Nil))
    }

    // Options passed to the defineProperty method
    val descriptor = js.ObjectConstr {
      // Basic config
      val base =
        js.StringLiteral("enumerable") -> js.BooleanLiteral(true) :: Nil

      // Optionally add getter
      val wget = {
        if (property.getterBody == EmptyTree) base
        else {
          val fun = desugarToFunction(this, className,
              Nil, property.getterBody, isStat = false)
          js.StringLiteral("get") -> fun :: base
        }
      }

      // Optionally add setter
      if (property.setterBody == EmptyTree) wget
      else {
        val fun = desugarToFunction(this, className,
            property.setterArg :: Nil, property.setterBody, isStat = true)
        js.StringLiteral("set") -> fun :: wget
      }
    }

    js.Apply(defProp, proto :: name :: descriptor :: Nil)
  }

  private def genPropertyES6(className: String,
      property: PropertyDef): js.Tree = {
    implicit val pos = property.pos

    val propName = genPropertyName(property.name)

    val getter = {
      if (property.getterBody == EmptyTree) js.Skip()
      else {
        val fun = desugarToFunction(this, className,
            Nil, property.getterBody, isStat = false)
        js.GetterDef(static = false, propName, fun.body)
      }
    }

    val setter = {
      if (property.setterBody == EmptyTree) js.Skip()
      else {
        val fun = desugarToFunction(this, className,
            property.setterArg :: Nil, property.setterBody, isStat = true)
        js.SetterDef(static = false, propName, fun.args.head, fun.body)
      }
    }

    js.Block(getter, setter)
  }

  /** Generate `classVar.prototype.name = value` */
  def genAddToPrototype(className: String, name: js.PropertyName,
      value: js.Tree)(implicit pos: Position): js.Tree = {
    val proto = encodeClassVar(className).prototype
    val select = name match {
      case name: js.Ident         => js.DotSelect(proto, name)
      case name: js.StringLiteral => genBracketSelect(proto, name)
    }
    js.Assign(select, value)
  }

  /** Generate `classVar.prototype.name = value` */
  def genAddToPrototype(className: String, name: PropertyName,
      value: js.Tree)(implicit pos: Position): js.Tree = {
    genAddToPrototype(className, genPropertyName(name), value)
  }

  def genPropertyName(name: PropertyName): js.PropertyName = name match {
    case ident: Ident         => transformIdent(ident)
    case StringLiteral(value) => js.StringLiteral(value)(name.pos)
  }

  private[tools] def needInstanceTests(tree: LinkedClass): Boolean = {
    tree.hasInstanceTests || {
      tree.hasRuntimeTypeInfo &&
      ClassesWhoseDataReferToTheirInstanceTests.contains(tree.encodedName)
    }
  }

  def genInstanceTests(tree: LinkedClass): js.Tree = {
    import Definitions._
    import TreeDSL._

    implicit val pos = tree.pos

    if (tree.kind.isClass || tree.kind == ClassKind.Interface ||
        tree.name.name == Definitions.StringClass) {
      val className = tree.name.name
      val displayName = decodeClassName(className)

      val isAncestorOfString =
        AncestorsOfStringClass.contains(className)
      val isAncestorOfHijackedNumberClass =
        AncestorsOfHijackedNumberClasses.contains(className)
      val isAncestorOfBoxedBooleanClass =
        AncestorsOfBoxedBooleanClass.contains(className)

      val objParam = js.ParamDef(Ident("obj"), rest = false)
      val obj = objParam.ref

      val createIsStat = {
        envFieldDef("is", className,
          js.Function(List(objParam), js.Return(className match {
            case Definitions.ObjectClass =>
              js.BinaryOp(JSBinaryOp.!==, obj, js.Null())

            case Definitions.StringClass =>
              js.UnaryOp(JSUnaryOp.typeof, obj) === js.StringLiteral("string")

            case Definitions.RuntimeNothingClass =>
              // Even null is not an instance of Nothing
              js.BooleanLiteral(false)

            case _ =>
              var test = {
                genIsScalaJSObject(obj) &&
                genIsClassNameInAncestors(className,
                    obj DOT "$classData" DOT "ancestors")
              }

              if (isAncestorOfString)
                test = test || (
                    js.UnaryOp(JSUnaryOp.typeof, obj) === js.StringLiteral("string"))
              if (isAncestorOfHijackedNumberClass)
                test = test || (
                    js.UnaryOp(JSUnaryOp.typeof, obj) === js.StringLiteral("number"))
              if (isAncestorOfBoxedBooleanClass)
                test = test || (
                    js.UnaryOp(JSUnaryOp.typeof, obj) === js.StringLiteral("boolean"))

              !(!test)
          })))
      }

      val createAsStat = if (semantics.asInstanceOfs == Unchecked) {
        js.Skip()
      } else {
        envFieldDef("as", className,
          js.Function(List(objParam), js.Return(className match {
            case Definitions.ObjectClass =>
              obj

            case _ =>
              val throwError = {
                genCallHelper("throwClassCastException",
                    obj, js.StringLiteral(displayName))
              }
              if (className == RuntimeNothingClass) {
                // Always throw for .asInstanceOf[Nothing], even for null
                throwError
              } else {
                js.If(js.Apply(envField("is", className), List(obj)) ||
                    (obj === js.Null()), {
                  obj
                }, {
                  throwError
                })
              }
        })))
      }

      js.Block(createIsStat, createAsStat)
    } else {
      js.Skip()
    }
  }

  def genArrayInstanceTests(tree: LinkedClass): js.Tree = {
    import Definitions._
    import TreeDSL._

    implicit val pos = tree.pos

    val className = tree.name.name
    val displayName = decodeClassName(className)

    val objParam = js.ParamDef(Ident("obj"), rest = false)
    val obj = objParam.ref

    val depthParam = js.ParamDef(Ident("depth"), rest = false)
    val depth = depthParam.ref

    val createIsArrayOfStat = {
      envFieldDef("isArrayOf", className,
        js.Function(List(objParam, depthParam), className match {
          case Definitions.ObjectClass =>
            val dataVarDef = genLet(Ident("data"), mutable = false, {
              obj && (obj DOT "$classData")
            })
            val data = dataVarDef.ref
            js.Block(
              dataVarDef,
              js.If(!data, {
                js.Return(js.BooleanLiteral(false))
              }, {
                val arrayDepthVarDef = genLet(Ident("arrayDepth"), mutable = false, {
                  (data DOT "arrayDepth") || js.IntLiteral(0)
                })
                val arrayDepth = arrayDepthVarDef.ref
                js.Block(
                  arrayDepthVarDef,
                  js.Return {
                    // Array[A] </: Array[Array[A]]
                    !js.BinaryOp(JSBinaryOp.<, arrayDepth, depth) && (
                      // Array[Array[A]] <: Array[Object]
                      js.BinaryOp(JSBinaryOp.>, arrayDepth, depth) ||
                      // Array[Int] </: Array[Object]
                      !genIdentBracketSelect(data DOT "arrayBase", "isPrimitive")
                    )
                  })
              }))

          case _ =>
            js.Return(!(!({
              genIsScalaJSObject(obj) &&
              ((obj DOT "$classData" DOT "arrayDepth") === depth) &&
              genIsClassNameInAncestors(className,
                  obj DOT "$classData" DOT "arrayBase" DOT "ancestors")
            })))
        }))
    }

    val createAsArrayOfStat = if (semantics.asInstanceOfs == Unchecked) {
      js.Skip()
    } else {
      envFieldDef("asArrayOf", className,
        js.Function(List(objParam, depthParam), js.Return {
          js.If(js.Apply(envField("isArrayOf", className), List(obj, depth)) ||
              (obj === js.Null()), {
            obj
          }, {
            genCallHelper("throwArrayCastException",
                obj, js.StringLiteral("L"+displayName+";"), depth)
          })
        }))
    }

    js.Block(createIsArrayOfStat, createAsArrayOfStat)
  }

  private def genIsScalaJSObject(obj: js.Tree)(implicit pos: Position): js.Tree = {
    import TreeDSL._
    obj && (obj DOT "$classData")
  }

  private def genIsClassNameInAncestors(className: String, ancestors: js.Tree)(
      implicit pos: Position): js.Tree = {
    import TreeDSL._
    ancestors DOT className
  }

  def genTypeData(tree: LinkedClass): js.Tree = {
    import Definitions._
    import TreeDSL._

    implicit val pos = tree.pos

    val classIdent = transformIdent(tree.name)
    val className = classIdent.name
    val kind = tree.kind

    val isObjectClass =
      className == ObjectClass
    val isHijackedBoxedClass =
      HijackedBoxedClasses.contains(className)
    val isAncestorOfHijackedClass =
      AncestorsOfHijackedClasses.contains(className)
    val isRawJSType =
      kind == ClassKind.RawJSType || kind.isJSClass

    val isRawJSTypeParam =
      if (isRawJSType) js.BooleanLiteral(true)
      else js.Undefined()

    val parentData = if (linkingUnit.globalInfo.isParentDataAccessed) {
      tree.superClass.fold[js.Tree] {
        if (isObjectClass) js.Null()
        else js.Undefined()
      } { parent =>
        envField("d", parent.name)
      }
    } else {
      js.Undefined()
    }

    val ancestorsRecord = js.ObjectConstr(
        tree.ancestors.map(ancestor => (js.Ident(ancestor), js.IntLiteral(1))))

    val (isInstanceFun, isArrayOfFun) = {
      if (isObjectClass) {
        /* Object has special ScalaJS.is.O *and* ScalaJS.isArrayOf.O. */
        (envField("is", className), envField("isArrayOf", className))
      } else if (isHijackedBoxedClass) {
        /* Hijacked boxed classes have a special isInstanceOf test. */
        val xParam = js.ParamDef(Ident("x"), rest = false)
        (js.Function(List(xParam), js.Return {
          genIsInstanceOf(xParam.ref, ClassType(className))
        }), js.Undefined())
      } else if (isAncestorOfHijackedClass || className == StringClass) {
        /* java.lang.String and ancestors of hijacked classes have a normal
         * ScalaJS.is.pack_Class test but with a non-standard behavior. */
        (envField("is", className), js.Undefined())
      } else if (isRawJSType) {
        /* Raw JS types have an instanceof operator-based isInstanceOf test
         * dictated by their jsName. If there is no jsName, the test cannot
         * be performed and must throw.
         * JS classes have something similar, based on their constructor.
         */
        if (tree.jsName.isEmpty && kind != ClassKind.JSClass) {
          (envField("noIsInstance"), js.Undefined())
        } else {
          val jsCtor = genRawJSClassConstructor(tree)
          (js.Function(List(js.ParamDef(Ident("x"), rest = false)), js.Return {
            js.BinaryOp(JSBinaryOp.instanceof, js.VarRef(Ident("x")), jsCtor)
          }), js.Undefined())
        }
      } else {
        // For other classes, the isInstance function can be inferred.
        (js.Undefined(), js.Undefined())
      }
    }

    val allParams = List(
        js.ObjectConstr(List(classIdent -> js.IntLiteral(0))),
        js.BooleanLiteral(kind == ClassKind.Interface),
        js.StringLiteral(semantics.runtimeClassName(tree)),
        ancestorsRecord,
        isRawJSTypeParam,
        parentData,
        isInstanceFun,
        isArrayOfFun
    )

    val prunedParams =
      allParams.reverse.dropWhile(_.isInstanceOf[js.Undefined]).reverse

    val typeData = js.Apply(js.New(envField("TypeData"), Nil) DOT "initClass",
        prunedParams)

    envFieldDef("d", className, typeData)
  }

  def genSetTypeData(tree: LinkedClass): js.Tree = {
    import TreeDSL._

    implicit val pos = tree.pos

    assert(tree.kind.isClass)

    encodeClassVar(tree.name.name).prototype DOT "$classData" :=
      envField("d", tree.name.name)
  }

  def genModuleAccessor(tree: LinkedClass): js.Tree = {
    import TreeDSL._

    implicit val pos = tree.pos

    val classIdent = transformIdent(tree.name)
    val className = classIdent.name
    val tpe = ClassType(className)

    require(tree.kind.hasModuleAccessor,
        s"genModuleAccessor called with non-module class: $className")

    val createModuleInstanceField =
      envFieldDef("n", className, js.Undefined(), mutable = true)

    val createAccessor = {
      val moduleInstanceVar = envField("n", className)

      val assignModule = {
        val jsNew = js.New(encodeClassVar(className), Nil)
        val instantiateModule =
          if (tree.kind == ClassKind.JSModuleClass) jsNew
          else js.Apply(jsNew DOT js.Ident("init___"), Nil)
        moduleInstanceVar := instantiateModule
      }

      val initBlock = semantics.moduleInit match {
        case CheckedBehavior.Unchecked =>
          js.If(!(moduleInstanceVar), assignModule, js.Skip())
        case CheckedBehavior.Compliant =>
          js.If(moduleInstanceVar === js.Undefined(),
            js.Block(
                moduleInstanceVar := js.Null(),
                assignModule
            ),
            js.Skip())
        case CheckedBehavior.Fatal =>
          js.If(moduleInstanceVar === js.Undefined(), {
            js.Block(
                moduleInstanceVar := js.Null(),
                assignModule
            )
          }, js.If(moduleInstanceVar === js.Null(), {
            // throw new UndefinedBehaviorError(
            //     "Initializer of $className called before completion of its" +
            //     "super constructor")
            val decodedName = Definitions.decodeClassName(className).stripSuffix("$")
            val msg = s"Initializer of $decodedName called before completion " +
              "of its super constructor"
            val obj = js.New(encodeClassVar("sjsr_UndefinedBehaviorError"), Nil)
            val ctor = obj DOT js.Ident("init___T")
            js.Throw(js.Apply(ctor, js.StringLiteral(msg) :: Nil))
          }, js.Skip()))
      }

      val body = js.Block(initBlock, js.Return(moduleInstanceVar))

      envFieldDef("m", className, js.Function(Nil, body))
    }

    js.Block(createModuleInstanceField, createAccessor)
  }

  def genExportedMembers(tree: LinkedClass): js.Tree = {
    val exports = tree.exportedMembers map { member =>
      member.tree match {
        case MethodDef(false, StringLiteral("constructor"), _, _, _)
            if tree.kind.isJSClass =>
          js.Skip()(member.tree.pos)
        case m: MethodDef =>
          genMethod(tree.encodedName, m)
        case p: PropertyDef =>
          genProperty(tree.encodedName, p)
        case tree =>
          throw new AssertionError(
              "Illegal exportedMember " + tree.getClass.getName)
      }
    }

    js.Block(exports)(tree.pos)
  }

  def genClassExports(tree: LinkedClass): js.Tree = {
    val exports = tree.classExports collect {
      case e: ConstructorExportDef =>
        genConstructorExportDef(tree, e)
      case e: JSClassExportDef =>
        genJSClassExportDef(tree, e)
      case e: ModuleExportDef =>
        genModuleExportDef(tree, e)
    }

    js.Block(exports)(tree.pos)
  }

  def genConstructorExportDef(cd: LinkedClass,
      tree: ConstructorExportDef): js.Tree = {
    import TreeDSL._

    implicit val pos = tree.pos
    val classType = ClassType(cd.name.name)
    val ConstructorExportDef(fullName, args, body) = tree

    val baseCtor = envField("c", cd.name.name, cd.name.originalName)

    val thisIdent = js.Ident("$thiz")

    val js.Function(ctorParams, ctorBody) =
      desugarToFunction(this, cd.encodedName,
          Some(thisIdent), args, body, isStat = true)

    val exportedCtor = js.Function(ctorParams, js.Block(
      genLet(thisIdent, mutable = false, js.New(baseCtor, Nil)),
      ctorBody,
      js.Return(js.VarRef(thisIdent))
    ))

    val (createNamespace, expCtorVar) =
      genCreateNamespaceInExports(fullName)
    js.Block(
      createNamespace,
      js.DocComment("@constructor"),
      expCtorVar := exportedCtor,
      expCtorVar DOT "prototype" := baseCtor DOT "prototype"
    )
  }

  def genJSClassExportDef(cd: LinkedClass, tree: JSClassExportDef): js.Tree = {
    import TreeDSL._

    implicit val pos = tree.pos

    val classVar = envField("c", cd.name.name)
    genClassOrModuleExportDef(cd, tree.fullName, classVar)
  }

  def genModuleExportDef(cd: LinkedClass, tree: ModuleExportDef): js.Tree = {
    import TreeDSL._

    implicit val pos = tree.pos

    val baseAccessor = envField("m", cd.name.name)
    genClassOrModuleExportDef(cd, tree.fullName, baseAccessor)
  }

  private def genClassOrModuleExportDef(cd: LinkedClass, exportFullName: String,
      exportedValue: js.Tree)(implicit pos: Position): js.Tree = {
    import TreeDSL._

    val (createNamespace, expAccessorVar) =
      genCreateNamespaceInExports(exportFullName)
    js.Block(
      createNamespace,
      expAccessorVar := exportedValue
    )
  }

  // Helpers

  /** Gen JS code for assigning an rhs to a qualified name in the exports scope.
   *  For example, given the qualified name "foo.bar.Something", generates:
   *
   *  ScalaJS.e["foo"] = ScalaJS.e["foo"] || {};
   *  ScalaJS.e["foo"]["bar"] = ScalaJS.e["foo"]["bar"] || {};
   *
   *  Returns (statements, ScalaJS.e["foo"]["bar"]["Something"])
   */
  private def genCreateNamespaceInExports(qualName: String)(
      implicit pos: Position): (js.Tree, js.Tree) = {
    val parts = qualName.split("\\.")
    val statements = List.newBuilder[js.Tree]
    var namespace = envField("e")
    for (i <- 0 until parts.length-1) {
      namespace = genBracketSelect(namespace, js.StringLiteral(parts(i)))
      statements +=
        js.Assign(namespace, js.BinaryOp(JSBinaryOp.||,
            namespace, js.ObjectConstr(Nil)))
    }
    val lhs = genBracketSelect(namespace, js.StringLiteral(parts.last))
    (js.Block(statements.result()), lhs)
  }

}

private object ScalaJSClassEmitter {
  private val ClassesWhoseDataReferToTheirInstanceTests = {
    Definitions.AncestorsOfHijackedClasses +
    Definitions.ObjectClass + Definitions.StringClass
  }
}
