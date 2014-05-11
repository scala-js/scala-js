/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.ir

import Position._
import Transformers._
import Trees._
import Types._

/** Defines methods to emit Scala.js classes to JavaScript code.
 *  The results are not desugared. Only the class *structure* is decomposed
 *  to a lower representation in the IR. The output of all these methods should
 *  be further desugared before being emitted as JavaScript code.
 */
object ScalaJSClassEmitter {

  import JSDesugaring._

  /** Desugar a Scala.js class into ECMAScript 5 constructs */
  def genClassDef(tree: ClassDef): Tree = {
    implicit val pos = tree.pos
    val kind = tree.kind

    var reverseParts: List[Tree] = Nil

    if (kind == ClassKind.TraitImpl) {
      reverseParts ::= genTraitImpl(tree)
    } else {
      if (kind.isClass)
        reverseParts ::= genClass(tree)
      if (kind.isClass || kind == ClassKind.Interface)
        reverseParts ::= genInstanceTests(tree)
      reverseParts ::= genArrayInstanceTests(tree)
      reverseParts ::= genTypeData(tree)
      if (kind.isClass)
        reverseParts ::= genSetTypeData(tree)
      if (kind == ClassKind.ModuleClass)
        reverseParts ::= genModuleAccessor(tree)
      if (kind.isClass)
        reverseParts ::= genClassExports(tree)
    }

    Block(reverseParts.reverse)
  }

  def genClass(tree: ClassDef): Tree = {
    val typeFunctionDef = genConstructor(tree)
    val memberDefs = tree.defs collect {
      case m: MethodDef =>
        genMethod(tree, m)
      case p: PropertyDef =>
        genProperty(tree, p)
    }

    Block(typeFunctionDef :: memberDefs)(tree.pos)
  }

  /** Generates the JS constructor for a class. */
  def genConstructor(tree: ClassDef): Tree = {
    assert(tree.kind.isClass)
    assert(tree.parent.isDefined)

    val classIdent = tree.name
    val tpe = ClassType(classIdent.name)
    val parentIdent = tree.parent.get
    val parentTpe = ClassType(parentIdent.name)

    val ctorFun = {
      val superCtorCall = {
        implicit val pos = tree.pos
        JSApply(
            JSDotSelect(encodeClassVar(parentTpe), Ident("call")),
            List(This()(tpe)))
      }
      val fieldDefs = for {
        field @ VarDef(name, vtpe, mutable, rhs) <- tree.defs
      } yield {
        implicit val pos = field.pos
        Assign(JSDotSelect(This()(tpe), name), rhs)
      }
      Function(tpe, Nil, UndefType,
          Block(superCtorCall :: fieldDefs)(tree.pos))(tree.pos)
    }

    {
      implicit val pos = tree.pos
      val typeVar = encodeClassVar(tpe)
      val docComment = DocComment("@constructor")
      val ctorDef = Assign(typeVar, ctorFun)
      val chainProto =
        Assign(typeVar.prototype,
            JSNew(JSDotSelect(envField("h"), parentIdent), Nil))
      val reassignConstructor =
        genAddToPrototype(tree, Ident("constructor"), typeVar)

      val inheritableCtorDef = {
        val inheritableCtorVar =
          JSDotSelect(envField("h"), classIdent)
        Block(
          DocComment("@constructor"),
          Assign(inheritableCtorVar, Function(DynType, Nil, UndefType, Skip())),
          Assign(inheritableCtorVar.prototype, typeVar.prototype)
        )
      }

      Block(docComment, ctorDef, chainProto, reassignConstructor,
          inheritableCtorDef)
    }
  }

  /** Generates a method. */
  def genMethod(cd: ClassDef, method: MethodDef): Tree = {
    implicit val pos = method.pos
    val methodFun = Function(ClassType(cd.name.name),
        method.args, method.resultType, method.body)
    genAddToPrototype(cd, method.name, methodFun)
  }

  /** Generates a property. */
  def genProperty(cd: ClassDef, property: PropertyDef): Tree = {
    implicit val pos = property.pos
    val classType = ClassType(cd.name.name)

    // defineProperty method
    val defProp =
      JSBracketSelect(VarRef(Ident("Object"), false)(DynType),
          StringLiteral("defineProperty"))

    // class prototype
    val proto = encodeClassVar(cd.name).prototype

    // property name
    val name = property.name match {
      case lit: StringLiteral => lit
      case id:  Ident         =>
        // We need to work around the closure compiler. Call propertyName to
        // get a string representation of the optimized name
        CallHelper("propertyName",
            JSObjectConstr(id -> IntLiteral(0) :: Nil) :: Nil)(StringType)
    }

    // Options passed to the defineProperty method
    val descriptor = JSObjectConstr {
      // Basic config
      val base =
        StringLiteral("enumerable") -> BooleanLiteral(true) :: Nil

      // Optionally add getter
      val wget =
        if (property.getterBody == EmptyTree) base
        else StringLiteral("get") ->
          Function(classType, Nil, DynType, property.getterBody) :: base

      // Optionally add setter
      if (property.setterBody == EmptyTree) wget
      else StringLiteral("set") ->
          Function(classType, property.setterArg :: Nil,
              UndefType, property.setterBody) :: wget
    }

    JSApply(defProp, proto :: name :: descriptor :: Nil)
  }

  /** Generate `classVar.prototype.name = value` */
  def genAddToPrototype(cd: ClassDef, name: PropertyName,
      value: Tree)(implicit pos: Position = value.pos): Tree = {
    val proto = encodeClassVar(cd.name).prototype
    val select = name match {
      case name: Ident         => JSDotSelect(proto, name)
      case name: StringLiteral => JSBracketSelect(proto, name)
    }
    Assign(select, value)
  }

  def genInstanceTests(tree: ClassDef): Tree = {
    import Definitions._
    import TreeDSL._

    implicit val pos = tree.pos

    val classIdent = tree.name
    val className = classIdent.name
    val displayName = decodeClassName(className)

    val isAncestorOfString =
      AncestorsOfStringClass.contains(className)
    val isAncestorOfHijackedNumberClass =
      AncestorsOfHijackedNumberClasses.contains(className)
    val isAncestorOfBoxedBooleanClass =
      AncestorsOfBoxedBooleanClass.contains(className)

    val objIdent = Ident("obj")
    val objParam = ParamDef(objIdent, DynType)
    val obj = VarRef(objIdent, mutable = false)(DynType)

    val createIsStat = {
      envField("is") DOT classIdent :=
        Function(NoType, List(objParam), DynType, Return {
          var test = (obj && (obj DOT "$classData") &&
              (obj DOT "$classData" DOT "ancestors" DOT classIdent))

          if (isAncestorOfString)
            test = test || (
                JSUnaryOp("typeof", obj) === StringLiteral("string"))
          if (isAncestorOfHijackedNumberClass)
            test = test || (
                JSUnaryOp("typeof", obj) === StringLiteral("number"))
          if (isAncestorOfBoxedBooleanClass)
            test = test || (
                JSUnaryOp("typeof", obj) === StringLiteral("boolean"))

          !(!test)
        })
    }

    val createAsStat = {
      envField("as") DOT classIdent :=
        Function(NoType, List(objParam), ClassType(className), {
          If(JSApply(envField("is") DOT classIdent, List(obj)) ||
              (obj === Null()), {
            Return(obj)
          }, {
            CallHelper("throwClassCastException",
                obj :: StringLiteral(displayName) :: Nil)(NothingType)
          })(NothingType)
      })
    }

    Block(createIsStat, createAsStat)
  }

  def genArrayInstanceTests(tree: ClassDef): Tree = {
    import Definitions._
    import TreeDSL._

    implicit val pos = tree.pos

    val classIdent = tree.name
    val className = classIdent.name
    val displayName = decodeClassName(className)

    val objIdent = Ident("obj")
    val objParam = ParamDef(objIdent, DynType)
    val obj = VarRef(objIdent, mutable = false)(DynType)

    val depthIdent = Ident("depth")
    val depthParam = ParamDef(depthIdent, IntType)
    val depth = VarRef(depthIdent, mutable = false)(IntType)

    val createIsArrayOfStat = {
      envField("isArrayOf") DOT classIdent :=
        Function(NoType, List(objParam, depthParam), DynType, Return {
          !(!(obj && (obj DOT "$classData") &&
              ((obj DOT "$classData" DOT "arrayDepth") === depth) &&
              (obj DOT "$classData" DOT "arrayBase" DOT "ancestors" DOT classIdent)))
        })
    }

    val createAsArrayOfStat = {
      envField("asArrayOf") DOT classIdent :=
        Function(NoType, List(objParam, depthParam), DynType, {
          If(JSApply(envField("isArrayOf") DOT classIdent, List(obj, depth)) ||
              (obj === Null()), {
            Return(obj)
          }, {
            CallHelper("throwArrayCastException",
                obj :: StringLiteral("L"+displayName+";") :: depth :: Nil)(NothingType)
          })(NothingType)
        })
    }

    Block(createIsArrayOfStat, createAsArrayOfStat)
  }

  def genTypeData(tree: ClassDef): Tree = {
    import Definitions._
    import TreeDSL._

    implicit val pos = tree.pos

    val classIdent = tree.name
    val className = classIdent.name
    val kind = tree.kind
    assert(kind.isType)

    val isHijackedBoxedClass =
      HijackedBoxedClasses.contains(className)
    val isAncestorOfHijackedClass =
      AncestorsOfHijackedClasses.contains(className)

    val parentData = tree.parent.fold[Tree] {
      Undefined()
    } { parent =>
      envField("d") DOT parent
    }

    val ancestorsRecord = JSObjectConstr(
        for (ancestor <- classIdent :: tree.ancestors)
          yield (ancestor, IntLiteral(1)))

    val typeData = JSNew(envField("ClassTypeData"), List(
        JSObjectConstr(List(classIdent -> IntLiteral(0))),
        BooleanLiteral(kind == ClassKind.Interface),
        StringLiteral(decodeClassName(className)),
        parentData,
        ancestorsRecord
    ) ++ (
        // Optional parameter isInstance
        if (isHijackedBoxedClass) {
          /* Hijacked boxed classes have a special isInstanceOf test. */
          List(Function(NoType, List(ParamDef(Ident("x"), DynType)), BooleanType, Return {
            IsInstanceOf(VarRef(Ident("x"), false)(DynType), ClassType(className))
          }))
        } else if (isAncestorOfHijackedClass) {
          /* Ancestors of hijacked classes have a normal
           * ScalaJS.is.pack_Class test but with a non-standard behavior. */
          List(envField("is") DOT classIdent)
        } else {
          // For other classes, the isInstance function can be inferred.
          Nil
        }
    ))

    envField("d") DOT classIdent := typeData
  }

  def genSetTypeData(tree: ClassDef): Tree = {
    import TreeDSL._

    implicit val pos = tree.pos

    assert(tree.kind.isClass)

    encodeClassVar(tree.name).prototype DOT "$classData" :=
      envField("d") DOT tree.name
  }

  def genModuleAccessor(tree: ClassDef): Tree = {
    import TreeDSL._

    implicit val pos = tree.pos

    val classIdent = tree.name
    val className = classIdent.name
    val tpe = ClassType(className)

    require(tree.kind == ClassKind.ModuleClass,
        s"genModuleAccessor called with non-module class: $className")
    assert(className.endsWith("$"))

    val moduleName = className.dropRight(1)
    val moduleIdent = Ident(moduleName)

    val moduleInstanceVar = envField("n") DOT moduleIdent
    val accessorVar = envField("m") DOT moduleIdent

    val createModuleInstanceField = {
      moduleInstanceVar := Undefined()
    }

    val createAccessor = {
      accessorVar := Function(NoType, Nil, DynType, Block(
        If(!(moduleInstanceVar), {
          moduleInstanceVar := New(tpe, Ident("init___"), Nil)
        }, Skip())(UndefType),
        Return(moduleInstanceVar)
      ))
    }

    Block(createModuleInstanceField, createAccessor)
  }

  def genClassExports(tree: ClassDef): Tree = {
    val exports = tree.defs collect {
      case e: ConstructorExportDef =>
        genConstructorExportDef(tree, e)
      case e: ModuleExportDef =>
        genModuleExportDef(tree, e)
    }

    Block(exports)(tree.pos)
  }

  def genConstructorExportDef(cd: ClassDef, tree: ConstructorExportDef): Tree = {
    import TreeDSL._

    implicit val pos = tree.pos
    val classType = ClassType(cd.name.name)
    val ConstructorExportDef(fullName, args, body) = tree

    val baseCtor = envField("c") DOT cd.name
    val (createNamespace, expCtorVar) = genCreateNamespaceInExports(fullName)

    Block(
      createNamespace,
      DocComment("@constructor"),
      expCtorVar := Function(classType, args, DynType, Block(
        JSApply(JSDotSelect(baseCtor, Ident("call")), List(This()(DynType))),
        body
      )),
      expCtorVar DOT "prototype" := baseCtor DOT "prototype"
    )
  }

  def genModuleExportDef(cd: ClassDef, tree: ModuleExportDef): Tree = {
    import TreeDSL._

    implicit val pos = tree.pos

    val baseAccessor =
      envField("m") DOT cd.name.name.dropRight(1)
    val (createNamespace, expAccessorVar) =
      genCreateNamespaceInExports(tree.fullName)

    Block(
      createNamespace,
      expAccessorVar := baseAccessor
    )
  }

  def genTraitImpl(tree: ClassDef): Tree = {
    val defs = tree.defs collect {
      case m: MethodDef =>
        genTraitImplMethod(tree, m)
    }
    Block(defs)(tree.pos)
  }

  def genTraitImplMethod(cd: ClassDef, tree: MethodDef): Tree = {
    implicit val pos = tree.pos
    val MethodDef(name: Ident, args, resultType, body) = tree
    Assign(
        JSDotSelect(envField("i"), name),
        Function(NoType, args, resultType, body))
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
      implicit pos: Position): (Tree, Tree) = {
    val parts = qualName.split("\\.")
    val statements = List.newBuilder[Tree]
    var namespace = envField("e")
    for (i <- 0 until parts.length-1) {
      namespace = JSBracketSelect(namespace, StringLiteral(parts(i)))
      statements +=
        Assign(namespace, JSBinaryOp("||", namespace, JSObjectConstr(Nil)))
    }
    val lhs = JSBracketSelect(namespace, StringLiteral(parts.last))
    (Block(statements.result()), lhs)
  }

}
