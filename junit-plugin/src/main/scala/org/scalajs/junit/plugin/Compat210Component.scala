package org.scalajs.junit.plugin

import scala.reflect.internal.Flags
import scala.tools.nsc._

/** Hacks to have our source code compatible with 2.10 and 2.11.
 *  It exposes 2.11 API in a 2.10 compiler.
 *
 *  @author Nicolas Stucki
 */
trait Compat210Component {

  val global: Global

  import global._

  def newValDef(sym: Symbol, rhs: Tree)(
      mods: Modifiers = Modifiers(sym.flags),
      name: TermName = sym.name.toTermName,
      tpt: Tree = TypeTreeMemberType(sym)): ValDef = {
    atPos(sym.pos)(ValDef(mods, name, tpt, rhs)) setSymbol sym
  }

  def newDefDef(sym: Symbol, rhs: Tree)(
      mods: Modifiers = Modifiers(sym.flags),
      name: TermName = sym.name.toTermName,
      tparams: List[TypeDef] = sym.typeParams.map(sym =>
          newTypeDef(sym, typeBoundsTree(sym))()),
      vparamss: List[List[ValDef]] = mapParamss(sym)(sym =>
          newValDef(sym, EmptyTree)()),
      tpt: Tree = TypeTreeMemberType(sym)): DefDef = {
    atPos(sym.pos)(DefDef(mods, name, tparams, vparamss, tpt, rhs)).setSymbol(sym)
  }

  def TypeTreeMemberType(sym: Symbol): TypeTree = {
    val resType = {
      if (sym.owner.isTerm) sym.tpe
      else sym.owner.thisType.memberType(sym)
    }.finalResultType
    atPos(sym.pos.focus)(TypeTree(resType))
  }

  private def newTypeDef(sym: Symbol, rhs: Tree)(
      mods: Modifiers = Modifiers(sym.flags),
      name: TypeName = sym.name.toTypeName,
      tparams: List[TypeDef] = sym.typeParams.map(sym =>
          newTypeDef(sym, typeBoundsTree(sym))())): TypeDef = {
    atPos(sym.pos)(TypeDef(mods, name, tparams, rhs)) setSymbol sym
  }

  private def typeBoundsTree(bounds: TypeBounds): TypeBoundsTree =
    TypeBoundsTree(TypeTree(bounds.lo), TypeTree(bounds.hi))

  private def typeBoundsTree(sym: Symbol): TypeBoundsTree =
    atPos(sym.pos)(typeBoundsTree(sym.info.bounds))

  implicit final class GenCompat(self: global.TreeGen) {
    def mkClassDef(mods: Modifiers, name: TypeName,
        tparams: List[TypeDef], templ: Template): ClassDef = {
      val isInterface =
        mods.isTrait && templ.body.forall(treeInfo.isInterfaceMember)
      val mods1 = if (isInterface) mods | Flags.INTERFACE else mods
      ClassDef(mods1, name, tparams, templ)
    }
  }

  implicit final class DefinitionsCompat(
      self: Compat210Component.this.global.definitions.type) {
    lazy val StringTpe = definitions.StringClass.tpe
  }
}
