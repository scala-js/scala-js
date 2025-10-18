package org.scalajs.linker.backend.wasmemitter

import org.scalajs.ir.OriginalName
import org.scalajs.linker.backend.webassembly.Identitities.{FieldID, LocalID, TypeID}
import org.scalajs.linker.backend.webassembly.{Modules, Types}
import org.scalajs.linker.backend.webassembly.Instructions._
import org.scalajs.linker.backend.webassembly.Types.{HeapType, Int16, Int32, Int8, RecType, RefType,
  StorageType, StructField, StructType, SubType}

import scala.collection.mutable

object WasmModuleOptimizer {

  def optimize(wasmModule: Modules.Module): Modules.Module = {
    val optimizedFuncs = wasmModule.funcs.map(WasmFunctionOptimizer(_,wasmModule).optimize)
    new Modules.Module(
      wasmModule.types,
      wasmModule.imports,
      optimizedFuncs,
      wasmModule.tags,
      wasmModule.globals,
      wasmModule.exports,
      wasmModule.start,
      wasmModule.elems,
      wasmModule.datas)
  }

  private case class WasmFunctionOptimizer(function: Modules.Function, module: Modules.Module) {
    private val candidates: Set[LocalID] = collectCandidates(function.body.instr)
    private val renamedLocals: mutable.Map[LocalID, LocalID] = mutable.Map.empty
    private val cseCTX: mutable.Map[(Instr, Instr), LocalID] = mutable.Map.empty

    private val freshLocals: mutable.ListBuffer[Modules.Local] = mutable.ListBuffer.empty
    private val freshLocalIDs: mutable.Set[LocalID] = mutable.Set.empty

    def optimize: Modules.Function = {
      val optimizedBody = Expr(tidy(cse(function.body.instr)))
      val updatedLocals = function.locals ++ freshLocals.toList
      Modules.Function(
        function.id,
        function.originalName,
        function.typeID,
        function.params,
        function.results,
        updatedLocals,
        optimizedBody,
        function.pos,
      )
    }

    private def cse(instructions: List[Instr]): List[Instr] = {
      instructions match {
        case current :: next :: tail =>
          current match {
            case LocalGet(i) if renamedLocals.contains(i) =>
              cse(LocalGet(renamedLocals(i)) :: next :: tail)
            case LocalGet(origID) if isImmutable(origID) =>
              val i = renamedLocals.getOrElse(origID, origID)
              next match {
                case LocalTee(id) if isImmutable(id) =>
                  renamedLocals += (id -> i)
                  cse(current :: tail)
                case LocalSet(id) if isImmutable(id) =>
                  renamedLocals += (id -> i)
                  cse(current :: Drop :: tail)
                case RefAsNonNull =>
                  val intendedType = getLocalType(i).asInstanceOf[RefType].toNonNullable
                  applyCSE(current, i, intendedType, next, tail)
                case sg: StructGet =>
                  val fieldType = getFieldType(sg.structTypeID, sg.fieldID)
                  applyCSE(current, i, fieldType, next, tail)
                case _ => // No reduction possible
                  current :: cse(next :: tail)
              }
            // Need to flatten a LocalTee(i) to facilitate cse or renaming
            case LocalTee(i) if isImmutable(i) =>
              LocalSet(i) :: cse(LocalGet(i) :: next :: tail)
            case instr =>
              instr :: cse(next :: tail)
          }
        case current :: Nil => List(current)
        case Nil => Nil
      }
    }

    private def applyCSE(current: Instr, i: LocalID, tp: Types.Type, next: Instr, tail: List[Instr]): List[Instr] =
      if (renamedLocals.contains(i)) {
        cse(LocalGet(renamedLocals(i)) :: next :: tail)
      } else {
        if (cseCTX.contains((current, next))) { // Pair of instructions already seen, so apply cse
          cse(LocalGet(cseCTX(current, next)) :: tail)
        } else { // First time seen, so register it on the map and link it to a freshLocal
          val freshLocal = newLocal(tp)
          val freshID = freshLocal.id
          freshLocalIDs += freshID
          freshLocals += freshLocal
          cseCTX.update((current, next), freshID)
          current :: next :: LocalSet(freshID) :: cse(LocalGet(freshID) :: tail)
        }
      }

    private def tidy(instructions: List[Instr]): List[Instr] = {
      instructions match {
        case LocalSet(is) :: LocalGet(ig) :: tail if ig == is =>
          LocalTee(ig) :: tidy(tail)
        case current :: tail =>
          current :: tidy(tail)
        case Nil => Nil
      }
    }

    private def getLocalType(id: LocalID): Types.Type = {
      function.locals.collectFirst {
        case el if el.id == id => el.tpe
      }.getOrElse(freshLocals.filter(_.id == id).head.tpe)
    }

    private def getFieldType(structTID: TypeID, fieldTID: FieldID): Types.Type = {
      val types = module.types
      val structFields: Option[List[StructField]] =
        types
          .iterator
          .flatMap(_.subTypes.iterator)
          .find(_.id==structTID)
          .collect { case SubType(_, _, _, _, StructType(fields)) => fields }
       val res = structFields.flatMap(_.find(_.id == fieldTID)).map(_.fieldType)
      storageTypeToType(res.head.tpe)
    }

    private def storageTypeToType(tpe: StorageType): Types.Type = {
      tpe match {
        case t: Types.Type => t
        case Int8 | Int16 => Int32
      }
    }

    private def isImmutable(id: LocalID): Boolean =
      candidates(id) || freshLocalIDs(id)

    private def newLocal(tp: Types.Type): Modules.Local = {
      val noOrigName = OriginalName.NoOriginalName
      val freshID = new SynthLocalIDImpl(function.locals.size + freshLocalIDs.size, noOrigName)
      Modules.Local(freshID, noOrigName, tp)
    }

    private def collectCandidates(instructions: List[Instr]): Set[LocalID] = {
      val params = function.params.map(_.id).toSet
      instructions.foldLeft[Map[LocalID, Int]](Map.empty)((census, current) => {
        current match {
          case LocalSet(i) =>
            census + (i -> (1 + census.getOrElse(i, 0)))
          case LocalTee(i) =>
            census + (i -> (1 + census.getOrElse(i, 0)))
          case _ =>
            census
        }
      }).filter(_._2 == 1).keySet.diff(params)
    }
  }

  private final class SynthLocalIDImpl(index: Int, originalName: OriginalName) extends LocalID {
    override def toString(): String =
      if (originalName.isDefined) originalName.get.toString()
      else s"<local $index>"
  }
}
