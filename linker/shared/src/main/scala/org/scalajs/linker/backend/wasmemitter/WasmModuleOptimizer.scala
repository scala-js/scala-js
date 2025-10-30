package org.scalajs.linker.backend.wasmemitter

import org.scalajs.ir.OriginalName
import org.scalajs.linker.backend.webassembly.Identitities.{FieldID, LocalID, TypeID}
import org.scalajs.linker.backend.webassembly.{Modules, Types}
import org.scalajs.linker.backend.webassembly.Instructions._
import org.scalajs.linker.backend.webassembly.Types.{HeapType, Int16, Int32, Int8, RecType, RefType, StorageType, StructField, StructType, SubType}

import scala.annotation.tailrec
import scala.collection.mutable

object WasmModuleOptimizer {

  def optimize(wasmModule: Modules.Module): Modules.Module = {
    val optimizedFuncs = wasmModule.funcs.map(WasmFunctionOptimizer(_,wasmModule).optimize)
    //val optimizedFuncs = wasmModule.funcs
    /*val optimizedFuncs = wasmModule.funcs.zipWithIndex.map(tuple => {
      if (tuple._2 == 111){
        WasmFunctionOptimizer(tuple._1, wasmModule).optimize
      } else {
        tuple._1
      }
    })*/
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

    /** Renaming locals is done to propagate CSE, starting from a synth. local */
    private val renamedLocals: mutable.Map[LocalID, LocalID] = mutable.Map.empty
    private val cseCTX: mutable.Map[(Instr, Instr), LocalID] = mutable.Map.empty

    private val synthLocals: mutable.ListBuffer[Modules.Local] = mutable.ListBuffer.empty
    private var onScopeIDs: List[(Instr, mutable.Set[LocalID])] = List((Unreachable, mutable.Set[LocalID]()))

    private val result = List.newBuilder[Instr]

    def optimize: Modules.Function = {
      val optimizedBody = Expr(tidy(cse(function.body.instr), Nil))
      val updatedLocals = function.locals ++ synthLocals.toList
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

    @tailrec
    private def cse(instructions: List[Instr]): List[Instr] = {
      instructions match {
        case current :: next :: tail =>
          updateNestingLevel(current)
          current match {
            case LocalGet(i) if renamedLocals.contains(i) && isSynthLocalReachable(renamedLocals(i)) =>
              cse(LocalGet(renamedLocals(i)) :: next :: tail)
            case LocalGet(origID) if isImmutable(origID) =>
              val i = renamedLocals.getOrElse(origID, origID)
              next match {
                case LocalTee(id) if isImmutable(id) && isSynthLocalReachable(i) => // Renaming propagation
                  renamedLocals += (id -> i)
                  cse(current :: tail)
                case LocalSet(id) if isImmutable(id) && isSynthLocalReachable(i) => // Renaming propagation
                  renamedLocals += (id -> i)
                  cse(tail)
                case RefAsNonNull =>
                  val intendedType = getLocalType(i).asInstanceOf[RefType].toNonNullable
                  applyCSE(current, i, intendedType, next, tail)
                case sg: StructGet =>
                  val fieldType = getFieldType(sg.structTypeID, sg.fieldID)
                  applyCSE(current, i, fieldType, next, tail)
                case _ => // No reduction possible
                  result += current
                  cse(next :: tail)
              }
            // Need to flatten a LocalTee(i) to facilitate cse or renaming
            case LocalTee(i) if isImmutable(i) =>
              result += LocalSet(i)
              cse(LocalGet(i) :: next :: tail)
            case instr =>
              result += instr
              cse(next :: tail)
          }
        case current :: Nil =>
          updateNestingLevel(current)
          result += current
          result.result()
        case Nil => result.result()
      }
    }

    private def updateNestingLevel(instr: Instr): Unit = {
      instr match {
        case _: StructuredLabeledInstr =>
          onScopeIDs = (instr, mutable.Set[LocalID]()) :: onScopeIDs
        case End =>
          onScopeIDs = onScopeIDs.tail
        case Else =>
          onScopeIDs = (instr, mutable.Set[LocalID]()) :: onScopeIDs.tail
        case _ =>
      }
    }

    private def applyCSE(current: Instr, i: LocalID, tp: Types.Type, next: Instr, tail: List[Instr]): List[Instr] =
      if (renamedLocals.contains(i) && isSynthLocalReachable(renamedLocals(i))) {
        cse(LocalGet(renamedLocals(i)) :: next :: tail)
      } else {
        // Pair of instructions already seen, so apply cse
        if (cseCTX.contains((current, next)) && isSynthLocalReachable(cseCTX((current, next)))) {
          cse(LocalGet(cseCTX(current, next)) :: tail)
        } else { // First time seen, so register it on the map and link it to a freshLocal
          val freshLocal = newLocal(tp)
          val freshID = freshLocal.id
          onScopeIDs.head._2 += (freshID)
          synthLocals += freshLocal
          cseCTX.update((current, next), freshID)
          result ++= Seq(current, next, LocalSet(freshID))
          cse(LocalGet(freshID) :: tail)
        }
      }

    @tailrec
    private def tidy(instructions: List[Instr], result: List[Instr]): List[Instr] = {
      instructions match {
        case LocalSet(is) :: LocalGet(ig) :: tail if ig == is =>
          tidy(tail, LocalTee(ig) :: result)
        case current :: tail =>
          tidy(tail, current :: result)
        case Nil => result.reverse
      }
    }

    private def getLocalType(id: LocalID): Types.Type = {
      function.locals.collectFirst {
        case el if el.id == id => el.tpe
      }.getOrElse(synthLocals.filter(_.id == id).head.tpe)
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

    private def isSynthLocalReachable(id: LocalID): Boolean =
      onScopeIDs.exists(_._2(id))

    private def isImmutable(id: LocalID): Boolean =
      candidates(id) || (onScopeIDs.nonEmpty && onScopeIDs.head._2(id))
      // Either a defined candidate or a LocalID made by CSE

    private def newLocal(tp: Types.Type): Modules.Local = {
      val noOrigName = OriginalName.NoOriginalName
      val freshID = new SynthLocalIDImpl(function.locals.size + synthLocals.size,
        OriginalName.NoOriginalName)
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
