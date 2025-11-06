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

    /** Renaming locals is done to propagate CSE, always starting from a synth. local */
    private val renamedLocals: mutable.Map[LocalID, LocalID] = mutable.Map.empty
    private val cseCTX: mutable.Map[(Instr, Instr), LocalID] = mutable.Map.empty

    private val synthLocals: mutable.ListBuffer[Modules.Local] = mutable.ListBuffer.empty
    private var onScopeSynths: List[(Instr, mutable.Set[LocalID])] = List((Unreachable, mutable.Set[LocalID]()))

    private val cseResult = mutable.ListBuffer.empty[Instr]

    def optimize: Modules.Function = {
      val optimizedBody = Expr(tidy(cse(function.body.instr)))
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
            case LocalGet(i) if renamedLocals.contains(i) && isSynthLocalReachable(renamedLocals(i))  =>
              cse(LocalGet(renamedLocals(i)) :: next :: tail) // Rename the local before any possible CSE
            case LocalGet(origID) if isImmutable(origID) =>
              next match {
                // Look at the 'next' instruction of a LocalGet to either collapse it by renaming or by CSE application
                case LocalTee(id) if isImmutable(id) && isSynthLocalReachable(origID) => // Renaming propagation
                  renamedLocals += (id -> origID)
                  cse(current :: tail)
                case LocalSet(id) if isImmutable(id) && isSynthLocalReachable(origID)=> // Renaming propagation
                  renamedLocals += (id -> origID)
                  cse(tail)
                case RefAsNonNull =>
                  val intendedType = getLocalType(origID).asInstanceOf[RefType].toNonNullable
                  applyCSE(current, origID, intendedType, next, tail)
                case sg: StructGet =>
                  val fieldType = getFieldType(sg.structTypeID, sg.fieldID)
                  applyCSE(current, origID, fieldType, next, tail)
                case _ => // No reduction possible
                  cseResult += current
                  cse(next :: tail)
              }
            // Need to flatten a LocalTee(i) to facilitate cse or renaming
            case LocalTee(i) if isImmutable(i) =>
              cseResult += LocalSet(i)
              cse(LocalGet(i) :: next :: tail)
            case instr =>
              cseResult += instr
              cse(next :: tail)
          }
        case current :: Nil =>
          updateNestingLevel(current)
          cseResult += current
          cse(Nil)
        case Nil =>
          cseResult.result()
      }
    }

    private def updateNestingLevel(instr: Instr): Unit = {
      instr match {
        case br: Br =>
          if (onScopeSynths.head._1.isInstanceOf[Block]) {
            moveOneScopeUp(br)
          }
        case _: StructuredLabeledInstr =>
          onScopeSynths = (instr, mutable.Set[LocalID]()) :: onScopeSynths
        case End =>
          onScopeSynths = onScopeSynths.tail
        case Else =>
          onScopeSynths = (instr, mutable.Set[LocalID]()) :: onScopeSynths.tail
        case _ =>
      }
    }

    private def moveOneScopeUp(from: Instr): Unit = {
      val currentHead = onScopeSynths.head
      onScopeSynths = onScopeSynths.tail
      onScopeSynths.head._2 ++= currentHead._2
      onScopeSynths = (from, mutable.Set[LocalID]()) :: onScopeSynths
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
          onScopeSynths.head._2 += freshID
          synthLocals += freshLocal
          cseCTX.update((current, next), freshID)
          cseResult ++= Seq(current, next, LocalSet(freshID))
          cse(LocalGet(freshID) :: tail)
        }
      }

    private def tidy(instructions: List[Instr]): List[Instr] = {
      val resultBuilder = List.newBuilder[Instr]
      @tailrec
      def tidyRec(instructions: List[Instr]): List[Instr] = {
        instructions match {
          case LocalSet(is) :: LocalGet(ig) :: tail if ig == is =>
            resultBuilder += LocalTee(ig)
            tidyRec(tail)
          case current :: tail =>
            resultBuilder += current
            tidyRec(tail)
          case Nil => resultBuilder.result()
        }
      }
      tidyRec(instructions)
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
      onScopeSynths.exists(_._2(id))

    private def isImmutable(id: LocalID): Boolean =
      candidates(id) || (onScopeSynths.nonEmpty && isSynthLocalReachable(id))
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
