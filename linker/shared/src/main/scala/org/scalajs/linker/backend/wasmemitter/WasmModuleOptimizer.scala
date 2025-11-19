package org.scalajs.linker.backend.wasmemitter

import org.scalajs.ir.OriginalName
import org.scalajs.linker.backend.webassembly.Identitities.{FieldID, LocalID, TypeID}
import org.scalajs.linker.backend.webassembly.{Modules, Types}
import org.scalajs.linker.backend.webassembly.Instructions._
import org.scalajs.linker.backend.webassembly.Types._

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

    def optimize: Modules.Function = {
      val cse = CSEOptimization(function, module.types)
      val optimizedBody = Expr(tidy(cse.apply()))
      val updatedLocals = function.locals ++ cse.getSynthLocals
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
  }

  private case class CSEOptimization(function: Modules.Function, types: List[RecType]) {
    private val candidates: Set[LocalID] = collectCandidates(function.body.instr)

    /** Renaming locals is done to propagate CSE, always starting from a synth. local */
    private val renamedLocals: mutable.Map[LocalID, LocalID] = mutable.Map.empty
    private val cseCTX: mutable.Map[(Instr, Instr), LocalID] = mutable.Map.empty

    private val synthLocals: mutable.ListBuffer[Modules.Local] = mutable.ListBuffer.empty
    private var controlStack: List[ControlFrame] = List(ControlFrame(Unreachable, 0))

    private val synthsOnScope: mutable.Set[LocalID] = mutable.Set.empty
    private var initSynthsStack: List[LocalID] = List.empty

    private val cseResult = mutable.ListBuffer.empty[Instr]

    def getSynthLocals: List[Modules.Local] = synthLocals.toList
    def apply(): List[Instr] = cse(function.body.instr)

    @tailrec
    private def cse(instructions: List[Instr]): List[Instr] = {
      instructions match {
        case current :: next :: tail =>
          updateNestingLevel(current)
          current match {
            case LocalGet(i) if renamedLocals.contains(i) && isSynthLocalReachable(renamedLocals(i)) =>
              cse(LocalGet(renamedLocals(i)) :: next :: tail) // Rename the local before any possible CSE
            case LocalGet(origID) if isImmutable(origID) =>
              next match {
                // Look at the 'next' instruction of a LocalGet to either collapse it by renaming or by CSE application
                case LocalTee(id) if isImmutable(id) && isSynthLocalReachable(origID) => // Renaming propagation
                  renamedLocals += (id -> origID)
                  cse(current :: tail)
                case LocalSet(id) if isImmutable(id) && isSynthLocalReachable(origID) => // Renaming propagation
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
        case _: StructuredLabeledInstr =>
          controlStack = ControlFrame(instr, synthsOnScope.size) :: controlStack
        case End =>
          resetScopeSynths(controlStack.head.startSynthsHeight)
          controlStack = controlStack.tail
        case Else =>
          val startSynthHeight = controlStack.head.startSynthsHeight
          resetScopeSynths(startSynthHeight)
          controlStack = ControlFrame(instr, startSynthHeight) :: controlStack.tail
        case _ =>
      }
    }

    private def resetScopeSynths(nextSynthHeight: Int): Unit = {
      while (synthsOnScope.size > nextSynthHeight) {
        synthsOnScope.remove(initSynthsStack.head)
        initSynthsStack = initSynthsStack.tail
      }
    }

    private def registerSynthLocal(id: LocalID): Unit = {
      synthsOnScope += id
      initSynthsStack = id :: initSynthsStack
    }

    private def applyCSE(current: Instr, i: LocalID, tp: Types.Type, next: Instr, tail: List[Instr]): List[Instr] = {
      if (renamedLocals.contains(i) && isSynthLocalReachable(renamedLocals(i))) {
        cse(LocalGet(renamedLocals(i)) :: next :: tail)
      } else {
        // Pair of instructions already seen, so apply cse
        if (cseCTX.contains((current, next)) && isSynthLocalReachable(cseCTX((current, next)))) {
          cse(LocalGet(cseCTX(current, next)) :: tail)
        } else { // First time seen, so register it on the map and link it to a freshLocal
          val freshLocal = newLocal(tp)
          synthLocals += freshLocal
          val freshID = freshLocal.id
          registerSynthLocal(freshID)
          cseCTX.update((current, next), freshID)
          cseResult ++= Seq(current, next, LocalSet(freshID))
          cse(LocalGet(freshID) :: tail)
        }
      }
    }

    private def getLocalType(id: LocalID): Types.Type = {
      val localsAndParams = function.locals ++ function.params
      localsAndParams.collectFirst {
        case el if el.id == id => el.tpe
      }.getOrElse(synthLocals.filter(_.id == id).head.tpe)
    }

    private def getFieldType(structTID: TypeID, fieldTID: FieldID): Types.Type = {
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
      synthsOnScope(id)

    private def isImmutable(id: LocalID): Boolean =
      candidates(id) || (controlStack.nonEmpty && isSynthLocalReachable(id))
    // Either a defined candidate or a LocalID made by CSE

    private def newLocal(tp: Types.Type): Modules.Local = {
      val noOrigName = OriginalName.NoOriginalName
      val freshID = new SynthLocalIDImpl(function.locals.size + synthLocals.size,
        OriginalName.NoOriginalName)
      Modules.Local(freshID, noOrigName, tp)
    }

    private def collectCandidates(instructions: List[Instr]): Set[LocalID] = {
      val finalSet: mutable.Set[LocalID] = mutable.Set.empty
      val prohibitedSet: mutable.Set[LocalID] = mutable.Set.empty
      var localStack: List[LocalID] = List.empty
      var controlStack: List[ControlFrame] = List(ControlFrame(Unreachable, 0))
      val params = function.params.map(_.id).toSet

      val currentScopeSetCount: mutable.Map[LocalID, Int] = mutable.Map.empty
      val currentScopeGetCount: mutable.Map[LocalID, Int] = mutable.Map.empty

      def alreadySeen(id: LocalID): Boolean = {
        currentScopeGetCount.contains(id) || currentScopeSetCount.contains(id)
      }
      def pushToLocals(id: LocalID): Unit = {
        if (!alreadySeen(id)) {
          localStack = id :: localStack
        }
      }
      def resetScope(): Unit = {
        val nextSynthHeight = controlStack.head.startSynthsHeight
        while (localStack.size > nextSynthHeight) {
          val candidate = localStack.head
          val setCount = currentScopeSetCount.getOrElse(candidate, 0)
          if(prohibitedSet(candidate)) {
            finalSet -= candidate
          } else {
            if (params(candidate) && setCount == 0) {
              finalSet += candidate
            }
            if (!params(candidate) && setCount == 1) {
              finalSet += candidate
            }
            prohibitedSet += candidate
          }
          currentScopeSetCount.remove(candidate)
          currentScopeGetCount.remove(candidate)
          localStack = localStack.tail
        }
      }

      val it = instructions.iterator
      while (it.hasNext) {
        val instr = it.next()
        instr match {
          case LocalSet(i) =>
            pushToLocals(i)
            currentScopeSetCount.update(i, 1 + currentScopeSetCount.getOrElse(i, 0))
          case LocalTee(i) =>
            pushToLocals(i)
            currentScopeSetCount.update(i, 1 + currentScopeSetCount.getOrElse(i, 0))
            currentScopeGetCount.update(i, 1 + currentScopeGetCount.getOrElse(i, 0))
          case LocalGet(i) =>
            pushToLocals(i)
            currentScopeGetCount.update(i, 1 + currentScopeGetCount.getOrElse(i, 0))
          case _: StructuredLabeledInstr =>
            controlStack = ControlFrame(instr, localStack.size) :: controlStack
          case End =>
            resetScope()
            controlStack = controlStack.tail
          case Else =>
            resetScope()
            val startSynthHeight = controlStack.head.startSynthsHeight
            controlStack = ControlFrame(instr, startSynthHeight) :: controlStack.tail
          case _ =>
        }
      }
      resetScope()
      finalSet.toSet
    }
  }

  private case class ControlFrame(kind: Instr, var startSynthsHeight: Int, var hasIndirection: Boolean = false)

  private final class SynthLocalIDImpl(index: Int, originalName: OriginalName) extends LocalID {
    override def toString(): String =
      if (originalName.isDefined) originalName.get.toString()
      else s"<local $index>"
  }
}
