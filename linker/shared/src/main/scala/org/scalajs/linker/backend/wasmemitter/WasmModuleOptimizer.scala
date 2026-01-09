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

package org.scalajs.linker.backend.wasmemitter

import org.scalajs.ir.OriginalName
import org.scalajs.linker.backend.webassembly.Identitities.{FieldID, FunctionID, LabelID, LocalID, TypeID}
import org.scalajs.linker.backend.webassembly.Instructions.BlockType.ValueType
import org.scalajs.linker.backend.webassembly.{Modules, Types}
import org.scalajs.linker.backend.webassembly.Instructions._
import org.scalajs.linker.backend.webassembly.Types._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class WasmModuleOptimizer(private val wasmModule: Modules.Module) {

  private val structFieldIdxFieldType: Map[(TypeID, FieldID), FieldType] = {
    wasmModule.types
      .flatMap(_.subTypes)
      .collect {
        case SubType(id, _, _, _, StructType(fields)) =>
          fields.map(sf => (id, sf.id) -> sf.fieldType)
      }
      .flatten
      .toMap
  }

  def optimize(): Modules.Module = {
    new Modules.Module(
      wasmModule.types,
      wasmModule.imports,
      wasmModule.funcs.map(WasmFunctionOptimizer(_).apply),
      wasmModule.tags,
      wasmModule.globals,
      wasmModule.exports,
      wasmModule.start,
      wasmModule.elems,
      wasmModule.datas)
  }

  private case class WasmFunctionOptimizer(private val function: Modules.Function) {
    private var localsCount = function.locals.size
    private val localIdXLocalType: mutable.Map[LocalID, Type] = {
      val m = mutable.Map.empty[LocalID, Type]
      function.params.foreach(p => m.put(p.id, p.tpe))
      function.locals.foreach(l => m.put(l.id, l.tpe))
      m
    }

    def apply: Modules.Function = {
      val cse = CSEOptimization(function.body.instr)
      val optimizedByCSE = cse.optimizeBody()
      val cseSynths = cse.getSynthsLocals
      localIdXLocalType ++= cseSynths.map(local => local.id -> local.tpe).toMap
      localsCount += cseSynths.size

      val licm = LICMOptimization(optimizedByCSE)
      val optimizedBody = licm.optimizeBody()
      val licmSynths = licm.getSynthsLocals

      Modules.Function(
        function.id,
        function.originalName,
        function.typeID,
        function.params,
        function.results,
        cseSynths ++ licmSynths ++ function.locals,
        Expr(optimizedBody),
        function.pos
      )
    }

    private trait WasmFunctionOptimization {
      protected val synthLocals: mutable.Map[LocalID, Modules.Local] = mutable.Map.empty
      def optimizeBody(): List[Instr]

      def getSynthsLocals: List[Modules.Local] = {
        synthLocals.values.toList
      }

      protected def getLocalType(id: LocalID): Types.Type = {
        localIdXLocalType.getOrElse(id, synthLocals(id).tpe)
      }

      protected def makeSyntheticLocal(tp: Type): LocalID = {
        val noOrigName = OriginalName.NoOriginalName
        val freshID = new SynthLocalIDImpl(localsCount + synthLocals.size,
          OriginalName.NoOriginalName)
        synthLocals += freshID -> Modules.Local(freshID, noOrigName, tp)
        freshID
      }

      private final class SynthLocalIDImpl(index: Int, originalName: OriginalName) extends LocalID {
        override def toString(): String =
          if (originalName.isDefined) originalName.get.toString()
          else s"<local s$index>"
      }

      protected def storageTypeToType(tpe: StorageType): Types.Type = {
        tpe match {
          case t: Types.Type => t
          case Int8 | Int16 => Int32
        }
      }
    }

    private case class LICMOptimization(private val instructions: List[Instr]) extends WasmFunctionOptimization {
      private val res: ListBuffer[Instr] = ListBuffer.empty
      override def optimizeBody(): List[Instr] = {
        loopInvariantCodeMotion(instructions)
        val optimized = res.result()
        res.clear()
        optimized
      }

      @tailrec
      private def loopInvariantCodeMotion(instructions: List[Instr]): Unit = {
        instructions match {
          case current :: nextInstructions =>
            current match {
              case loop: Loop if isLoopEmptyToEmpty(loop) =>
                val ((hoisted, optimizedBody), remaining) = applyLICM(nextInstructions)
                res ++= hoisted
                res ++= loop :: optimizedBody
                loopInvariantCodeMotion(remaining)
              case _ =>
                res += current
                loopInvariantCodeMotion(nextInstructions)
            }
          case _ =>
        }
      }

      private def applyLICM(instructions: List[Instr]): ((List[Instr], List[Instr]), List[Instr]) = {
        val toExtract: ListBuffer[Instr] = ListBuffer.empty // Without position markers
        val posMarkers: ListBuffer[Instr] = ListBuffer.empty
        val extracted: ListBuffer[Instr] = ListBuffer.empty
        val updatedBody: ListBuffer[Instr] = ListBuffer.empty
        var level = 1
        val (setInLoop, setMoreThanOnce) = collectLocalsSetWithinLoop(instructions)
        val typeStack = TypeStack(setInLoop, setMoreThanOnce)

        def tryExtraction(instruction: Instr): Unit = {
          val getOrConstSuffixLength = typeStack.getOrConstLength()
          if (!typeStack.updateStack(instruction)) {
            val toExtractSize = toExtract.size
            if (toExtractSize >= 2  && toExtractSize > getOrConstSuffixLength) {
              val stackedTypes: Option[List[Type]] = typeStack.popAllStack().map(_.drop(getOrConstSuffixLength))
              val getSynths: ListBuffer[LocalGet] = ListBuffer.empty
              val finalLengthToExtract = toExtractSize - getOrConstSuffixLength
              val (toExtractFinal, getOrConstSuffix) = toExtract.splitAt(finalLengthToExtract)
              extracted ++= toExtractFinal
              extracted ++= posMarkers
              posMarkers.clear()
              toExtract.clear()
              stackedTypes.foreach(types => {
                types.foreach(tp => {
                  val freshId = makeSyntheticLocal(tp)
                  extracted += LocalSet(freshId)
                  getSynths += LocalGet(freshId)
                })
              })
              updatedBody ++= getSynths.reverse
              updatedBody ++= getOrConstSuffix
              updatedBody += instruction
            } else {
              typeStack.popAllStack()
              updatedBody ++= toExtract
              updatedBody ++= posMarkers
              posMarkers.clear()
              toExtract.clear()
              updatedBody += instruction
            }
          } else {
            toExtract += instruction
          }
        }

        @tailrec
        def extractInvariantCode(loopBody: List[Instr]): List[Instr] = {
          loopBody match {
            case current :: remaining =>
              current match {
                case loop: Loop =>
                  val ((hoisted: List[Instr], updatedLoop: List[Instr]), afterLoop: List[Instr]) = applyLICM(remaining)
                  hoisted.foreach(tryExtraction)
                  tryExtraction(loop) // Will flush the `toExtract` buffer
                  updatedBody ++= updatedLoop
                  extractInvariantCode(afterLoop)
                case cond: If =>
                  level += 1
                  tryExtraction(cond)
                  extractInvariantCode(remaining)
                case sl: StructuredLabeledInstr =>
                  level += 1 // Does it have to be done on the Loop also
                  tryExtraction(sl)
                  extractInvariantCode(remaining)
                case End =>
                  level -= 1
                  if (level <= 0) {
                    tryExtraction(End)
                    remaining // Stop the recursion and returns the remaining instructions
                  } else {
                    tryExtraction(End)
                    extractInvariantCode(remaining)
                  }
                case pm: PositionMark =>
                  posMarkers += pm
                  extractInvariantCode(remaining)
                case instr =>
                  tryExtraction(instr)
                  extractInvariantCode(remaining)
              }
            case Nil => Nil
          }
        }
        val remaining = extractInvariantCode(instructions)
        ((extracted.result(), updatedBody.result()), remaining)
      }

      private def isLoopEmptyToEmpty(loop: Loop): Boolean = {
        loop.i == ValueType(None)
      }

      private def collectLocalsSetWithinLoop(loopBody: List[Instr]): (Set[LocalID], Set[LocalID]) = {
        val it = loopBody.iterator
        val setWithinLoop: mutable.Set[LocalID] = mutable.Set.empty
        val setTwiceWithinLoop: mutable.Set[LocalID] = mutable.Set.empty
        var level = 0
        while(it.hasNext && level >= 0) {
          val current = it.next()
          current match {
            case LocalSet(id) =>
              if (setWithinLoop(id)) {
                setTwiceWithinLoop += id
              }
              setWithinLoop += id
            case LocalTee(id) =>
              if (setWithinLoop(id)) {
                setTwiceWithinLoop += id
              }
              setWithinLoop += id
            case _:StructuredLabeledInstr =>
              level += 1
            case End =>
              level -= 1
            case _ =>
          }
        }
        (setWithinLoop.toSet, setTwiceWithinLoop.toSet)
      }

      private case class TypeStack(private val setWithinLoop: Set[LocalID],
                                   private val setMoreThanOnce: Set[LocalID]) {
        private var typeStack: List[Type] = List.empty // (Type, and whether it will be an invariant)
        private var typeStackHeight: Int = 0
        private var isStackPure: Boolean = true
        private var isLoopPrefix: Boolean = true
        private var getOrConstSuffixLength = 0

        // To track locals set once within the loop. These should be added to that set before any LocalGet on them
        private val localSetFromPureArg: mutable.Set[LocalID] = mutable.Set.empty

        private def pop(): Option[Type] = {
          if (typeStack.isEmpty) {
            // If we pop one element on an empty TypeStack,
            // it means that the element has been pushed before by some unpure instruction
            isStackPure = false
            None
          } else {
            val head = typeStack.head
            typeStack = typeStack.tail
            typeStackHeight -= 1
            Some(head)
          }
        }

        private def push(tp: Type): Unit = {
          if (isStackPure) {
            typeStack = tp :: typeStack
            typeStackHeight += 1
          }
        }

        def popAllStack(): Option[List[Type]] = {
          if (typeStack.isEmpty) {
            None
          } else {
            val res = typeStack
            typeStack = List.empty
            typeStackHeight = 0
            Some(res)
          }
        }

        private def unpureInstrIsMet(): Unit = {
          isStackPure = false
          isLoopPrefix = false
        }

        private val i32UnaryOp = PureOperation(Seq(Int32), Seq(Int32))
        private val i64toI32UnaryOp = PureOperation(Seq(Int64), Seq(Int32))
        private val i64toI64UnaryOp = PureOperation(Seq(Int64), Seq(Int64))
        private val i32BinOp = PureOperation(Seq(Int32, Int32), Seq(Int32))
        private val i64toi32BinOp = PureOperation(Seq(Int64, Int64), Seq(Int32))
        private val f32BinOp = PureOperation(Seq(Float32, Float32), Seq(Int32))
        private val f64BinOp = PureOperation(Seq(Float64, Float64), Seq(Int32))
        private val i64toI64BinOp = PureOperation(Seq(Int64, Int64), Seq(Int64))

        private val instrToPureOperation: Map[Instr, PureOperation] = Map(
          I32Eq -> i32BinOp,
          I32Ne -> i32BinOp,
          I32LtS -> i32BinOp,
          I32LtU -> i32BinOp,
          I32LeS -> i32BinOp,
          I32LeU -> i32BinOp,
          I32GtS -> i32BinOp,
          I32GtU -> i32BinOp,
          I32GeS -> i32BinOp,
          I32GeU -> i32BinOp,
          I32Add -> i32BinOp,
          I32Sub -> i32BinOp,
          I32Mul -> i32BinOp,
          I32DivS -> i32BinOp,
          I32DivU -> i32BinOp,
          I32RemS -> i32BinOp,
          I32RemU -> i32BinOp,
          I32And -> i32BinOp,
          I32Or -> i32BinOp,
          I32Xor -> i32BinOp,
          I32Shl -> i32BinOp,
          I32ShrS -> i32BinOp,
          I32ShrU -> i32BinOp,
          I32Rotl -> i32BinOp,
          I32Rotr -> i32BinOp,
          I64Eq -> i64toi32BinOp, I64Ne -> i64toi32BinOp,
          I64LtS -> i64toi32BinOp, I64LtU -> i64toi32BinOp, I64LeS -> i64toi32BinOp, I64LeU -> i64toi32BinOp,
          I64GtS -> i64toi32BinOp, I64GtU -> i64toi32BinOp, I64GeS -> i64toi32BinOp, I64GeU -> i64toi32BinOp,
          F32Eq -> f32BinOp, F32Ne -> f32BinOp, F32Lt -> f32BinOp,
          F32Gt -> f32BinOp, F32Le -> f32BinOp, F32Ge -> f32BinOp,
          F64Eq -> f64BinOp, F64Ne -> f64BinOp, F64Lt -> f64BinOp,
          F64Gt -> f64BinOp, F64Le -> f64BinOp, F64Ge -> f64BinOp,
          I64Add -> i64toI64BinOp, I64Sub -> i64toI64BinOp, I64Mul -> i64toI64BinOp,
          I64DivS -> i64toI64BinOp, I64DivU -> i64toI64BinOp, I64RemS -> i64toI64BinOp, I64RemU -> i64toI64BinOp,
          I64And -> i64toI64BinOp, I64Or -> i64toI64BinOp, I64Xor -> i64toI64BinOp, I64Shl -> i64toI64BinOp,
          I64ShrS -> i64toI64BinOp, I64ShrU -> i64toI64BinOp, I64Rotl -> i64toI64BinOp, I64Rotr -> i64toI64BinOp,
          I32Eqz -> i32UnaryOp,
          I32Clz -> i32UnaryOp,
          I32Ctz -> i32UnaryOp,
          I32Popcnt -> i32UnaryOp,
          I64Eqz -> i64toI32UnaryOp,
          I64Clz -> i64toI64UnaryOp,
          I64Ctz -> i64toI64UnaryOp,
          I64Popcnt -> i64toI64UnaryOp
        )

        private def isNonNullable(tp: Type) =
          tp match {
            case rt: RefType => !rt.nullable
            case _ => false
          }

        def getOrConstLength(): Int = {
          getOrConstSuffixLength
        }

        private def getOrConstNoLongerASuffix(): Unit = {
          getOrConstSuffixLength = 0
        }

        /** Updates the stack according to a new instruction
         * Returns whether the instruction is hoistable, according to our LICM policy */
        def updateStack(instr: Instr): Boolean = {
          isStackPure = true
          instr match {
            case If(_, _) =>
              getOrConstNoLongerASuffix()
              unpureInstrIsMet()
            case I32Const(v) =>
              getOrConstSuffixLength += 1
              push(Int32)
            case I64Const(v) =>
              getOrConstSuffixLength += 1
              push(Int64)
            case F32Const(v) =>
              getOrConstSuffixLength += 1
              push(Float32)
            case F64Const(v) =>
              getOrConstSuffixLength += 1
              push(Float64)
            case op if instrToPureOperation.contains(op) =>
              getOrConstNoLongerASuffix()
              val pureInstr = instrToPureOperation(op)
              if (typeStackHeight >= pureInstr.in.size) {
                pureInstr.in.foreach(_ => pop())
                pureInstr.out.foreach(push)
              } else {
                unpureInstrIsMet()
              }
            case LocalGet(id) =>
              // Either not set within the loop or set exactly once and before that LocalGet
              // in the loop prefix from an idempotent argument
              isStackPure = (!setWithinLoop(id) || localSetFromPureArg(id))
              val localType = getLocalType(id)
              getOrConstSuffixLength += 1
              push(localType)
            case LocalSet(id) =>
              getOrConstNoLongerASuffix()
              if (isLoopPrefix && !setMoreThanOnce(id) && typeStack.nonEmpty) {
                pop()
                localSetFromPureArg += id
              } else {
                localSetFromPureArg -= id
                unpureInstrIsMet()
              }
            case LocalTee(id) =>
              getOrConstNoLongerASuffix()
              if (isLoopPrefix && !setMoreThanOnce(id) && typeStack.nonEmpty) {
                pop()
                localSetFromPureArg += id
                val localType = getLocalType(id)
                push(localType)
              } else {
                localSetFromPureArg -= id
                unpureInstrIsMet()
              }
            case StructGet(tyidx, fidx) =>
              getOrConstNoLongerASuffix()
              if (typeStack.nonEmpty && (isLoopPrefix || isNonNullable(typeStack.head))) {
                val fieldType = structFieldIdxFieldType((tyidx, fidx))
                if (fieldType.isMutable) {
                  unpureInstrIsMet()
                } else {
                  pop()
                  push(storageTypeToType(fieldType.tpe))
                }
              } else {
                unpureInstrIsMet()
              }
            case RefAsNonNull =>
              getOrConstNoLongerASuffix()
              if (typeStack.nonEmpty && isLoopPrefix) {
                val stacked = pop()
                stacked.foreach(t => push(t.asInstanceOf[RefType].toNonNullable))
              } else {
                unpureInstrIsMet()
              }
            case RefCast(refType) =>
              getOrConstNoLongerASuffix()
              if (typeStack.nonEmpty && isLoopPrefix) {
                pop()
                push(refType)
              } else {
                unpureInstrIsMet()
              }
            case _ =>
              getOrConstNoLongerASuffix()
              unpureInstrIsMet()
          }
          isStackPure // If the stack is still "pure" after consuming that instruction,
                      // then the consumed instruction is a candidate for hoisting
        }
      }

      private case class PureOperation(in: Seq[Type], out: Seq[Type])
    }


    private case class CSEOptimization(private val instructions: List[Instr]) extends WasmFunctionOptimization {
      private val candidates: Set[LocalID] = collectCandidates(function.body.instr)

      /** Renaming locals is done to propagate CSE, always starting from a synthetic local */
      private val renamedLocals: mutable.Map[LocalID, LocalID] = mutable.Map.empty
      private val cseCTX: mutable.Map[(Instr, Instr), LocalID] = mutable.Map.empty
      private var controlStack: List[ControlFrame] = List(ControlFrame(Unreachable, 0))
      private val synthsOnScope: mutable.Set[LocalID] = mutable.Set.empty
      private var initSynthsStack: List[LocalID] = List.empty

      private val cseCTXLog: mutable.Map[LocalID, (Instr, Instr)] = mutable.Map.empty
      private val renamedLog: mutable.Map[LocalID, List[LocalID]] = mutable.Map.empty

      private val cseResult = mutable.ListBuffer.empty[Instr]

      override def optimizeBody(): List[Instr] = {
        cse(instructions)
        val optimized = tidy(cseResult.result())
        cseResult.clear()
        optimized
      }

      @tailrec
      private def cse(instructions: List[Instr]): Unit = {
        instructions match {
          case current :: next :: tail =>
            updateNestingLevel(current)
            current match {
              case LocalGet(i) if renamedLocals.contains(i) =>
                cse(LocalGet(renamedLocals(i)) :: next :: tail) // Rename the local before any possible CSE
              case LocalGet(origID) if isImmutable(origID) =>
                next match {
                  // Look at the 'next' instruction of a LocalGet to either collapse it by renaming or by CSE
                  case LocalTee(id) if isImmutable(id) && isSynthLocalReachable(origID) => // Renaming propagation
                    registerRenaming(id, origID)
                    cse(current :: tail)
                  case LocalSet(id) if isImmutable(id) && isSynthLocalReachable(origID) => // Renaming propagation
                    registerRenaming(id, origID)
                    cse(tail)
                  case RefCast(refType) =>
                    applyCSE(current, origID, refType, next, tail)
                  case RefAsNonNull =>
                    val intendedType = getLocalType(origID).asInstanceOf[RefType].toNonNullable
                    applyCSE(current, origID, intendedType, next, tail)
                  case sg: StructGet =>
                    val fieldType = structFieldIdxFieldType((sg.structTypeID,sg.fieldID))
                    if (!fieldType.isMutable) {
                      val tp = storageTypeToType(fieldType.tpe)
                      applyCSE(current, origID, tp, next, tail)
                    } else {
                      cseResult += current
                      cse(next :: tail)
                    }
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
          val toRemove = initSynthsStack.head
          synthsOnScope.remove(toRemove)
          cseCTX -= cseCTXLog(toRemove)
          cseCTXLog -= toRemove
          initSynthsStack = initSynthsStack.tail
        }
      }

      private def registerSynthLocal(id: LocalID): Unit = {
        synthsOnScope += id
        initSynthsStack = id :: initSynthsStack
      }

      private def registerRenaming(toRename: LocalID, origID: LocalID): Unit = {
        renamedLocals += (toRename -> origID)
        renamedLog.update(origID, toRename :: renamedLog.getOrElse(origID, Nil))
      }

      private def applyCSE(current: Instr, i: LocalID, tp: Types.Type, next: Instr, tail: List[Instr]): Unit = {
        if (cseCTX.contains((current, next))) { // Pair of instructions already seen, so apply cse
          cse(LocalGet(cseCTX(current, next)) :: tail)
        } else { // First time seen, so register it on the map and link it to a freshLocal
          val freshID = makeSyntheticLocal(tp)
          registerSynthLocal(freshID)
          cseCTX.update((current, next), freshID)
          cseCTXLog.update(freshID, (current, next))
          cseResult ++= Seq(current, next, LocalSet(freshID))
          cse(LocalGet(freshID) :: tail)
        }
      }

      private def isSynthLocalReachable(id: LocalID): Boolean = synthsOnScope(id)

      private def isImmutable(id: LocalID): Boolean =
        candidates(id) || (controlStack.nonEmpty && isSynthLocalReachable(id))
      // Either a defined candidate or a LocalID made by CSE

      private def collectCandidates(instructions: List[Instr]): Set[LocalID] = {
        val params = function.params.map(_.id).toSet
        val setCount: mutable.Map[LocalID, Int] = mutable.Map.empty.withDefaultValue(0)
        params.foreach(id => setCount.update(id, 0))
        val it = instructions.iterator
        while (it.hasNext) {
          val instr = it.next()
          instr match {
            case LocalSet(i) => setCount(i) += 1
            case LocalTee(i) => setCount(i) += 1
            case _ =>
          }
        }
        setCount.toMap.filter(tuple => {
          val (id, count) = tuple
          (count == 0 && params(id)) || (count == 1 && !params(id))
        }).keySet
      }

      private def tidy(instructions: List[Instr]): List[Instr] = {
        val resultBuilder = List.newBuilder[Instr]
        val synthsToRemove: mutable.Set[LocalID] = mutable.Set.empty
        synthsToRemove ++= synthLocals.keySet

        @tailrec
        def tidyRec(instructions: List[Instr]): List[Instr] = {
          instructions match {
            case LocalSet(is) :: LocalGet(ig) :: tail if ig == is =>
              resultBuilder += LocalTee(ig)
              tidyRec(tail)
            case LocalGet(id) :: tail =>
              resultBuilder += LocalGet(id)
              synthsToRemove -= id
              tidyRec(tail)
            case current :: tail =>
              resultBuilder += current
              tidyRec(tail)
            case Nil => resultBuilder.result()
          }
        }

        val withoutUnusedSynths = List.newBuilder[Instr]
        tidyRec(instructions).foreach {
          case LocalTee(id) =>
            if (synthsToRemove(id)) {
              synthLocals -= id
            } else {
              withoutUnusedSynths += LocalTee(id)
            }
          case LocalSet(id) =>
            if (synthsToRemove(id)) {
              withoutUnusedSynths += Drop
              synthLocals -= id
            } else {
              withoutUnusedSynths += LocalSet(id)
            }
          case instr =>
            withoutUnusedSynths += instr
        }
        withoutUnusedSynths.result()
      }

      private case class ControlFrame(kind: Instr, var startSynthsHeight: Int, var hasIndirection: Boolean = false)
    }

  }
}
