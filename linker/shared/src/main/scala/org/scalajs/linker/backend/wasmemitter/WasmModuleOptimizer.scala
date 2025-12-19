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
    val optimizedFuncs =
      wasmModule.funcs
        //.map(CSEOptimization(_).apply)
        .map(LICMOptimization(_).apply)
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

  private abstract class WasmFunctionOptimizer(private val function: Modules.Function) {
    private val localIdXLocalType: Map[LocalID, Type] =
      (function.locals ++ function.params).map(local => local.id -> local.tpe).toMap
    protected val synthLocals: mutable.Map[LocalID, Modules.Local] = mutable.Map.empty

    def apply: Modules.Function

    protected def getLocalType(id: LocalID): Types.Type = {
      localIdXLocalType.getOrElse(id, synthLocals(id).tpe)
    }

    protected def makeSyntheticLocal(tp: Type): LocalID = {
      val noOrigName = OriginalName.NoOriginalName
      val freshID = new SynthLocalIDImpl(function.locals.size + synthLocals.size,
        OriginalName.NoOriginalName)
      synthLocals += freshID -> Modules.Local(freshID, noOrigName, tp)
      freshID
    }

    protected def tidy(instructions: List[Instr]): List[Instr] = {
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

    protected def functionModuleFromLocalsAndBody(locals: List[Modules.Local],
                                                instrs: List[Instr]): Modules.Function =
      Modules.Function(
        function.id,
        function.originalName,
        function.typeID,
        function.params,
        function.results,
        locals,
        Expr(instrs),
        function.pos
      )

    private final class SynthLocalIDImpl(index: Int, originalName: OriginalName) extends LocalID {
      override def toString(): String =
        if (originalName.isDefined) originalName.get.toString()
        else s"<local $index>"
    }

    protected def storageTypeToType(tpe: StorageType): Types.Type = {
      tpe match {
        case t: Types.Type => t
        case Int8 | Int16 => Int32
      }
    }
  }

  private case class LICMOptimization(private val function: Modules.Function) extends WasmFunctionOptimizer(function) {
    private val res: ListBuffer[Instr] = ListBuffer.empty
    override def apply: Modules.Function = {
      loopInvariantCodeMotion(function.body.instr)
      val optimizedBody = res.result()
      val updatedLocals = function.locals ++ synthLocals.values
      functionModuleFromLocalsAndBody(updatedLocals, optimizedBody)
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
      val toExtract: ListBuffer[Instr] = ListBuffer.empty
      var toExtractSize = 0 // toExtract.size without the markers
      val extracted: ListBuffer[Instr] = ListBuffer.empty
      val updatedBody: ListBuffer[Instr] = ListBuffer.empty
      var level = 1
      val (setOnce, setTwice) = collectLocalsSetWithinLoop(instructions)
      val typeStack = TypeStack(setOnce, setTwice)

      def tryExtraction(instruction: Instr): Unit = {
        typeStack.updateStack(instruction)
        if (!typeStack.isExtractable(instruction)) {
          if (toExtractSize >= 2) {
            val stackedTypes: Option[List[Type]] = typeStack.popAllStack()
            val getSynths: ListBuffer[LocalGet] = ListBuffer.empty
            extracted ++= toExtract
            toExtract.clear()
            toExtractSize = 0
            stackedTypes.foreach(types => {
              types.foreach(tp => {
                val freshId = makeSyntheticLocal(tp)
                extracted += LocalSet(freshId)
                getSynths += LocalGet(freshId)
              })
            })
            updatedBody ++= getSynths.reverse
            updatedBody += instruction
          } else {
            typeStack.popAllStack()
            updatedBody ++= toExtract
            toExtract.clear()
            toExtractSize = 0
            updatedBody += instruction
          }
        } else {
          toExtract += instruction
          toExtractSize += 1
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
                toExtract += pm
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
                                 private val setTwiceWithinLoop: Set[LocalID]) {
      private var typeStack: List[Type] = List.empty // (Type, and whether it will be an invariant)
      private var typeStackHeight: Int = 0
      private var isStackPure: Boolean = true
      private var isLoopPrefix: Boolean = true
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

      def isExtractable(instr: Instr): Boolean =
        isStackPure

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


      def updateStack(instr: Instr): Unit = {
        isStackPure = true
        instr match {
          case If(_,_) => unpureInstrIsMet()
          case I32Const(v) => push(Int32)
          case I64Const(v) => push(Int64)
          case F32Const(v) => push(Float32)
          case F64Const(v) => push(Float64)

          case op if instrToPureOperation.contains(op) =>
            val pureInstr = instrToPureOperation(op)
            if(typeStackHeight >= pureInstr.in.size) {
              pureInstr.in.foreach(_ => pop())
              pureInstr.out.foreach(push)
            } else {
              unpureInstrIsMet()
            }
          case LocalGet(id) =>
            isStackPure = (!setWithinLoop(id) || localSetFromPureArg(id))
            val localType = getLocalType(id)
            push(localType)
          case LocalSet(id) =>
            if (isLoopPrefix && !setTwiceWithinLoop(id) && typeStack.nonEmpty) {
              pop()
              localSetFromPureArg += id
            } else {
              localSetFromPureArg -= id
              unpureInstrIsMet()
            }
          case LocalTee(id) =>
            if (isLoopPrefix && !setTwiceWithinLoop(id) && typeStack.nonEmpty) {
              pop()
              localSetFromPureArg += id
              val localType = getLocalType(id)
              push(localType)
            } else {
              localSetFromPureArg -= id
              unpureInstrIsMet()
            }
          case StructGet(tyidx, fidx) =>
            if (typeStack.nonEmpty && (isLoopPrefix || isNonNullable(typeStack.head))) {
              val fieldType = structFieldIdxFieldType((tyidx, fidx))
              if (fieldType.isMutable) {
                unpureInstrIsMet()
              } else {
                pop()
                push(storageTypeToType(fieldType.tpe))
              }
            } else { unpureInstrIsMet() }
          case RefAsNonNull =>
            if (typeStack.nonEmpty && isLoopPrefix) {
              val stacked = pop()
              stacked.foreach(t => push(t.asInstanceOf[RefType].toNonNullable))
            } else {
              unpureInstrIsMet()
            }
          case RefCast(refType) =>
            if (typeStack.nonEmpty && isLoopPrefix) {
              pop()
              push(refType)
            } else {
              unpureInstrIsMet()
            }
          case _ => unpureInstrIsMet()
        }
      }
    }

    private case class PureOperation(in: Seq[Type], out: Seq[Type])
  }


  private case class CSEOptimization(private val function: Modules.Function) extends WasmFunctionOptimizer(function) {
    private val candidates: Set[LocalID] = collectCandidates(function.body.instr)

    /** Renaming locals is done to propagate CSE, always starting from a synthetic local */
    private val renamedLocals: mutable.Map[LocalID, LocalID] = mutable.Map.empty
    private val cseCTX: mutable.Map[(Instr, Instr), LocalID] = mutable.Map.empty
    private var controlStack: List[ControlFrame] = List(ControlFrame(Unreachable, 0))
    private val synthsOnScope: mutable.Set[LocalID] = mutable.Set.empty
    private var initSynthsStack: List[LocalID] = List.empty

    private val cseResult = mutable.ListBuffer.empty[Instr]

    override def apply: Modules.Function = {
      val optimizedBody = tidy(cse(function.body.instr))
      val updatedLocals = function.locals ++ synthLocals.values.toList
      functionModuleFromLocalsAndBody(updatedLocals, optimizedBody)
    }

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
          val freshID = makeSyntheticLocal(tp)
          registerSynthLocal(freshID)
          cseCTX.update((current, next), freshID)
          cseResult ++= Seq(current, next, LocalSet(freshID))
          cse(LocalGet(freshID) :: tail)
        }
      }
    }

    private def isSynthLocalReachable(id: LocalID): Boolean =
      synthsOnScope(id)

    private def isImmutable(id: LocalID): Boolean =
      candidates(id) || (controlStack.nonEmpty && isSynthLocalReachable(id))
    // Either a defined candidate or a LocalID made by CSE

    private def collectCandidates(instructions: List[Instr]): Set[LocalID] = {
      val finalSet: mutable.Set[LocalID] = mutable.Set.empty
      val prohibitedSet: mutable.Set[LocalID] = mutable.Set.empty
      var localStackHeight: Int = 0
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
          localStackHeight += 1
        }
      }
      def resetScope(): Unit = {
        val nextSynthHeight = controlStack.head.startSynthsHeight
        while (localStackHeight > nextSynthHeight) {
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
          localStackHeight -= 1
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
            controlStack = ControlFrame(instr, localStackHeight) :: controlStack
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
    private case class ControlFrame(kind: Instr, var startSynthsHeight: Int, var hasIndirection: Boolean = false)
  }
}
