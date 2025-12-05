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
    val optimizedFuncs = wasmModule.funcs.map(LICMOptimization(_).apply).map(CSEOptimization(_).apply)
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
    protected val localIdXLocalType: Map[LocalID, Type] =
      (function.locals ++ function.params).map(local => local.id -> local.tpe).toMap
    protected val synthLocals: mutable.ListBuffer[Modules.Local] = mutable.ListBuffer.empty

    def apply: Modules.Function

    protected def getLocalType(id: LocalID): Types.Type = {
      localIdXLocalType.getOrElse(id, synthLocals.find(_.id == id).get.tpe)
    }

    protected def makeSyntheticLocal(tp: Type): LocalID = {
      val noOrigName = OriginalName.NoOriginalName
      val freshID = new SynthLocalIDImpl(function.locals.size + synthLocals.size,
        OriginalName.NoOriginalName)
      synthLocals += Modules.Local(freshID, noOrigName, tp)
      freshID
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
        function.pos,
      )

    protected def tidy(instructions: List[Instr]): List[Instr] = {
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
      val updatedLocals = function.locals ++ synthLocals.toList
      functionModuleFromLocalsAndBody(updatedLocals, optimizedBody)
    }

    private def loopInvariantCodeMotion(instructions: List[Instr]): Unit = {
      val it = instructions.iterator
      while(it.hasNext) {
        val current = it.next()
        current match {
          case loop: Loop if isLoopEmptyToEmpty(loop) =>
            val ((hoisted, optimizedBody), remaining) = applyLICM(it.toList)
            res ++= hoisted
            res ++= loop :: optimizedBody
            loopInvariantCodeMotion(remaining)
          case _ =>
            res += current
        }
      }
    }

    private def applyLICM(instructions: List[Instr]): ((List[Instr], List[Instr]), List[Instr]) = {
      val toExtract: ListBuffer[Instr] = ListBuffer.empty
      val extracted: ListBuffer[Instr] = ListBuffer.empty
      val updatedBody: ListBuffer[Instr] = ListBuffer.empty
      var level = 1
      val typeStack = TypeStack(collectLocalsSetWithinLoop(instructions))

      def tryExtraction(instruction: Instr): Unit = {
        typeStack.updateStack(instruction)
        if (!typeStack.isExtractable(instruction)) {
          if (toExtract.size >= 2) {
            val stackedTypes: Option[List[Type]] = typeStack.popAllStack()
            val getSynths: ListBuffer[LocalGet] = ListBuffer.empty
            extracted ++= toExtract
            toExtract.clear()
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
              case sl:StructuredLabeledInstr =>
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
                //updatedBody += pm
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

    private def collectLocalsSetWithinLoop(loopBody: List[Instr]): Set[LocalID] = {
      val it = loopBody.iterator
      val setWithinLoop: mutable.Set[LocalID] = mutable.Set.empty
      var level = 0
      while(it.hasNext && level >= 0) {
        val current = it.next()
        current match {
          case LocalSet(id) =>
            setWithinLoop += id
          case LocalTee(id) =>
            setWithinLoop += id
          case _:StructuredLabeledInstr =>
            level += 1
          case End =>
            level -= 1
          case _ =>
        }
      }
      setWithinLoop.toSet
    }

    private case class TypeStack(private val setWithinLoop: Set[LocalID]) {
      private var typeStack: List[(Type, Boolean)] = List.empty // (Type, and whether it will be an invariant)
      private var isStackPure: Boolean = true
      private var isLoopPrefix: Boolean = true
      private val localSetFromPureArg: mutable.Set[LocalID] = mutable.Set.empty

      private def pop(): Option[Type] = {
        if (typeStack.isEmpty) {
          // If we pop one element on an empty TypeStack,
          // it means that the element has been pushed before by some unpure instruction
          //unpureInstrIsMet()
          isStackPure = false
          None
        } else {
          val head = typeStack.head._1
          typeStack = typeStack.tail
          Some(head)
        }
      }

      private def push(tp: Type): Unit = {
        if (isStackPure) {
          typeStack = (tp, true) :: typeStack
        }
      }

      def popAllStack(): Option[List[Type]] = {
        if (typeStack.isEmpty) {
          None
        } else {
          val res = typeStack.map(_._1)
          typeStack = List.empty
          Some(res)
        }
      }

      def isExtractable(instr: Instr): Boolean =
        isStackPure

      private def unpureInstrIsMet(): Unit = {
        isStackPure = false
        isLoopPrefix = false
      }

      private val i32BinOp: Set[Instr] = Set(
        I32Eq, I32Ne,
          I32LtS, I32LtU, I32LeS, I32LeU,
          I32GtS, I32GtU, I32GeS, I32GeU,
        I32Add, I32Sub, I32Mul,
          I32DivS, I32DivU, I32RemS, I32RemU,
          I32And, I32Or, I32Xor, I32Shl, I32ShrS, I32ShrU, I32Rotl, I32Rotr
      )
      private val i64toi32BinOp: Set[Instr] = Set(
        I64Eq, I64Ne,
          I64LtS, I64LtU, I64LeS, I64LeU,
          I64GtS, I64GtU, I64GeS, I64GeU,

      )
      private val i64toI64BinOp: Set[Instr] = Set(
        I64Add, I64Sub, I64Mul,
        I64DivS, I64DivU, I64RemS, I64RemU,
        I64And, I64Or, I64Xor, I64Shl, I64ShrS, I64ShrU, I64Rotl, I64Rotr
      )
      private val f32BinOp: Set[Instr] = Set(
        F32Eq, F32Ne, F32Lt, F32Gt, F32Le, F32Ge
      )
      private val f64BinOp: Set[Instr] = Set(
        F64Eq, F64Ne, F64Lt, F64Gt, F64Le, F64Ge
      )

      def updateStack(instr: Instr): Unit = {
        isStackPure = true
        instr match {
          case If(_,_) => unpureInstrIsMet()
          case I32Const(v) => push(Int32)
          case I64Const(v) => push(Int64)
          case F32Const(v) => push(Float32)
          case F64Const(v) => push(Float64)
          // i32 -> i32
          case I32Eqz =>
            if (typeStack.size >= 1) {
              pop()
              push(Int32)
            } else { unpureInstrIsMet() }
          // [i32, i32] -> i32
          case op if i32BinOp(op) =>
            if (typeStack.size >= 2) {
              pop()
              pop()
              push(Int32)
            } else { unpureInstrIsMet() }
          // i64 -> i32
          case I64Eqz =>
            if (typeStack.size >= 1) {
              pop()
              push(Int32)
            } else { unpureInstrIsMet() }
          // [i64, i64] -> i32
          case op if i64toi32BinOp(op) =>
            if (typeStack.size >= 2) {
              pop()
              pop()
              push(Int32)
            } else { unpureInstrIsMet() }
          // [f32, f32] -> i32
          case op if f32BinOp(op) =>
            if (typeStack.size >= 2) {
              pop()
              pop()
              push(Int32)
            } else { unpureInstrIsMet() }
          // [f64, f64] -> i32
          case op if f64BinOp(op) =>
            if (typeStack.size >= 2) {
              pop()
              pop()
              push(Int32)
            } else { unpureInstrIsMet() }
          // i32 -> i32
          case I32Clz | I32Ctz | I32Popcnt =>
            if (typeStack.size >= 1) {
              pop()
              push(Int32)
            } else {
              unpureInstrIsMet()
            }
          // i64 -> i64
          case I64Clz | I64Ctz | I64Popcnt =>
            if (typeStack.size >= 1) {
              pop()
              push(Int64)
            } else {
              unpureInstrIsMet()
            }
          // [i64, i64] -> i64
          case op if i64toI64BinOp(op) =>
            if (typeStack.size >= 2) {
              pop()
              pop()
              push(Int64)
            } else { unpureInstrIsMet() }
          case LocalGet(id) =>
            isStackPure = (!setWithinLoop(id) || localSetFromPureArg(id))
            val localType = localIdXLocalType(id)
            push(localType)
          case LocalSet(id) =>
            if (typeStack.size >=  1) {
              pop()
              localSetFromPureArg += id
            } else {
              localSetFromPureArg -= id
              unpureInstrIsMet()
            }
          case LocalTee(id) =>
            if (typeStack.size >=  1) {
              pop()
              localSetFromPureArg += id
              val localType = localIdXLocalType(id)
              push(localType)
            } else {
              localSetFromPureArg -= id
              unpureInstrIsMet()
            }
          case StructGet(tyidx, fidx) =>
            if (typeStack.size >=  1) {
              val fieldType = structFieldIdxFieldType(tyidx, fidx)
              if (fieldType.isMutable) {
                unpureInstrIsMet()
              } else {
                pop()
                push(storageTypeToType(fieldType.tpe))
              }
            } else { unpureInstrIsMet() }
          case RefAsNonNull =>
            if (typeStack.size >=  1) {
              if (!isLoopPrefix) { // Not pure but idempotent
                unpureInstrIsMet()
              } else {
                val stacked = pop()
                stacked.foreach(t => push(t.asInstanceOf[RefType].toNonNullable))
              }
            } else {
              unpureInstrIsMet()
            }
          case _ => unpureInstrIsMet()
        }
      }

    }
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
      val updatedLocals = function.locals ++ synthLocals.toList
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
    private case class ControlFrame(kind: Instr, var startSynthsHeight: Int, var hasIndirection: Boolean = false)
  }

}
