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

package org.scalajs.linker.backend.webassembly

import scala.collection.mutable

import org.scalajs.ir.{OriginalName, Position}

import Instructions._
import Identitities._
import Modules._
import Types._

final class FunctionBuilder(
    moduleBuilder: ModuleBuilder,
    val functionID: FunctionID,
    val functionOriginalName: OriginalName,
    functionPos: Position
) {
  import FunctionBuilder._

  private var labelIdx = 0

  private val params = mutable.ListBuffer.empty[Local]
  private val locals = mutable.ListBuffer.empty[Local]
  private var resultTypes: List[Type] = Nil

  private var specialFunctionType: Option[TypeID] = None

  /** The instructions buffer. */
  private val instrs: mutable.ListBuffer[Instr] = mutable.ListBuffer.empty

  // Signature building

  /** Adds one parameter to the function with the given orignal name and type.
   *
   *  Returns the `LocalID` of the new parameter.
   *
   *  @note
   *    This follows a builder pattern to easily and safely correlate the
   *    definition of a parameter and extracting its `LocalID`.
   */
  def addParam(originalName: OriginalName, tpe: Type): LocalID = {
    val id = new ParamIDImpl(params.size, originalName)
    params += Local(id, originalName, tpe)
    id
  }

  /** Adds one parameter to the function with the given orignal name and type.
   *
   *  Returns the `LocalID` of the new parameter.
   *
   *  @note
   *    This follows a builder pattern to easily and safely correlate the
   *    definition of a parameter and extracting its `LocalID`.
   */
  def addParam(name: String, tpe: Type): LocalID =
    addParam(OriginalName(name), tpe)

  /** Sets the list of result types of the function to build.
   *
   *  By default, the list of result types is `Nil`.
   *
   *  @note
   *    This follows a builder pattern to be consistent with `addParam`.
   */
  def setResultTypes(tpes: List[Type]): Unit =
    resultTypes = tpes

  /** Sets the list of result types to a single type.
   *
   *  This method is equivalent to
   *  {{{
   *  setResultTypes(tpe :: Nil)
   *  }}}
   *
   *  @note
   *    This follows a builder pattern to be consistent with `addParam`.
   */
  def setResultType(tpe: Type): Unit =
    setResultTypes(tpe :: Nil)

  /** Specifies the function type to use for the function.
   *
   *  If this method is not called, a default function type will be
   *  automatically generated. Generated function types are always alone in a
   *  recursive type group, without supertype, and final.
   *
   *  Use `setFunctionType` if the function must conform to a specific function
   *  type, such as one that is defined within a recursive type group, or that
   *  is a subtype of other function types.
   *
   *  The given function type must be consistent with the params created with
   *  `addParam` and with the result types specified by `setResultType(s)`.
   *  Using `setFunctionType` does not implicitly set any result type or create
   *  any parameter (it cannot, since it cannot *resolve* the `typeID` to a
   *  `FunctionType`).
   */
  def setFunctionType(typeID: TypeID): Unit =
    specialFunctionType = Some(typeID)

  // Local definitions

  def genLabel(): LabelID = {
    val label = new LabelIDImpl(labelIdx)
    labelIdx += 1
    label
  }

  def addLocal(originalName: OriginalName, tpe: Type): LocalID = {
    val id = new LocalIDImpl(locals.size, originalName)
    locals += Local(id, originalName, tpe)
    id
  }

  def addLocal(name: String, tpe: Type): LocalID =
    addLocal(OriginalName(name), tpe)

  // Instructions

  def +=(instr: Instr): Unit =
    instrs += instr

  def ++=(instrs: Iterable[Instr]): Unit =
    this.instrs ++= instrs

  def markCurrentInstructionIndex(): InstructionIndex =
    new InstructionIndex(instrs.size)

  def insert(index: InstructionIndex, instr: Instr): Unit =
    instrs.insert(index.value, instr)

  def insertAll(index: InstructionIndex, instrs: List[Instr]): Unit =
    this.instrs.insertAll(index.value, instrs)

  // Helpers to build structured control flow

  def sigToBlockType(sig: FunctionType): BlockType = sig match {
    case FunctionType(Nil, Nil) =>
      BlockType.ValueType()
    case FunctionType(Nil, resultType :: Nil) =>
      BlockType.ValueType(resultType)
    case _ =>
      BlockType.FunctionType(moduleBuilder.functionTypeToTypeID(sig))
  }

  private def toBlockType[BT: BlockTypeLike](blockType: BT): BlockType =
    implicitly[BlockTypeLike[BT]].toBlockType(this, blockType)

  /* Work around a bug in the Scala compiler.
   *
   * We force it to see `ForResultTypes` here, so that it actually typechecks
   * it and realizes that it is a valid implicit instance of
   * `BlockTypeLike[ForResultTypes]`. I guess this is because it appears later
   * in the same file.
   *
   * If we remove this line, the invocations with `()` in this file, which
   * desugar to `(Nil)` due to the default value, do not find the implicit value.
   */
  BlockTypeLike.ForResultTypes

  def ifThenElse[BT: BlockTypeLike](blockType: BT = Nil)(thenp: => Unit)(elsep: => Unit): Unit = {
    instrs += If(toBlockType(blockType))
    thenp
    instrs += Else
    elsep
    instrs += End
  }

  def ifThen[BT: BlockTypeLike](blockType: BT = Nil)(thenp: => Unit): Unit = {
    instrs += If(toBlockType(blockType))
    thenp
    instrs += End
  }

  def block[BT: BlockTypeLike, A](blockType: BT = Nil)(body: LabelID => A): A = {
    val label = genLabel()
    instrs += Block(toBlockType(blockType), Some(label))
    val result = body(label)
    instrs += End
    result
  }

  def loop[BT: BlockTypeLike, A](blockType: BT = Nil)(body: LabelID => A): A = {
    val label = genLabel()
    instrs += Loop(toBlockType(blockType), Some(label))
    val result = body(label)
    instrs += End
    result
  }

  def whileLoop()(cond: => Unit)(body: => Unit): Unit = {
    loop() { loopLabel =>
      cond
      ifThen() {
        body
        instrs += Br(loopLabel)
      }
    }
  }

  def tryTable[BT: BlockTypeLike, A](blockType: BT = Nil)(
      clauses: List[CatchClause])(body: => A): A = {
    instrs += TryTable(toBlockType(blockType), clauses)
    val result = body
    instrs += End
    result
  }

  /** Builds a `switch` over a scrutinee using a `br_table` instruction.
   *
   *  This function produces code that encodes the following control-flow:
   *
   *  {{{
   *  switch (scrutinee) {
   *    case clause0_alt0 | ... | clause0_altN => clause0_body
   *    ...
   *    case clauseM_alt0 | ... | clauseM_altN => clauseM_body
   *    case _ => default
   *  }
   *  }}}
   *
   *  All the alternative values must be non-negative and distinct, but they need not be
   *  consecutive. The highest one must be strictly smaller than 128, as a safety precaution against
   *  generating unexpectedly large tables.
   *
   *  @param scrutineeSig
   *    The signature of the `scrutinee` block, *excluding* the i32 result that will be switched
   *    over.
   *  @param clauseSig
   *    The signature of every `clauseI_body` block and of the `default` block. The clauses' params
   *    must consume at least all the results of the scrutinee.
   */
  def switch(scrutineeSig: FunctionType, clauseSig: FunctionType)(
      scrutinee: () => Unit)(
      clauses: (List[Int], () => Unit)*)(
      default: () => Unit): Unit = {

    // Check prerequisites

    require(clauseSig.params.size >= scrutineeSig.results.size,
        "The clauses of a switch must consume all the results of the scrutinee " +
        s"(scrutinee results: ${scrutineeSig.results}; clause params: ${clauseSig.params})")

    val numCases = clauses.map(_._1.max).max + 1
    require(numCases <= 128, s"Too many cases for switch: $numCases")

    // Allocate all the labels we will use
    val doneLabel = genLabel()
    val defaultLabel = genLabel()
    val clauseLabels = clauses.map(_ => genLabel())

    // Build the dispatch vector, i.e., the array of caseValue -> target clauseLabel
    val dispatchVector = {
      val dv = Array.fill(numCases)(defaultLabel)
      for {
        ((caseValues, _), clauseLabel) <- clauses.zip(clauseLabels)
        caseValue <- caseValues
      } {
        require(dv(caseValue) == defaultLabel, s"Duplicate case value for switch: $caseValue")
        dv(caseValue) = clauseLabel
      }
      dv.toList
    }

    // Input parameter to the overall switch "instruction"
    val switchInputParams =
      clauseSig.params.drop(scrutineeSig.results.size) ::: scrutineeSig.params

    // Compute the BlockType's we will need
    val doneBlockType = sigToBlockType(FunctionType(switchInputParams, clauseSig.results))
    val clauseBlockType = sigToBlockType(FunctionType(switchInputParams, clauseSig.params))

    // Open done block
    instrs += Block(doneBlockType, Some(doneLabel))
    // Open case and default blocks (in reverse order: default block is outermost!)
    for (label <- (defaultLabel +: clauseLabels.reverse)) {
      instrs += Block(clauseBlockType, Some(label))
    }

    // Load the scrutinee and dispatch
    scrutinee()
    instrs += BrTable(dispatchVector, defaultLabel)

    // Close all the case blocks and emit their respective bodies
    for ((_, caseBody) <- clauses) {
      instrs += End // close the block whose label is the corresponding label for this clause
      caseBody() // emit the body of that clause
      instrs += Br(doneLabel) // jump to done
    }

    // Close the default block and emit its body (no jump to done necessary)
    instrs += End
    default()

    instrs += End // close the done block
  }

  def switch(clauseSig: FunctionType)(scrutinee: () => Unit)(
      clauses: (List[Int], () => Unit)*)(default: () => Unit): Unit = {
    switch(FunctionType.NilToNil, clauseSig)(scrutinee)(clauses: _*)(default)
  }

  def switch(resultType: Type)(scrutinee: () => Unit)(
      clauses: (List[Int], () => Unit)*)(default: () => Unit): Unit = {
    switch(FunctionType(Nil, List(resultType)))(scrutinee)(clauses: _*)(default)
  }

  def switch()(scrutinee: () => Unit)(
      clauses: (List[Int], () => Unit)*)(default: () => Unit): Unit = {
    switch(FunctionType.NilToNil)(scrutinee)(clauses: _*)(default)
  }

  // Final result

  def buildAndAddToModule(): Function = {
    val functionTypeID = specialFunctionType.getOrElse {
      val sig = FunctionType(params.toList.map(_.tpe), resultTypes)
      moduleBuilder.functionTypeToTypeID(sig)
    }

    val dcedInstrs = localDeadCodeEliminationOfInstrs()

    val func = Function(
      functionID,
      functionOriginalName,
      functionTypeID,
      params.toList,
      resultTypes,
      locals.toList,
      Expr(dcedInstrs),
      functionPos
    )
    moduleBuilder.addFunction(func)
    func
  }

  /** Performs local dead code elimination and produces the final list of instructions.
   *
   *  After a stack-polymorphic instruction, the rest of the block is unreachable. In theory,
   *  WebAssembly specifies that the rest of the block should be type-checkeable no matter the
   *  contents of the stack. In practice, however, it seems V8 cannot handle `throw_ref` in such a
   *  context. It reports a validation error of the form "invalid type for throw_ref: expected
   *  exnref, found <bot>".
   *
   *  We work around this issue by forcing a pass of local dead-code elimination. This is in fact
   *  straightforwrd: after every stack-polymorphic instruction, ignore all instructions until the
   *  next `Else` or `End`. The only tricky bit is that if we encounter nested
   *  `StructuredLabeledInstr`s during that process, must jump over them. That means we need to
   *  track the level of nesting at which we are.
   */
  private def localDeadCodeEliminationOfInstrs(): List[Instr] = {
    val resultBuilder = List.newBuilder[Instr]

    val iter = instrs.iterator
    while (iter.hasNext) {
      // Emit the current instruction
      val instr = iter.next()
      resultBuilder += instr

      /* If it is a stack-polymorphic instruction, dead-code eliminate until the
       * end of the current block.
       */
      if (instr.isInstanceOf[StackPolymorphicInstr]) {
        var nestingLevel = 0

        while (nestingLevel >= 0 && iter.hasNext) {
          val deadCodeInstr = iter.next()
          deadCodeInstr match {
            case End | Else | _: Catch if nestingLevel == 0 =>
              /* We have reached the end of the original block of dead code.
               * Actually emit this END or ELSE and then drop `nestingLevel`
               * below 0 to end the dead code processing loop.
               */
              resultBuilder += deadCodeInstr
              nestingLevel = -1 // acts as a `break` instruction

            case End =>
              nestingLevel -= 1

            case _: StructuredLabeledInstr =>
              nestingLevel += 1

            case _ =>
              ()
          }
        }
      }
    }

    resultBuilder.result()
  }
}

object FunctionBuilder {
  private final class ParamIDImpl(index: Int, originalName: OriginalName) extends LocalID {
    override def toString(): String =
      if (originalName.isDefined) originalName.get.toString()
      else s"<param $index>"
  }

  private final class LocalIDImpl(index: Int, originalName: OriginalName) extends LocalID {
    override def toString(): String =
      if (originalName.isDefined) originalName.get.toString()
      else s"<local $index>"
  }

  private final class LabelIDImpl(index: Int) extends LabelID {
    override def toString(): String = s"<label $index>"
  }

  final class InstructionIndex(private[FunctionBuilder] val value: Int) extends AnyVal

  sealed abstract class BlockTypeLike[-A] {
    def toBlockType(fb: FunctionBuilder, value: A): BlockType
  }

  object BlockTypeLike {
    implicit object ForBlockType extends BlockTypeLike[BlockType] {
      def toBlockType(fb: FunctionBuilder, value: BlockType): BlockType = value
    }

    implicit object ForFunctionType extends BlockTypeLike[FunctionType] {
      def toBlockType(fb: FunctionBuilder, value: FunctionType): BlockType =
        fb.sigToBlockType(value)
    }

    implicit object ForResultTypes extends BlockTypeLike[List[Type]] {
      def toBlockType(fb: FunctionBuilder, value: List[Type]): BlockType =
        fb.sigToBlockType(FunctionType(Nil, value))
    }

    implicit object ForResultType extends BlockTypeLike[Type] {
      def toBlockType(fb: FunctionBuilder, value: Type): BlockType =
        BlockType.ValueType(value)
    }
  }
}
