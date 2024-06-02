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

  def setFunctionType(typeID: TypeID): Unit =
    specialFunctionType = Some(typeID)

  def setResultTypes(tpes: List[Type]): Unit =
    resultTypes = tpes

  def setResultType(tpe: Type): Unit =
    setResultTypes(tpe :: Nil)

  def addParam(originalName: OriginalName, tpe: Type): LocalID = {
    val id = new ParamIDImpl(params.size, originalName)
    params += Local(id, originalName, tpe)
    id
  }

  def addParam(name: String, tpe: Type): LocalID =
    addParam(OriginalName(name), tpe)

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

  // Helpers to build structured control flow

  def sigToBlockType(sig: FunctionType): BlockType = sig match {
    case FunctionType(Nil, Nil) =>
      BlockType.ValueType()
    case FunctionType(Nil, resultType :: Nil) =>
      BlockType.ValueType(resultType)
    case _ =>
      BlockType.FunctionType(moduleBuilder.functionTypeToTypeID(sig))
  }

  def ifThenElse(blockType: BlockType)(thenp: => Unit)(elsep: => Unit): Unit = {
    instrs += If(blockType)
    thenp
    instrs += Else
    elsep
    instrs += End
  }

  def ifThenElse(resultType: Type)(thenp: => Unit)(elsep: => Unit): Unit =
    ifThenElse(BlockType.ValueType(resultType))(thenp)(elsep)

  def ifThenElse(sig: FunctionType)(thenp: => Unit)(elsep: => Unit): Unit =
    ifThenElse(sigToBlockType(sig))(thenp)(elsep)

  def ifThenElse(resultTypes: List[Type])(thenp: => Unit)(elsep: => Unit): Unit =
    ifThenElse(FunctionType(Nil, resultTypes))(thenp)(elsep)

  def ifThenElse()(thenp: => Unit)(elsep: => Unit): Unit =
    ifThenElse(BlockType.ValueType())(thenp)(elsep)

  def ifThen(blockType: BlockType)(thenp: => Unit): Unit = {
    instrs += If(blockType)
    thenp
    instrs += End
  }

  def ifThen(sig: FunctionType)(thenp: => Unit): Unit =
    ifThen(sigToBlockType(sig))(thenp)

  def ifThen(resultTypes: List[Type])(thenp: => Unit): Unit =
    ifThen(FunctionType(Nil, resultTypes))(thenp)

  def ifThen()(thenp: => Unit): Unit =
    ifThen(BlockType.ValueType())(thenp)

  def block[A](blockType: BlockType)(body: LabelID => A): A = {
    val label = genLabel()
    instrs += Block(blockType, Some(label))
    val result = body(label)
    instrs += End
    result
  }

  def block[A](resultType: Type)(body: LabelID => A): A =
    block(BlockType.ValueType(resultType))(body)

  def block[A]()(body: LabelID => A): A =
    block(BlockType.ValueType())(body)

  def block[A](sig: FunctionType)(body: LabelID => A): A =
    block(sigToBlockType(sig))(body)

  def block[A](resultTypes: List[Type])(body: LabelID => A): A =
    block(FunctionType(Nil, resultTypes))(body)

  def loop[A](blockType: BlockType)(body: LabelID => A): A = {
    val label = genLabel()
    instrs += Loop(blockType, Some(label))
    val result = body(label)
    instrs += End
    result
  }

  def loop[A](resultType: Type)(body: LabelID => A): A =
    loop(BlockType.ValueType(resultType))(body)

  def loop[A]()(body: LabelID => A): A =
    loop(BlockType.ValueType())(body)

  def loop[A](sig: FunctionType)(body: LabelID => A): A =
    loop(sigToBlockType(sig))(body)

  def loop[A](resultTypes: List[Type])(body: LabelID => A): A =
    loop(FunctionType(Nil, resultTypes))(body)

  def whileLoop()(cond: => Unit)(body: => Unit): Unit = {
    loop() { loopLabel =>
      cond
      ifThen() {
        body
        instrs += Br(loopLabel)
      }
    }
  }

  def tryTable[A](blockType: BlockType)(clauses: List[CatchClause])(body: => A): A = {
    instrs += TryTable(blockType, clauses)
    val result = body
    instrs += End
    result
  }

  def tryTable[A](resultType: Type)(clauses: List[CatchClause])(body: => A): A =
    tryTable(BlockType.ValueType(resultType))(clauses)(body)

  def tryTable[A]()(clauses: List[CatchClause])(body: => A): A =
    tryTable(BlockType.ValueType())(clauses)(body)

  def tryTable[A](sig: FunctionType)(clauses: List[CatchClause])(body: => A): A =
    tryTable(sigToBlockType(sig))(clauses)(body)

  def tryTable[A](resultTypes: List[Type])(clauses: List[CatchClause])(body: => A): A =
    tryTable(FunctionType(Nil, resultTypes))(clauses)(body)

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
}
