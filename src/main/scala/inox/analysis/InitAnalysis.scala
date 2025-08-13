package inox.analysis

import inox.ast.BinaryOp
import inox.ir.{Block, Local, Operand, Place, UnOp}

/** Initialisation analysis for Inox. */
object InitAnalysis {

  /** Applies initialisation analysis on a function declaration. */
  def apply(
      locals: IndexedSeq[Local],
      aliases: IndexedSeq[AliasMap],
      function: inox.ir.Function
  ): IndexedSeq[InitMap] = {
    val initState =
      InitMap.init(locals, function.paramCount, function.locals.length)
    new InitAnalysis(aliases)(
      initState,
      function.body
    ).prepended(initState)
  }

}

/** Initialisation analysis for Inox. */
private class InitAnalysis(aliases: IndexedSeq[AliasMap])
    extends ForwardAnalysis[InitMap] {

  private var remaining = aliases

  override def whileInstr(
      state: InitMap,
      cond: Operand,
      body: Block
  ): IndexedSeq[InitMap] = {
    remaining = remaining.tail
    val states = apply(state, body)
    states :+ states.lastOption.getOrElse(state)
  }

  override def ifInstr(
      state: InitMap,
      cond: Operand,
      thn: Block,
      els: Block
  ): IndexedSeq[InitMap] = {
    remaining = remaining.tail
    val thnStates = apply(state, thn)
    val elsStates = apply(state, els)
    val join = thnStates.lastOption.getOrElse(state)
      | elsStates.lastOption.getOrElse(state)
    thnStates :++ elsStates :+ join
  }

  override def callInstr(
      state: InitMap,
      target: Place,
      callee: Operand,
      args: IndexedSeq[Operand]
  ): IndexedSeq[InitMap] =
    initTarget(state, target)

  override def borrowInstr(
      state: InitMap,
      target: Place,
      mutable: Boolean,
      source: Place
  ): IndexedSeq[InitMap] =
    initTarget(state, target)

  override def assignInstr(
      state: InitMap,
      target: Place,
      value: Operand
  ): IndexedSeq[InitMap] =
    initTarget(state, target)

  override def binaryInstr(
      state: InitMap,
      target: Place,
      op: BinaryOp,
      lhs: Operand,
      rhs: Operand
  ): IndexedSeq[InitMap] =
    initTarget(state, target)

  override def unaryInstr(
      state: InitMap,
      target: Place,
      op: UnOp,
      operand: Operand
  ): IndexedSeq[InitMap] =
    initTarget(state, target)

  override def returnInstr(state: InitMap): IndexedSeq[InitMap] = {
    remaining = remaining.tail
    IndexedSeq(state)
  }

  private def initTarget(state: InitMap, target: Place): IndexedSeq[InitMap] = {
    val ids = AliasAnalysis.placeAliases(remaining.head, target.item)
    remaining = remaining.tail
    IndexedSeq(state.updated(ids, InitState.Initialized))
  }

}
