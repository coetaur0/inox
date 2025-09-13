package inox.analysis

import inox.ast.BinaryOp
import inox.ir.*

/** Initialisation analysis for Inox. */
object InitAnalysis {

  /** Applies initialisation analysis on a function declaration. */
  def apply(
      locals: IndexedSeq[Local],
      function: inox.ir.Function
  ): IndexedSeq[InitMap] = {
    val initState =
      InitMap.init(locals, function.paramCount, function.locals.length)
    new InitAnalysis()(IndexedSeq(initState), function.body)
  }

}

/** Initialisation analysis for an Inox function. */
private class InitAnalysis extends ForwardAnalysis[InitMap] {

  override def whileInstr(
      states: IndexedSeq[InitMap],
      cond: Operand,
      body: Block
  ): IndexedSeq[InitMap] = {
    val state = states.last
    val bodyStates = apply(IndexedSeq(state), body)
    states.init :++ bodyStates.init :+ (bodyStates.last | state)
  }

  override def ifInstr(
      states: IndexedSeq[InitMap],
      cond: Operand,
      thn: Block,
      els: Block
  ): IndexedSeq[InitMap] = {
    val state = states.last
    val thnStates = apply(IndexedSeq(state), thn)
    val elsStates = apply(IndexedSeq(state), els)
    val join = thnStates.last | elsStates.last
    states.init :++ thnStates.init :++ elsStates.init :+ join
  }

  override def callInstr(
      states: IndexedSeq[InitMap],
      target: Place,
      callee: Operand,
      args: IndexedSeq[Operand]
  ): IndexedSeq[InitMap] =
    initTarget(states, target)

  override def borrowInstr(
      states: IndexedSeq[InitMap],
      target: Place,
      mutable: Boolean,
      source: Place
  ): IndexedSeq[InitMap] =
    initTarget(states, target)

  override def assignInstr(
      states: IndexedSeq[InitMap],
      target: Place,
      value: Operand
  ): IndexedSeq[InitMap] =
    initTarget(states, target)

  override def binaryInstr(
      states: IndexedSeq[InitMap],
      target: Place,
      op: BinaryOp,
      lhs: Operand,
      rhs: Operand
  ): IndexedSeq[InitMap] =
    initTarget(states, target)

  override def unaryInstr(
      states: IndexedSeq[InitMap],
      target: Place,
      op: UnOp,
      operand: Operand
  ): IndexedSeq[InitMap] =
    initTarget(states, target)

  override def returnInstr(states: IndexedSeq[InitMap]): IndexedSeq[InitMap] =
    states :+ states.last

  private def initTarget(
      states: IndexedSeq[InitMap],
      target: Place
  ): IndexedSeq[InitMap] = {
    val state = states.last
    states :+ (target.item match {
      case PlaceKind.Deref(place) => state
      case PlaceKind.Var(id)      => state.updated(id, InitState.Initialized)
    })
  }

}
