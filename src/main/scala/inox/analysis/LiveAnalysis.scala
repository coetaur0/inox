package inox.analysis

import scala.annotation.tailrec
import inox.ast.BinaryOp
import inox.ir.{Block, Instr, Local, LocalId, Operand, Place, UnOp}

/** A set of live variables. */
type LiveSet = Set[LocalId]

/** Liveness analysis for Inox. */
object LiveAnalysis {

  /** Computes the liveness information for a function. */
  def apply(function: inox.ir.Function): IndexedSeq[LiveSet] =
    new LiveAnalysis()(Set(0), function.body) :+ Set(0)

}

/** Liveness analysis for an Inox function. */
private class LiveAnalysis extends BackwardAnalysis[LiveSet] {

  override def whileInstr(
      state: LiveSet,
      cond: Operand,
      body: Block
  ): IndexedSeq[LiveSet] = {
    val condLive = cond.item.locals

    @tailrec
    def fixpoint(state: LiveSet): IndexedSeq[LiveSet] = {
      val states = apply(state, body)
      val nextState = condLive | state | states.headOption.getOrElse(Set.empty)
      if nextState == state then states.prepended(state)
      else fixpoint(nextState)
    }

    fixpoint(state)
  }

  override def ifInstr(
      state: LiveSet,
      cond: Operand,
      thn: Block,
      els: Block
  ): IndexedSeq[LiveSet] = {
    val thnStates = apply(state, thn)
    val elsStates = apply(state, els)
    val join = thnStates.headOption.getOrElse(Set.empty)
      | elsStates.headOption.getOrElse(Set.empty)
    IndexedSeq(cond.item.locals | join) :++ thnStates :++ elsStates
  }

  override def callInstr(
      state: LiveSet,
      target: Place,
      callee: Operand,
      args: IndexedSeq[Operand]
  ): IndexedSeq[LiveSet] =
    IndexedSeq(
      state - target.item.local |
        callee.item.locals |
        args.foldLeft(Set.empty) { (live, operand) =>
          live | operand.item.locals
        }
    )

  override def borrowInstr(
      state: LiveSet,
      target: Place,
      mutable: Boolean,
      source: Place
  ): IndexedSeq[LiveSet] =
    IndexedSeq(state - target.item.local + source.item.local)

  override def assignInstr(
      state: LiveSet,
      target: Place,
      value: Operand
  ): IndexedSeq[LiveSet] =
    IndexedSeq(state - target.item.local | value.item.locals)

  override def binaryInstr(
      state: LiveSet,
      target: Place,
      op: BinaryOp,
      lhs: Operand,
      rhs: Operand
  ): IndexedSeq[LiveSet] =
    IndexedSeq(state - target.item.local | lhs.item.locals | rhs.item.locals)

  override def unaryInstr(
      state: LiveSet,
      target: Place,
      op: UnOp,
      operand: Operand
  ): IndexedSeq[LiveSet] =
    IndexedSeq(state - target.item.local | operand.item.locals)

  override def returnInstr(state: LiveSet): IndexedSeq[LiveSet] =
    IndexedSeq(state + 0)

}
