package inox.analysis

import inox.ast.BinaryOp
import inox.ir.*

import scala.annotation.tailrec

/** A set of live variables. */
type LiveSet = Set[LocalId]

/** Liveness analysis for Inox. */
object LiveAnalysis {

  /** Computes the liveness information for a function. */
  def apply(function: inox.ir.Function): IndexedSeq[LiveSet] =
    new LiveAnalysis()(IndexedSeq(Set(0)), function.body)
}

/** Liveness analysis for an Inox function. */
private class LiveAnalysis extends BackwardAnalysis[LiveSet] {
  override def whileInstr(
      states: IndexedSeq[LiveSet],
      cond: Operand,
      body: Block
  ): IndexedSeq[LiveSet] = {
    val condLive = cond.item.locals

    @tailrec
    def fixpoint(state: LiveSet): IndexedSeq[LiveSet] = {
      val states = apply(IndexedSeq(state), body)
      val nextState = condLive | state | states.head
      if (nextState == state) {
        states.init.prepended(nextState)
      } else {
        fixpoint(nextState)
      }
    }

    fixpoint(states.head) :++ states
  }

  override def ifInstr(
      states: IndexedSeq[LiveSet],
      cond: Operand,
      thn: Block,
      els: Block
  ): IndexedSeq[LiveSet] = {
    val thnStates = apply(IndexedSeq(states.head), thn)
    val elsStates = apply(IndexedSeq(states.head), els)
    val join = thnStates.head | elsStates.head
    IndexedSeq(cond.item.locals | join) :++ thnStates.init :++ elsStates.init :++ states
  }

  override def callInstr(
      states: IndexedSeq[LiveSet],
      target: Place,
      callee: Operand,
      args: IndexedSeq[Operand]
  ): IndexedSeq[LiveSet] = states.prepended(
    states.head - target.item.local | callee.item.locals | args.foldLeft(Set.empty) {
      (live, operand) => live | operand.item.locals
    }
  )

  override def borrowInstr(
      states: IndexedSeq[LiveSet],
      target: Place,
      mutable: Boolean,
      source: Place
  ): IndexedSeq[LiveSet] = states.prepended(states.head - target.item.local + source.item.local)

  override def assignInstr(
      states: IndexedSeq[LiveSet],
      target: Place,
      value: Operand
  ): IndexedSeq[LiveSet] = states.prepended(states.head - target.item.local | value.item.locals)

  override def binaryInstr(
      states: IndexedSeq[LiveSet],
      target: Place,
      op: BinaryOp,
      lhs: Operand,
      rhs: Operand
  ): IndexedSeq[LiveSet] =
    states.prepended(states.head - target.item.local | lhs.item.locals | rhs.item.locals)

  override def unaryInstr(
      states: IndexedSeq[LiveSet],
      target: Place,
      op: UnOp,
      operand: Operand
  ): IndexedSeq[LiveSet] = states.prepended(states.head - target.item.local | operand.item.locals)

  override def returnInstr(states: IndexedSeq[LiveSet]): IndexedSeq[LiveSet] =
    states.prepended(states.head + 0)
}
