package inox.borrowing

import scala.annotation.tailrec
import inox.ir.{Block, Instr, LocalId, Operand}

/** A set of live variables (local ids). */
type LiveSet = Set[LocalId]

/** Liveness analysis for Inox. */
object LiveAnalysis {

  /** Computes the sets of live-before variables for a block of instructions,
    * given a set of variables that are live after it.
    */
  def live(after: LiveSet, block: Block): IndexedSeq[LiveSet] = {
    var newAfter = after
    block.foldRight(IndexedSeq.empty) { (instr, seq) =>
      val liveSets = liveInstr(newAfter, instr)
      liveSets.headOption
        .map { set =>
          newAfter = set
          liveSets :++ seq
        }
        .getOrElse(seq)
    }
  }

  /** Computes the sets of live-before variables for an instruction, given a set
    * of variables that are live after it.
    */
  private def liveInstr(after: LiveSet, instr: Instr): IndexedSeq[LiveSet] =
    instr match {
      case Instr.While(cond, body)          => liveWhile(after, cond, body)
      case Instr.If(cond, thn, els)         => liveIf(after, cond, thn, els)
      case Instr.Call(target, callee, args) =>
        IndexedSeq(
          after - target.item.local
            | callee.item.locals
            | args.foldLeft(Set.empty)((ids, operand) =>
              ids | operand.item.locals
            )
        )
      case Instr.Borrow(target, _, source) =>
        IndexedSeq(after - target.item.local + source.item.local)
      case Instr.Assign(target, value) =>
        IndexedSeq(after - target.item.local | value.item.locals)
      case Instr.Binary(target, _, lhs, rhs) =>
        IndexedSeq(
          after - target.item.local | lhs.item.locals | rhs.item.locals
        )
      case Instr.Unary(target, _, operand) =>
        IndexedSeq(after - target.item.local | operand.item.locals)
      case Instr.Return => IndexedSeq(after + 0)
    }

  /** Computes the set of live-before variables for a while instruction, given a
    * set of variables that are live after it.
    */
  private def liveWhile(
      after: LiveSet,
      cond: Operand,
      body: Block
  ): IndexedSeq[LiveSet] = {
    val liveCond = cond.item.locals

    @tailrec
    def fixpoint(after: LiveSet): IndexedSeq[LiveSet] = {
      val liveBody = live(after, body)
      val before = liveCond | after | liveBody.headOption.getOrElse(Set.empty)
      if before == after then IndexedSeq(before) :++ liveBody
      else fixpoint(before)
    }

    fixpoint(after)
  }

  /** Computes the set of live-before variables for an if instruction, given a
    * set of variables that are live after it.
    */
  private def liveIf(
      after: LiveSet,
      cond: Operand,
      thn: Block,
      els: Block
  ): IndexedSeq[LiveSet] = {
    val liveThen = live(after, thn)
    val liveElse = live(after, els)
    val join =
      liveThen.headOption.getOrElse(Set.empty)
        | liveElse.headOption.getOrElse(Set.empty)
    IndexedSeq(cond.item.locals | join) :++ liveThen :++ liveElse
  }
}
