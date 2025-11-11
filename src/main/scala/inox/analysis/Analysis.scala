package inox.analysis

import inox.ast.BinaryOp
import inox.ir.*

/** An abstract analysis on Inox's IR. */
trait Analysis[State] {

  /** Applies the analysis on a block of instructions. */
  def apply(states: IndexedSeq[State], block: Block): IndexedSeq[State]

  /** Applies the analysis over an instruction. */
  def apply(states: IndexedSeq[State], instr: Instr): IndexedSeq[State] = instr match {
    case Instr.While(cond, body)               => whileInstr(states, cond, body)
    case Instr.If(cond, thn, els)              => ifInstr(states, cond, thn, els)
    case Instr.Call(target, callee, args)      => callInstr(states, target, callee, args)
    case Instr.Borrow(target, mutable, source) => borrowInstr(states, target, mutable, source)
    case Instr.Assign(target, value)           => assignInstr(states, target, value)
    case Instr.Binary(target, op, lhs, rhs)    => binaryInstr(states, target, op, lhs, rhs)
    case Instr.Unary(target, op, operand)      => unaryInstr(states, target, op, operand)
    case Instr.Return                          => returnInstr(states)
  }

  /** Applies the analysis on a while instruction. */
  def whileInstr(states: IndexedSeq[State], cond: Operand, body: Block): IndexedSeq[State]

  /** Applies the analysis on an if instruction. */
  def ifInstr(states: IndexedSeq[State], cond: Operand, thn: Block, els: Block): IndexedSeq[State]

  /** Applies the analysis on a call instruction. */
  def callInstr(
      states: IndexedSeq[State],
      target: Place,
      callee: Operand,
      args: IndexedSeq[Operand]
  ): IndexedSeq[State]

  /** Applies the analysis on a borrow instruction. */
  def borrowInstr(
      states: IndexedSeq[State],
      target: Place,
      mutable: Boolean,
      source: Place
  ): IndexedSeq[State]

  /** Applies the analysis on an assignment instruction. */
  def assignInstr(states: IndexedSeq[State], target: Place, value: Operand): IndexedSeq[State]

  /** Applies the analysis on a binary instruction. */
  def binaryInstr(
      states: IndexedSeq[State],
      target: Place,
      op: BinaryOp,
      lhs: Operand,
      rhs: Operand
  ): IndexedSeq[State]

  /** Applies the analysis on a unary instruction. */
  def unaryInstr(
      states: IndexedSeq[State],
      target: Place,
      op: UnOp,
      operand: Operand
  ): IndexedSeq[State]

  /** Applies the analysis on a return instruction. */
  def returnInstr(states: IndexedSeq[State]): IndexedSeq[State]
}

/** An abstract forward analysis on Inox's IR. */
trait ForwardAnalysis[State] extends Analysis[State] {
  def apply(states: IndexedSeq[State], block: Block): IndexedSeq[State] = block.foldLeft(states) {
    (acc, instr) => apply(acc, instr)
  }
}

/** An abstract backward analysis on Inox's IR. */
trait BackwardAnalysis[State] extends Analysis[State] {
  def apply(states: IndexedSeq[State], block: Block): IndexedSeq[State] = block.foldRight(states) {
    (instr, acc) => apply(acc, instr)
  }
}
