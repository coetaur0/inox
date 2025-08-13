package inox.analysis

import inox.ast.BinaryOp
import inox.ir.{Block, Instr, Local, Operand, Place, UnOp}

/** An abstract analysis over Inox's IR. */
abstract class Analysis[A](
    locals: IndexedSeq[Local],
    forward: Boolean = true
) {

  /** Applies the analysis over a block of instructions. */
  def apply(state: A, block: Block): IndexedSeq[A] = {
    var currentState = state
    if forward then
      block.foldLeft(IndexedSeq.empty) { (acc, instr) =>
        val result = apply(currentState, instr)
        currentState = result.lastOption.getOrElse(currentState)
        acc ++ result
      }
    else
      block.foldRight(IndexedSeq.empty) { (instr, acc) =>
        val result = apply(currentState, instr)
        currentState = result.headOption.getOrElse(currentState)
        result ++ acc
      }
  }

  /** Applies the analysis over an instruction. */
  def apply(state: A, instr: Instr): IndexedSeq[A] =
    instr match {
      case Instr.While(cond, body)          => whileInstr(state, cond, body)
      case Instr.If(cond, thn, els)         => ifInstr(state, cond, thn, els)
      case Instr.Call(target, callee, args) =>
        callInstr(state, target, callee, args)
      case Instr.Borrow(target, mutable, source) =>
        borrowInstr(state, target, mutable, source)
      case Instr.Assign(target, value) => assignInstr(state, target, value)
      case Instr.Binary(target, op, lhs, rhs) =>
        binaryInstr(state, target, op, lhs, rhs)
      case Instr.Unary(target, op, operand) =>
        unaryInstr(state, target, op, operand)
      case Instr.Return => returnInstr(state)
    }

  /** Applies the analysis over a while instruction. */
  def whileInstr(state: A, cond: Operand, body: Block): IndexedSeq[A]

  /** Applies the analysis over an if instruction. */
  def ifInstr(state: A, cond: Operand, thn: Block, els: Block): IndexedSeq[A]

  /** Applies the analysis over a call instruction. */
  def callInstr(
      state: A,
      target: Place,
      callee: Operand,
      args: IndexedSeq[Operand]
  ): IndexedSeq[A]

  /** Applies the analysis over a borrow instruction. */
  def borrowInstr(
      state: A,
      target: Place,
      mutable: Boolean,
      source: Place
  ): IndexedSeq[A]

  /** Applies the analysis over an assignment instruction. */
  def assignInstr(state: A, target: Place, value: Operand): IndexedSeq[A]

  /** Applies the analysis over a binary instruction. */
  def binaryInstr(
      state: A,
      target: Place,
      op: BinaryOp,
      lhs: Operand,
      rhs: Operand
  ): IndexedSeq[A]

  /** Applies the analysis over a unary instruction. */
  def unaryInstr(
      state: A,
      target: Place,
      op: UnOp,
      operand: Operand
  ): IndexedSeq[A]

  /** Applies the analysis over a return instruction. */
  def returnInstr(state: A): IndexedSeq[A]
}
