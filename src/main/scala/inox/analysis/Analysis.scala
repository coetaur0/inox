package inox.analysis

import inox.ast.BinaryOp
import inox.ir.{Block, Instr, Operand, Place, UnOp}

/** An abstract analysis on Inox's IR. */
trait Analysis[State] {

  /** Applies the analysis on a block of instructions. */
  def apply(state: State, block: Block): IndexedSeq[State]

  /** Applies the analysis over an instruction. */
  def apply(state: State, instr: Instr): IndexedSeq[State] =
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

  /** Applies the analysis on a while instruction. */
  def whileInstr(state: State, cond: Operand, body: Block): IndexedSeq[State]

  /** Applies the analysis on an if instruction. */
  def ifInstr(
      state: State,
      cond: Operand,
      thn: Block,
      els: Block
  ): IndexedSeq[State]

  /** Applies the analysis on a call instruction. */
  def callInstr(
      state: State,
      target: Place,
      callee: Operand,
      args: IndexedSeq[Operand]
  ): IndexedSeq[State]

  /** Applies the analysis on a borrow instruction. */
  def borrowInstr(
      state: State,
      target: Place,
      mutable: Boolean,
      source: Place
  ): IndexedSeq[State]

  /** Applies the analysis on an assignment instruction. */
  def assignInstr(
      state: State,
      target: Place,
      value: Operand
  ): IndexedSeq[State]

  /** Applies the analysis on a binary instruction. */
  def binaryInstr(
      state: State,
      target: Place,
      op: BinaryOp,
      lhs: Operand,
      rhs: Operand
  ): IndexedSeq[State]

  /** Applies the analysis on a unary instruction. */
  def unaryInstr(
      state: State,
      target: Place,
      op: UnOp,
      operand: Operand
  ): IndexedSeq[State]

  /** Applies the analysis on a return instruction. */
  def returnInstr(state: State): IndexedSeq[State]

}

/** An abstract forward analysis on Inox's IR. */
trait ForwardAnalysis[State] extends Analysis[State] {

  def apply(state: State, block: Block): IndexedSeq[State] = {
    var currentState = state
    block.foldLeft(IndexedSeq.empty) { (acc, instr) =>
      val result = apply(currentState, instr)
      currentState = result.lastOption.getOrElse(currentState)
      acc ++ result
    }
  }

}

/** An abstract backward analysis on Inox's IR. */
trait BackwardAnalysis[State] extends Analysis[State] {

  def apply(state: State, block: Block): IndexedSeq[State] = {
    var currentState = state
    block.foldRight(IndexedSeq.empty) { (instr, acc) =>
      val result = apply(currentState, instr)
      currentState = result.headOption.getOrElse(currentState)
      result ++ acc
    }
  }

}
