package inox.ir

import inox.{Span, Spanned}
import inox.ast.BinaryOp

/** A block of instructions. */
type Block = IndexedSeq[Instr]

/** An IR instruction. */
enum Instr:
  case While(cond: Operand, body: Block)
  case If(cond: Operand, thn: Block, els: Block)
  case Call(target: Place, callee: Operand, args: IndexedSeq[Operand])
  case Borrow(target: Place, mutable: Boolean, source: Place)
  case Assign(target: Place, value: Operand)
  case Binary(target: Place, op: BinaryOp, lhs: Operand, rhs: Operand)
  case Unary(target: Place, op: UnOp, operand: Operand)
  case Return

/** A unary operator. */
enum UnOp:
  case Not
  case Neg
