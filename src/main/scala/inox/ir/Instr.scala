package inox.ir

import inox.{Span, Spanned}

/** An IR instruction. */
type Instr = Spanned[InstrKind]

object Instr:
  def While(cond: Operand, body: Block, span: Span): Instr =
    Spanned(InstrKind.While(cond, body), span)

  def If(cond: Operand, thn: Block, els: Block, span: Span): Instr =
    Spanned(InstrKind.If(cond, thn, els), span)

  def Call(
      target: Place,
      callee: Operand,
      args: IndexedSeq[Operand],
      span: Span
  ): Instr =
    Spanned(InstrKind.Call(target, callee, args), span)

  def Borrow(
      target: Place,
      mutable: Boolean,
      source: Place,
      span: Span
  ): Instr =
    Spanned(InstrKind.Borrow(target, mutable, source), span)

  def Assign(target: Place, value: Operand, span: Span): Instr =
    Spanned(InstrKind.Assign(target, value), span)

  def Binary(
      target: Place,
      op: BinaryOp,
      lhs: Operand,
      rhs: Operand,
      span: Span
  ): Instr =
    Spanned(InstrKind.Binary(target, op, lhs, rhs), span)

  def Unary(target: Place, op: UnaryOp, operand: Operand, span: Span): Instr =
    Spanned(InstrKind.Unary(target, op, operand), span)

  def Return(span: Span): Instr =
    Spanned(InstrKind.Return, span)

/** An IR instruction's kind. */
enum InstrKind:
  case While(cond: Operand, body: Block)
  case If(cond: Operand, thn: Block, els: Block)
  case Call(target: Place, callee: Operand, args: IndexedSeq[Operand])
  case Borrow(target: Place, mutable: Boolean, source: Place)
  case Assign(target: Place, value: Operand)
  case Binary(target: Place, op: BinaryOp, lhs: Operand, rhs: Operand)
  case Unary(target: Place, op: UnaryOp, operand: Operand)
  case Return

/** A binary operator. */
enum BinaryOp:
  case Eq
  case Neq
  case Lt
  case Le
  case Gt
  case Ge
  case Add
  case Sub
  case Mul
  case Div

/** A unary operator. */
enum UnaryOp:
  case Not
  case Neg
