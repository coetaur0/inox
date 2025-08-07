package inox.ast

import inox.{Name, Span, Spanned}

/** An expression. */
type Expr = Spanned[ExprKind]

/** An expression. */
object Expr {
  def Block(body: BlockExpr, span: Span): Expr =
    Spanned(ExprKind.Block(body), span)

  def If(cond: Expr, thn: Spanned[BlockExpr], els: Expr, span: Span): Expr =
    Spanned(ExprKind.If(cond, thn, els), span)

  def Call(callee: Expr, args: IndexedSeq[Expr], span: Span): Expr =
    Spanned(ExprKind.Call(callee, args), span)

  def Borrow(mutable: Boolean, expr: Expr, span: Span): Expr =
    Spanned(ExprKind.Borrow(mutable, expr), span)

  def Binary(op: BinaryOp, lhs: Expr, rhs: Expr, span: Span): Expr =
    Spanned(ExprKind.Binary(op, lhs, rhs), span)

  def Unary(op: UnaryOp, expr: Expr, span: Span): Expr =
    Spanned(ExprKind.Unary(op, expr), span)

  def Var(name: Name, origins: IndexedSeq[Option[Name]], span: Span): Expr =
    Spanned(ExprKind.Var(name, origins), span)

  def IntLit(value: Int, span: Span): Expr =
    Spanned(ExprKind.IntLit(value), span)

  def BoolLit(value: Boolean, span: Span): Expr =
    Spanned(ExprKind.BoolLit(value), span)

  def Unit(span: Span): Expr =
    Spanned(ExprKind.Unit, span)
}

/** An expression kind. */
enum ExprKind {
  case Block(body: BlockExpr)
  case If(cond: Expr, thn: Spanned[BlockExpr], els: Expr)
  case Call(callee: Expr, args: IndexedSeq[Expr])
  case Borrow(mutable: Boolean, expr: Expr)
  case Binary(op: BinaryOp, lhs: Expr, rhs: Expr)
  case Unary(op: UnaryOp, expr: Expr)
  case Var(name: Name, origins: IndexedSeq[Option[Name]])
  case IntLit(value: Int)
  case BoolLit(value: Boolean)
  case Unit
}

/** A block expression. */
case class BlockExpr(stmts: IndexedSeq[Stmt], result: Expr)

/** A binary operator. */
enum BinaryOp {
  case And
  case Or
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
}

/** A unary operator. */
enum UnaryOp {
  case Deref
  case Not
  case Neg
}
