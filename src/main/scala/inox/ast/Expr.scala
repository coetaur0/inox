package inox.ast

import inox.{Span, Spanned}

/** An expression. */
type Expr = Spanned[ExprKind]

object Expr {
  def Block(body: Block, span: Span): Expr =
    Spanned(ExprKind.Block(body), span)

  def If(cond: Expr, thn: Spanned[Block], els: Expr, span: Span): Expr =
    Spanned(ExprKind.If(cond, thn, els), span)

  def Call(callee: Expr, args: Seq[Expr], span: Span): Expr =
    Spanned(ExprKind.Call(callee, args), span)

  def Borrow(mutable: Boolean, expr: Expr, span: Span): Expr =
    Spanned(ExprKind.Borrow(mutable, expr), span)

  def Binary(op: BinaryOp, lhs: Expr, rhs: Expr, span: Span): Expr =
    Spanned(ExprKind.Binary(op, lhs, rhs), span)

  def Unary(op: UnaryOp, expr: Expr, span: Span): Expr =
    Spanned(ExprKind.Unary(op, expr), span)

  def Var(name: Name, origins: Seq[Option[Name]], span: Span): Expr =
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
  case Block(body: inox.ast.Block)
  case If(cond: Expr, thn: Spanned[inox.ast.Block], els: Expr)
  case Call(callee: Expr, args: Seq[Expr])
  case Borrow(mutable: Boolean, expr: Expr)
  case Binary(op: BinaryOp, lhs: Expr, rhs: Expr)
  case Unary(op: UnaryOp, expr: Expr)
  case Var(name: Name, origins: Seq[Option[Name]])
  case IntLit(value: Int)
  case BoolLit(value: Boolean)
  case Unit
}

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
