package inox.ast

import inox.{Name, Span, Spanned}

/** A statement. */
type Stmt = Spanned[StmtKind]

/** A statement. */
object Stmt {

  def While(cond: Expr, body: Spanned[BlockExpr], span: Span): Stmt =
    Spanned(StmtKind.While(cond, body), span)

  def Let(
      mutable: Boolean,
      name: Name,
      ty: Option[TypeExpr],
      value: Option[Expr],
      span: Span
  ): Stmt =
    Spanned(StmtKind.Let(mutable, name, ty, value), span)

  def Assign(lhs: Expr, rhs: Expr, span: Span): Stmt =
    Spanned(StmtKind.Assign(lhs, rhs), span)

  def Return(expr: Expr, span: Span): Stmt =
    Spanned(StmtKind.Return(expr), span)

  def ExprStmt(kind: ExprKind, span: Span): Stmt =
    Spanned(StmtKind.ExprStmt(kind), span)

}

/** A statement kind. */
enum StmtKind {

  case While(cond: Expr, body: Spanned[BlockExpr])
  case Let(
      mutable: Boolean,
      name: Name,
      ty: Option[TypeExpr],
      value: Option[Expr]
  )
  case Assign(lhs: Expr, rhs: Expr)
  case Return(expr: Expr)
  case ExprStmt(kind: ExprKind)

}
