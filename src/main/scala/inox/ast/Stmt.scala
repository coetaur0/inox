package inox.ast

import inox.{Span, Spanned}

/** A statement. */
type Stmt = Spanned[StmtKind]

object Stmt:
  def While(cond: inox.ast.Expr, body: Spanned[Block], span: Span): Stmt =
    Spanned(StmtKind.While(cond, body), span)

  def Let(
      mutable: Boolean,
      name: Name,
      ty: Option[Type],
      value: Option[inox.ast.Expr],
      span: Span
  ): Stmt =
    Spanned(StmtKind.Let(mutable, name, ty, value), span)

  def Assign(lhs: inox.ast.Expr, rhs: inox.ast.Expr, span: Span): Stmt =
    Spanned(StmtKind.Assign(lhs, rhs), span)

  def Return(expr: inox.ast.Expr, span: Span): Stmt =
    Spanned(StmtKind.Return(expr), span)

  def Expr(kind: ExprKind, span: Span): Stmt =
    Spanned(StmtKind.Expr(kind), span)

/** A statement kind. */
enum StmtKind:
  case While(cond: inox.ast.Expr, body: Spanned[Block])
  case Let(
      mutable: Boolean,
      name: Name,
      ty: Option[Type],
      value: Option[inox.ast.Expr]
  )
  case Assign(lhs: inox.ast.Expr, rhs: inox.ast.Expr)
  case Return(expr: inox.ast.Expr)
  case Expr(kind: ExprKind)
