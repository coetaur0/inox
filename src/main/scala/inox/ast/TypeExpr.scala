package inox.ast

import inox.{Name, Span, Spanned}

/** A type expression. */
type TypeExpr = Spanned[TypeExprKind]

object TypeExpr:
  def Fn(params: IndexedSeq[TypeExpr], result: TypeExpr, span: Span): TypeExpr =
    Spanned(TypeExprKind.Fn(params, result), span)

  def Ref(
      origin: Option[Name],
      mutable: Boolean,
      ty: TypeExpr,
      span: Span
  ): TypeExpr =
    Spanned(TypeExprKind.Ref(origin, mutable, ty), span)

  def I32(span: Span): TypeExpr = Spanned(TypeExprKind.I32, span)

  def Bool(span: Span): TypeExpr = Spanned(TypeExprKind.Bool, span)

  def Unit(span: Span): TypeExpr = Spanned(TypeExprKind.Unit, span)

/** A type expression kind. */
enum TypeExprKind:
  case Fn(params: IndexedSeq[TypeExpr], result: TypeExpr)
  case Ref(origin: Option[Name], mutable: Boolean, ty: TypeExpr)
  case I32
  case Bool
  case Unit
