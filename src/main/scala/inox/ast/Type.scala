package inox.ast

import inox.{Span, Spanned}

/** A type expression. */
type Type = Spanned[TypeKind]

object Type {
  def Fn(params: Seq[Type], result: Type, span: Span): Type =
    Spanned(TypeKind.Fn(params, result), span)

  def Ref(origin: Option[Name], mutable: Boolean, ty: Type, span: Span): Type =
    Spanned(TypeKind.Ref(origin, mutable, ty), span)

  def I32(span: Span): Type = Spanned(TypeKind.I32, span)

  def Bool(span: Span): Type = Spanned(TypeKind.Bool, span)

  def Unit(span: Span): Type = Spanned(TypeKind.Unit, span)
}

/** A type expression kind. */
enum TypeKind {
  case Fn(params: Seq[Type], result: Type)
  case Ref(origin: Option[Name], mutable: Boolean, ty: Type)
  case I32
  case Bool
  case Unit
}
