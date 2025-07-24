package inox.ast

import inox.Span

/** A function declaration. */
case class FnDecl(
    origins: IndexedSeq[Name],
    parameters: IndexedSeq[ParamDecl],
    result: TypeExpr,
    body: BlockExpr
):
  /** The function's type. */
  def ty: TypeExpr =
    val span =
      if parameters.isEmpty then result.span
      else Span(parameters.head.ty.span.start, result.span.end)

    TypeExpr.Fn(parameters.map(_.ty), result, span)

/** A function parameter. */
case class ParamDecl(mutable: Boolean, name: Name, ty: TypeExpr)
