package inox.ast

import inox.{Name, Span}

/** A function declaration. */
case class FnDecl(
    origins: IndexedSeq[Name],
    parameters: IndexedSeq[ParamDecl],
    result: TypeExpr,
    body: BlockExpr
) {

  /** The function's type. */
  def ty: TypeExpr =
    TypeExpr.Fn(
      parameters.map(_.ty),
      result,
      Span(
        parameters.headOption.map(_.ty).getOrElse(result).span.start,
        result.span.end
      )
    )
}

/** A function parameter. */
case class ParamDecl(mutable: Boolean, name: Name, ty: TypeExpr)
