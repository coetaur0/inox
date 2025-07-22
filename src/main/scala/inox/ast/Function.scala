package inox.ast

import inox.Span

/** A function declaration. */
case class Function(
    origins: IndexedSeq[Name],
    parameters: IndexedSeq[Parameter],
    result: Type,
    body: Block
) {

  /** The function's type. */
  def ty: Type = {
    val span =
      if parameters.isEmpty then result.span
      else Span(parameters.head.ty.span.start, result.span.end)

    Type.Fn(parameters.map(_.ty), result, span)
  }
}

/** A function parameter. */
case class Parameter(mutable: Boolean, name: Name, ty: Type)
