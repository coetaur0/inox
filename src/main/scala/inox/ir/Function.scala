package inox.ir

import inox.Span

/** An IR function. */
case class Function(
    originCount: Int,
    paramCount: Int,
    locals: IndexedSeq[Local],
    body: Block
):
  /** The function's type. */
  def ty: Type =
    val params = locals.slice(1, paramCount + 1).map(_.ty)
    val result = locals(0).ty
    Type.Fn(
      params,
      result,
      Span(
        params.headOption.getOrElse(result).value.span.start,
        result.value.span.end
      )
    )

/** A function parameter or local variable declaration. */
case class Local(mutable: Boolean, ty: Type)
