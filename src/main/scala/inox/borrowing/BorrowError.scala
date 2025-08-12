package inox.borrowing

import inox.{Name, Span}

/** A borrow checking error. */
enum BorrowError {
  case InvalidBorrow(span: Span)
  case UnauthorisedBorrow(span: Span)
  case UninitializedVariable(name: Name)

  override def toString: String =
    this match {
      case InvalidBorrow(span) =>
        s"$span: Cannot borrow the expression as mutable because it is already borrowed elsewhere."
      case UnauthorisedBorrow(span) =>
        s"$span: Cannot borrow the expression as mutable."
      case UninitializedVariable(name) =>
        s"${name.span}: Variable '${name.item} is used before it is initialized."
    }
}
