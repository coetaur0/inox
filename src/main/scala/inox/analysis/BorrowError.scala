package inox.analysis

import inox.util.{Name, Span}

/** A borrow checking error. */
enum BorrowError {
  case InvalidReborrow(span: Span)
  case UnauthorisedAssignment(name: Name)
  case UnauthorisedBorrow(span: Span)
  case UnauthorisedReassignment(name: Name)
  case UninitializedVariable(name: Name)

  override def toString: String = this match {
    case InvalidReborrow(span) =>
      s"$span: Cannot borrow the expression because it is already borrowed mutably elsewhere."
    case UnauthorisedAssignment(name) =>
      s"${name.span}: Cannot assign to variable '${name.item}' while it is being borrowed."
    case UnauthorisedBorrow(span)       => s"$span: Cannot borrow the expression as mutable."
    case UnauthorisedReassignment(name) =>
      s"${name.span}: Cannot reassign to immutable variable '${name.item}'."
    case UninitializedVariable(name) =>
      s"${name.span}: Variable '${name.item} is used before being potentially initialized."
  }
}
