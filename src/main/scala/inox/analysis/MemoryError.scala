package inox.analysis

import inox.{Name, Span}

/** A memory safety error. */
enum MemoryError {

  case InvalidBorrow(span: Span)
  case UnauthorisedAssignment(name: Name)
  case UnauthorisedBorrow(span: Span)
  case UnauthorisedReassignment(name: Name)
  case UninitializedVariable(name: Name)

  override def toString: String =
    this match {
      case InvalidBorrow(span) =>
        s"$span: Cannot borrow the expression as mutable because it is already borrowed elsewhere."
      case UnauthorisedAssignment(name) =>
        s"${name.span}: Cannot assign to '${name.item}' because it is borrowed."
      case UnauthorisedBorrow(span) =>
        s"$span: Cannot borrow the expression as mutable."
      case UnauthorisedReassignment(name) =>
        s"${name.span}: Cannot reassign to immutable variable '${name.item}'."
      case UninitializedVariable(name) =>
        s"${name.span}: Variable '${name.item} is used before it is initialized."
    }

}
