package inox.evaluation

import inox.util.{Name, Span}

/** An interpreter runtime error. */
enum RuntimeError {
  case UndefinedFunction(name: Name)
  case InvalidOperation(span: Span)
  case InvalidDereference(span: Span)
  case NoMain

  override def toString: String = this match {
    case UndefinedFunction(name) =>
      s"${name.span}: Undefined function '${name.item}'."
    case InvalidOperation(span) =>
      s"$span: Cannot perform this operation on the given operands."
    case InvalidDereference(span) =>
      s"$span: Cannot dereference a value that is not a reference."
    case NoMain => "No main function found."
  }
}
