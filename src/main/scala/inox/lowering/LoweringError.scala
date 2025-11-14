package inox.lowering

import inox.ir.Type
import inox.util.{InoxError, Name, Span}

/** An AST to IR lowering error. */
enum LoweringError extends InoxError {
  case DuplicateOrigin(name: Name)
  case DuplicateParameter(name: Name)
  case InvalidCallee(ty: Type)
  case InvalidDeref(ty: Type)
  case UnassignableExpr(span: Span)
  case UndefinedName(name: Name)
  case UndefinedOrigin(name: Name)
  case UndefinedType(name: Name)

  override def toString: String = this match {
    case DuplicateOrigin(name) =>
      s"${name.span}: An origin with the name '${name.item}' already exists."
    case DuplicateParameter(name) =>
      s"${name.span}: A parameter with the name '${name.item}' already exists."
    case InvalidCallee(ty)      => s"${ty.value.span}: Cannot call a value of type '$ty'."
    case InvalidDeref(ty)       => s"${ty.value.span}: Cannot dereference a value of type '$ty'."
    case UnassignableExpr(span) =>
      s"$span: Cannot assign a value to an expression that is not a dereference or a variable."
    case UndefinedName(name)   => s"${name.span}: Undefined name '${name.item}'."
    case UndefinedOrigin(name) => s"${name.span}: Undefined origin '${name.item}'."
    case UndefinedType(name)   =>
      s"${name.span}: Cannot determine the type of '${name.item}' without an annotation or value."
  }
}
