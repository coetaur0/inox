package inox.typing

import inox.ir.{Type, TypeKind}
import inox.util.{InoxError, Name, Span, Spanned}

/** A type error. */
enum TypeError extends InoxError {
  case IncompatibleTypes(found: Type, expected: Type)
  case InvalidArgNum(found: Spanned[Int], expected: Int)
  case InvalidArgType(found: Type, expected: Type)
  case InvalidCallee(ty: Type)
  case InvalidCondition(ty: Type)
  case InvalidDeref(ty: Type)
  case InvalidOperand(found: Type, expected: TypeKind)
  case InvalidOriginArgNum(name: Name, found: Int, expected: Int)
  case OriginNeeded(span: Span)

  override def toString: String = this match {
    case TypeError.IncompatibleTypes(found, expected) =>
      s"${found.value.span}: Expected a value of type '${expected.value.item}', but found type '${found.value.item}' instead."
    case TypeError.InvalidArgNum(found, expected) =>
      s"${found.span}: Expected $expected arguments, but found ${found.item} instead."
    case TypeError.InvalidArgType(found, expected) =>
      s"${found.value.span}: Expected an argument of type '${expected.value.item}', but found '${found.value.item}' instead."
    case TypeError.InvalidCallee(ty)    => s"${ty.value.span}: Cannot call a value of type '$ty'.'"
    case TypeError.InvalidCondition(ty) =>
      s"${ty.value.span}: Expected a boolean value, but found a value of type '${ty.value.item}'.'"
    case TypeError.InvalidDeref(ty) =>
      s"${ty.value.span}: Cannot dereference a value of type '${ty.value.item}'.'"
    case TypeError.InvalidOperand(found, expected) =>
      s"${found.value.span}: Expected an operand of type '$expected', but found '${found.value.item}' instead."
    case TypeError.InvalidOriginArgNum(name, found, expected) =>
      s"${name.span}: Expected $expected origin arguments for function '$name', but found $found instead."
    case TypeError.OriginNeeded(span) =>
      s"$span All references in function parameters and return types must be annotated with origins."
  }
}
