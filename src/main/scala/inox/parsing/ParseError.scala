package inox.parsing

import inox.util.{InoxError, Name, Spanned}

/** A syntax error. */
enum ParseError extends InoxError {
  case DuplicateFunction(name: Name)
  case UnclosedDelimiter(open: String, expected: String, found: Spanned[String])
  case UnexpectedSymbol(expected: String, found: Spanned[String])

  override def toString: String = this match {
    case DuplicateFunction(name) =>
      s"${name.span}: A function with the name '${name.item}' already exists."
    case UnclosedDelimiter(open, expected, found) =>
      s"${found.span}: Unclosed delimiter '$open', expected $expected but found '${found.item}' instead."
    case UnexpectedSymbol(expected, found) =>
      s"${found.span}: Unexpected symbol '${found.item}', expected $expected."
  }
}
