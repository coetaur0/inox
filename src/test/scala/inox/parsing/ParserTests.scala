package inox.parsing

import org.scalatest.funsuite.AnyFunSuite

import inox.{Location, Result, Span, Spanned}

class ParserTests extends AnyFunSuite {
  test("Type expressions should be properly parsed") {
    checkOk("fn(i32, bool) -> i32")
    checkOk("&'a mut i32")
    checkOk("(i32)")
    checkOk("bool")

    checkError(
      "(i32",
      IndexedSeq(
        ParseError.UnclosedDelimiter(
          "(",
          "a ')'",
          Spanned("", Span(Location(1, 5, 4), Location(1, 5, 4)))
        )
      )
    )
    checkError(
      "fn -> i32",
      IndexedSeq(
        ParseError.UnexpectedSymbol(
          "a '('",
          Spanned("->", Span(Location(1, 4, 3), Location(1, 6, 5)))
        )
      )
    )
    checkError(
      "&true",
      IndexedSeq(
        ParseError.UnexpectedSymbol(
          "a type expression",
          Spanned("true", Span(Location(1, 2, 1), Location(1, 6, 5)))
        )
      )
    )
  }

  /** Checks that parsing some source string succeeds. */
  private def checkOk(source: String): Unit =
    assert(Parser.parse(source).isSuccess)

  /** Checks that parsing some source string returns an `expected` sequence of
    * parse errors.
    */
  private def checkError(
      source: String,
      expected: IndexedSeq[ParseError]
  ): Unit =
    Parser.parse(source) match {
      case Result.Success(result) =>
        assert(false, "Expected parse errors in the input string.")
      case Result.Failure(errors) =>
        assert(errors == expected)
    }
}
