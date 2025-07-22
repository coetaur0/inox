package inox.parsing

import org.scalatest.funsuite.AnyFunSuite

import inox.{Location, Result, Span, Spanned}

class ParserTests extends AnyFunSuite {
  test("Expressions should be properly parsed") {
    checkOk("(x + 1) * 4 < 10 == c || d", Parser.parseExpr)
    checkOk("!a && b", Parser.parseExpr)
    checkOk("!!true", Parser.parseExpr)
    checkOk("f(42, 19)(true)", Parser.parseExpr)
    checkOk("&mut 42", Parser.parseExpr)
    checkOk("x::<'a, '_>", Parser.parseExpr)
    checkOk("x", Parser.parseExpr)
    checkOk("42", Parser.parseExpr)
    checkOk("true", Parser.parseExpr)
    checkOk("false", Parser.parseExpr)
    checkOk("()", Parser.parseExpr)

    checkError(
      "(x + 1",
      Parser.parseExpr,
      IndexedSeq(
        ParseError.UnclosedDelimiter(
          "(",
          "a ')'",
          Spanned("", Span(Location(1, 7, 6), Location(1, 7, 6)))
        )
      )
    )
    checkError(
      "&i32",
      Parser.parseExpr,
      IndexedSeq(
        ParseError.UnexpectedSymbol(
          "an expression",
          Spanned("i32", Span(Location(1, 2, 1), Location(1, 5, 4)))
        )
      )
    )
    checkError(
      "x::",
      Parser.parseExpr,
      IndexedSeq(
        ParseError.UnexpectedSymbol(
          "a '<'",
          Spanned("", Span(Location(1, 4, 3), Location(1, 4, 3)))
        )
      )
    )
    checkError(
      "x::<'a",
      Parser.parseExpr,
      IndexedSeq(
        ParseError.UnclosedDelimiter(
          "<",
          "a '>'",
          Spanned("", Span(Location(1, 7, 6), Location(1, 7, 6)))
        )
      )
    )
  }

  test("Type expressions should be properly parsed") {
    checkOk("fn(i32, bool) -> i32", Parser.parseType)
    checkOk("&'a mut i32", Parser.parseType)
    checkOk("(i32)", Parser.parseType)
    checkOk("bool", Parser.parseType)

    checkError(
      "(i32",
      Parser.parseType,
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
      Parser.parseType,
      IndexedSeq(
        ParseError.UnexpectedSymbol(
          "a '('",
          Spanned("->", Span(Location(1, 4, 3), Location(1, 6, 5)))
        )
      )
    )
    checkError(
      "&true",
      Parser.parseType,
      IndexedSeq(
        ParseError.UnexpectedSymbol(
          "a type expression",
          Spanned("true", Span(Location(1, 2, 1), Location(1, 6, 5)))
        )
      )
    )
  }

  /** Checks that parsing some source string succeeds. */
  private def checkOk[A](
      source: String,
      parse: String => Result[A, ParseError]
  ): Unit =
    assert(parse(source).isSuccess)

  /** Checks that parsing some source string returns an `expected` sequence of
    * parse errors.
    */
  private def checkError[A](
      source: String,
      parse: String => Result[A, ParseError],
      expected: IndexedSeq[ParseError]
  ): Unit =
    parse(source) match {
      case Result.Success(result) =>
        assert(false, "Expected parse errors in the input string.")
      case Result.Failure(errors) =>
        assert(errors == expected)
    }
}
