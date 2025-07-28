package inox.parsing

import org.scalatest.funsuite.AnyFunSuite
import inox.{Location, Result, Span, Spanned}

class ParserTests extends AnyFunSuite:
  test("Function declarations should be properly parsed") {
    checkOk(
      "fn f<'a>(r: &'a mut i32) -> i32 { *r } fn main() { f::<'_>(&42) }",
      Parser.parseModule
    )

    checkError(
      "fn f() {} fn f() {}",
      Parser.parseModule,
      IndexedSeq(
        ParseError.DuplicateFunction(
          Spanned("f", Span(Location(1, 14, 13), Location(1, 15, 14)))
        )
      )
    )
    checkError(
      "fn f() -> {}",
      Parser.parseModule,
      IndexedSeq(
        ParseError.UnexpectedSymbol(
          "a type expression",
          Spanned("{", Span(Location(1, 11, 10), Location(1, 12, 11)))
        )
      )
    )
  }

  test("Statement should be properly parsed") {
    checkOk("while b { x = f(); }", Parser.parseStmt)
    checkOk("let mut x: i32 = 42", Parser.parseStmt)
    checkOk("let b: bool", Parser.parseStmt)
    checkOk("let b = true", Parser.parseStmt)
    checkOk("x = 4", Parser.parseStmt)
    checkOk("f()", Parser.parseStmt)

    checkError(
      "while i32 {}",
      Parser.parseStmt,
      IndexedSeq(
        ParseError.UnexpectedSymbol(
          "an expression",
          Spanned("i32", Span(Location(1, 7, 6), Location(1, 10, 9)))
        )
      )
    )
    checkError(
      "while true x",
      Parser.parseStmt,
      IndexedSeq(
        ParseError.UnexpectedSymbol(
          "a '{'",
          Spanned("x", Span(Location(1, 12, 11), Location(1, 13, 12)))
        )
      )
    )
    checkError(
      "let x: 10",
      Parser.parseStmt,
      IndexedSeq(
        ParseError.UnexpectedSymbol(
          "a type expression",
          Spanned("10", Span(Location(1, 8, 7), Location(1, 10, 9)))
        )
      )
    )
    checkError(
      "x =",
      Parser.parseStmt,
      IndexedSeq(
        ParseError.UnexpectedSymbol(
          "an expression",
          Spanned("", Span(Location(1, 4, 3), Location(1, 4, 3)))
        )
      )
    )
    checkError(
      "return",
      Parser.parseStmt,
      IndexedSeq(
        ParseError.UnexpectedSymbol(
          "an expression",
          Spanned("", Span(Location(1, 7, 6), Location(1, 7, 6)))
        )
      )
    )
  }

  test("Expressions should be properly parsed") {
    checkOk("(x + 1) * 4 < 10 == c || d", Parser.parseExpr)
    checkOk("!a && b", Parser.parseExpr)
    checkOk("!!true", Parser.parseExpr)
    checkOk("f(42, 19)(true)", Parser.parseExpr)
    checkOk("if a { f() } else if b { g() } else { h() }", Parser.parseExpr)
    checkOk("{}", Parser.parseExpr)
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
    checkOk("fn(i32, bool) -> i32", Parser.parseTypeExpr)
    checkOk("&'a mut i32", Parser.parseTypeExpr)
    checkOk("(i32)", Parser.parseTypeExpr)
    checkOk("bool", Parser.parseTypeExpr)

    checkError(
      "(i32",
      Parser.parseTypeExpr,
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
      Parser.parseTypeExpr,
      IndexedSeq(
        ParseError.UnexpectedSymbol(
          "a '('",
          Spanned("->", Span(Location(1, 4, 3), Location(1, 6, 5)))
        )
      )
    )
    checkError(
      "&true",
      Parser.parseTypeExpr,
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
    parse(source) match
      case Result.Success(result) =>
        assert(false, "Expected parse errors in the input string.")
      case Result.Failure(errors) =>
        assert(errors == expected)
