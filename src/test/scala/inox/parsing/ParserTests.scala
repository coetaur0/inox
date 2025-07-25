package inox.parsing

import org.scalatest.funsuite.AnyFunSuite
import inox.{Location, Result, Span, Spanned}

class ParserTests extends AnyFunSuite:
  test("Function declarations should be properly parsed") {
    checkOk(
      "fn f<'a>(r: &'a mut i32) -> i32 { *r } fn main() { f::<'_>(&42) }",
      Parser.moduleDecl
    )

    checkError(
      "fn f() {} fn f() {}",
      Parser.moduleDecl,
      IndexedSeq(
        ParseError.DuplicateFunction(
          Spanned("f", Span(Location(1, 14, 13), Location(1, 15, 14)))
        )
      )
    )
    checkError(
      "fn f() -> {}",
      Parser.moduleDecl,
      IndexedSeq(
        ParseError.UnexpectedSymbol(
          "a type expression",
          Spanned("{", Span(Location(1, 11, 10), Location(1, 12, 11)))
        )
      )
    )
  }

  test("Statement should be properly parsed") {
    checkOk("while b { x = f(); }", Parser.stmt)
    checkOk("let mut x: i32 = 42", Parser.stmt)
    checkOk("let b: bool", Parser.stmt)
    checkOk("let b = true", Parser.stmt)
    checkOk("x = 4", Parser.stmt)
    checkOk("f()", Parser.stmt)

    checkError(
      "while i32 {}",
      Parser.stmt,
      IndexedSeq(
        ParseError.UnexpectedSymbol(
          "an expression",
          Spanned("i32", Span(Location(1, 7, 6), Location(1, 10, 9)))
        )
      )
    )
    checkError(
      "while true x",
      Parser.stmt,
      IndexedSeq(
        ParseError.UnexpectedSymbol(
          "a '{'",
          Spanned("x", Span(Location(1, 12, 11), Location(1, 13, 12)))
        )
      )
    )
    checkError(
      "let x: 10",
      Parser.stmt,
      IndexedSeq(
        ParseError.UnexpectedSymbol(
          "a type expression",
          Spanned("10", Span(Location(1, 8, 7), Location(1, 10, 9)))
        )
      )
    )
    checkError(
      "x =",
      Parser.stmt,
      IndexedSeq(
        ParseError.UnexpectedSymbol(
          "an expression",
          Spanned("", Span(Location(1, 4, 3), Location(1, 4, 3)))
        )
      )
    )
    checkError(
      "return",
      Parser.stmt,
      IndexedSeq(
        ParseError.UnexpectedSymbol(
          "an expression",
          Spanned("", Span(Location(1, 7, 6), Location(1, 7, 6)))
        )
      )
    )
  }

  test("Expressions should be properly parsed") {
    checkOk("(x + 1) * 4 < 10 == c || d", Parser.expr)
    checkOk("!a && b", Parser.expr)
    checkOk("!!true", Parser.expr)
    checkOk("f(42, 19)(true)", Parser.expr)
    checkOk("if a { f() } else if b { g() } else { h() }", Parser.expr)
    checkOk("{}", Parser.expr)
    checkOk("&mut 42", Parser.expr)
    checkOk("x::<'a, '_>", Parser.expr)
    checkOk("x", Parser.expr)
    checkOk("42", Parser.expr)
    checkOk("true", Parser.expr)
    checkOk("false", Parser.expr)
    checkOk("()", Parser.expr)

    checkError(
      "(x + 1",
      Parser.expr,
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
      Parser.expr,
      IndexedSeq(
        ParseError.UnexpectedSymbol(
          "an expression",
          Spanned("i32", Span(Location(1, 2, 1), Location(1, 5, 4)))
        )
      )
    )
    checkError(
      "x::",
      Parser.expr,
      IndexedSeq(
        ParseError.UnexpectedSymbol(
          "a '<'",
          Spanned("", Span(Location(1, 4, 3), Location(1, 4, 3)))
        )
      )
    )
    checkError(
      "x::<'a",
      Parser.expr,
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
    checkOk("fn(i32, bool) -> i32", Parser.typeExpr)
    checkOk("&'a mut i32", Parser.typeExpr)
    checkOk("(i32)", Parser.typeExpr)
    checkOk("bool", Parser.typeExpr)

    checkError(
      "(i32",
      Parser.typeExpr,
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
      Parser.typeExpr,
      IndexedSeq(
        ParseError.UnexpectedSymbol(
          "a '('",
          Spanned("->", Span(Location(1, 4, 3), Location(1, 6, 5)))
        )
      )
    )
    checkError(
      "&true",
      Parser.typeExpr,
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
