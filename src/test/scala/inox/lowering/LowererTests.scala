package inox.lowering

import inox.ir.Type
import inox.lowering.LowerError.UndefinedType
import org.scalatest.funsuite.AnyFunSuite
import inox.{Location, Result, Span, Spanned}
import inox.parsing.Parser

class LowererTests extends AnyFunSuite:
  test("Function declarations should be lowered properly") {
    checkOk("fn f<'a>(r: &'a mut i32) -> i32 { *r } fn main() { f::<'_>(&42) }")
    checkOk(
      "fn f<'a, 'b>(x: &'a i32, f: fn(&'a i32, &'a i32) -> &'a i32) -> &'a i32 { f::<'a>(x) }"
    )

    checkError(
      "fn f<'a, 'a>() {}",
      IndexedSeq(
        LowerError.DuplicateOrigin(
          Spanned("'a", Span(Location(1, 10, 9), Location(1, 12, 11)))
        )
      )
    )
    checkError(
      "fn f(x: i32, x: i32) {}",
      IndexedSeq(
        LowerError.DuplicateParameter(
          Spanned("x", Span(Location(1, 14, 13), Location(1, 15, 14)))
        )
      )
    )
  }

  test("Statement should be lowered properly") {
    checkOk("fn f() -> i32 { let x: i32; x = 42; g(); return x; x } fn g() {}")
    checkOk("fn loop() { let mut cond = true; while cond { cond = false; } }")

    checkError(
      "fn main() { let x; }",
      IndexedSeq(
        UndefinedType(
          Spanned("x", Span(Location(1, 17, 16), Location(1, 18, 17)))
        )
      )
    )
    checkError(
      "fn main() { 3 = 4; }",
      IndexedSeq(
        LowerError.UnassignableExpr(
          Span(Location(1, 13, 12), Location(1, 14, 13))
        )
      )
    )
  }

  test("Expressions should be lowered properly") {
    checkOk(
      "fn main() { let x = 42; let r = &x; if g() { *r } else { 3 }; } fn g() -> bool { true }"
    )

    checkError(
      "fn main() { 3(); }",
      IndexedSeq(
        LowerError.InvalidCallee(
          Type.I32(Span(Location(1, 13, 12), Location(1, 14, 13)))
        )
      )
    )
    checkError(
      "fn main() { *true; }",
      IndexedSeq(
        LowerError.InvalidDeref(
          Type.Bool(Span(Location(1, 14, 13), Location(1, 18, 17)))
        )
      )
    )
    checkError(
      "fn main() { x; }",
      IndexedSeq(
        LowerError.UndefinedName(
          Spanned("x", Span(Location(1, 13, 12), Location(1, 14, 13)))
        )
      )
    )
    checkError(
      "fn f<'a>() {} fn main() { f::<'b>; }",
      IndexedSeq(
        LowerError.UndefinedOrigin(
          Spanned("'b", Span(Location(1, 31, 30), Location(1, 33, 32)))
        )
      )
    )
  }

  test("Type expressions should be lowered properly") {
    checkOk(
      "fn f<'a, 'b>(x: &'a mut i32, f: fn(&'b i32) -> i32, b: bool) -> bool { true }"
    )

    checkError(
      "fn f(x: &'a i32) {}",
      IndexedSeq(
        LowerError.UndefinedOrigin(
          Spanned("'a", Span(Location(1, 10, 9), Location(1, 12, 11)))
        )
      )
    )
  }

  /** Checks that lowering the declarations in a source string succeeds. */
  private def checkOk(source: String): Unit =
    Parser.moduleDecl(source) match
      case Result.Success(ast) =>
        Lowerer.module(ast) match
          case Result.Success(ir)     => assert(true)
          case Result.Failure(errors) =>
            assert(
              false,
              s"Unexpected lowering errors in the input string: ${errors.mkString("\n")}."
            )
      case Result.Failure(errors) =>
        assert(
          false,
          s"Unexpected syntax errors in the input string: ${errors.mkString("\n")}."
        )

  /** Checks that lowering the declarations in a source string returns an
    * `expected` sequence of lowering errors.
    */
  private def checkError(
      source: String,
      expected: IndexedSeq[LowerError]
  ): Unit =
    Parser.moduleDecl(source) match
      case Result.Success(ast) =>
        Lowerer.module(ast) match
          case Result.Success(ir) =>
            assert(false, "Expected lowering errors in the input string.")
          case Result.Failure(errors) => assert(errors == expected)
      case Result.Failure(errors) =>
        assert(
          false,
          s"Unexpected syntax errors in the input string: ${errors.mkString("\n")}."
        )
