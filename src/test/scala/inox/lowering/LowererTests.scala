package inox.lowering

import org.scalatest.funsuite.AnyFunSuite
import inox.ir.Type
import inox.parsing.Parser
import inox.util.{Location, Result, Span, Spanned}

class LowererTests extends AnyFunSuite {
  test("Function declarations should be lowered properly") {
    checkOk("fn f<'a>(r: &'a mut i32) -> i32 { *r } fn main() { f::<'_>(&42) }")
    checkOk(
      "fn f<'a, 'b>(x: &'a i32, f: fn(&'a i32, &'a i32) -> &'a i32) -> &'a i32 { f::<'a>(x) }"
    )

    checkError(
      "fn f<'a, 'a>() {}",
      Seq(
        LoweringError.DuplicateOrigin(Spanned("'a", Span(Location(1, 10, 9), Location(1, 12, 11))))
      )
    )
    checkError(
      "fn f(x: i32, x: i32) {}",
      Seq(
        LoweringError.DuplicateParameter(
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
      Seq(
        LoweringError.UndefinedType(Spanned("x", Span(Location(1, 17, 16), Location(1, 18, 17))))
      )
    )
    checkError(
      "fn main() { 3 = 4; }",
      Seq(
        LoweringError.UnassignableExpr(Span(Location(1, 13, 12), Location(1, 14, 13)))
      )
    )
  }

  test("Expressions should be lowered properly") {
    checkOk(
      "fn main() { let x = 42; let r = &x; if g() { *r } else { 3 }; } fn g() -> bool { true }"
    )
    checkOk(
      "fn f() -> bool { let x = 3 + 1 * 4; if x == 7 { true } else { false } }"
    )

    checkError(
      "fn main() { 3(); }",
      Seq(
        LoweringError.InvalidCallee(Type.I32(Span(Location(1, 13, 12), Location(1, 14, 13))))
      )
    )
    checkError(
      "fn main() { *true; }",
      Seq(
        LoweringError.InvalidDeref(Type.Bool(Span(Location(1, 14, 13), Location(1, 18, 17))))
      )
    )
    checkError(
      "fn main() { x; }",
      Seq(
        LoweringError.UndefinedName(Spanned("x", Span(Location(1, 13, 12), Location(1, 14, 13))))
      )
    )
    checkError(
      "fn f<'a>() {} fn main() { f::<'b>; }",
      Seq(
        LoweringError.UndefinedOrigin(Spanned("'b", Span(Location(1, 31, 30), Location(1, 33, 32))))
      )
    )
  }

  test("Type expressions should be lowered properly") {
    checkOk(
      "fn f<'a, 'b>(x: &'a mut i32, f: fn(&'b i32) -> i32, b: bool) -> bool { true }"
    )

    checkError(
      "fn f(x: &'a i32) {}",
      Seq(
        LoweringError.UndefinedOrigin(Spanned("'a", Span(Location(1, 10, 9), Location(1, 12, 11))))
      )
    )
  }

  /** Checks that lowering the declarations in a source string succeeds. */
  private def checkOk(source: String): Unit = Parser.parseModule(source) match {
    case Result.Success(ast) =>
      Lowerer(ast) match
        case Result.Success(ir)     => assert(true)
        case Result.Failure(errors) =>
          assert(
            false,
            s"Unexpected lowering errors in the input string: ${errors.mkString("\n")}."
          )
    case Result.Failure(errors) =>
      assert(false, s"Unexpected syntax errors in the input string: ${errors.mkString("\n")}.")
  }

  /** Checks that lowering the declarations in a source string returns an `expected` sequence of
    * lowering errors.
    */
  private def checkError(source: String, expected: Seq[LoweringError]): Unit =
    Parser.parseModule(source) match {
      case Result.Success(ast) =>
        Lowerer(ast) match {
          case Result.Success(ir) => assert(false, "Expected lowering errors in the input string.")
          case Result.Failure(errors) => assert(errors == expected)
        }
      case Result.Failure(errors) =>
        assert(false, s"Unexpected syntax errors in the input string: ${errors.mkString("\n")}.")
    }
}
