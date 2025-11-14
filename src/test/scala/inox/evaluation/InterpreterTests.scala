package inox.evaluation

import inox.evaluation.Interpreter.Value
import inox.lowering.Lowerer
import inox.parsing.Parser
import inox.util.{Location, Result, Span, Spanned}
import org.scalatest.funsuite.AnyFunSuite

class InterpreterTests extends AnyFunSuite {
  test("The interpreter should return the expected results") {
    checkOk("fn main() -> i32 { 42 }", Value.I32(42))
    checkOk(
      "fn f(x: i32, y: i32) -> i32 { x + y } fn main() -> i32 { f(5, 5) }",
      Value.I32(10)
    )
    checkOk(
      "fn cmp(x: i32, y: i32) -> bool { x == y } fn main() -> bool { let a = 5 + 5; cmp(a, 10) }",
      Value.Bool(true)
    )
    checkOk(
      "fn f() -> i32 { 42 } fn main() -> fn() -> i32 { f }",
      Value.Function(Spanned("f", Span(Location(1, 49, 48), Location(1, 50, 49))))
    )
    checkOk(
      "fn f<'a>(r: &'a mut i32) -> i32 { *r } fn main() -> i32 { f::<'_>(&42) }",
      Value.I32(42)
    )
    checkOk(
      "fn add_refs<'a>(x: &'a i32, y: &'a i32) -> i32 { *x + *y } fn main() -> i32 { add_refs::<'_>(&40, &2) }",
      Value.I32(42)
    )

    checkError("fn f() {}", Seq(RuntimeError.NoMain))
    checkError("fn main(x: i32) {}", Seq(RuntimeError.NoMain))
    checkError(
      "fn main() -> i32 { true + 1 }",
      Seq(RuntimeError.InvalidOperation(Span(Location(1, 20, 19), Location(1, 28, 27))))
    )
  }

  /** Checks that interpreting the module in a source string returns the expected result. */
  private def checkOk(source: String, expected: Interpreter.Value): Unit =
    Parser.parseModule(source) match {
      case Result.Success(ast) =>
        Lowerer.lowerModule(ast) match {
          case Result.Success(ir) =>
            Interpreter(ir) match {
              case Result.Success(value)  => assert(value == expected)
              case Result.Failure(errors) =>
                assert(
                  false,
                  s"Unexpected runtime errors in the input string: ${errors.mkString("\n")}."
                )
            }
          case Result.Failure(errors) =>
            assert(
              false,
              s"Unexpected lowering errors in the input string: ${errors.mkString("\n")}."
            )
        }
      case Result.Failure(errors) =>
        assert(false, s"Unexpected syntax errors in the input string: ${errors.mkString("\n")}.")
    }

  private def checkError(source: String, expected: Seq[RuntimeError]): Unit =
    Parser.parseModule(source) match {
      case Result.Success(ast) =>
        Lowerer.lowerModule(ast) match {
          case Result.Success(ir) =>
            Interpreter(ir) match {
              case Result.Success(value) =>
                assert(false, "Expected runtime errors in the input string.")
              case Result.Failure(errors) => assert(errors == expected)
            }
          case Result.Failure(errors) =>
            assert(
              false,
              s"Unexpected lowering errors in the input string: ${errors.mkString("\n")}."
            )
        }
      case Result.Failure(errors) =>
        assert(false, s"Unexpected syntax errors in the input string: ${errors.mkString("\n")}.")
    }
}
