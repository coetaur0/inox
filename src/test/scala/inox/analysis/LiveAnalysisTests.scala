package inox.analysis

import org.scalatest.funsuite.AnyFunSuite
import inox.Result
import inox.lowering.Lowerer
import inox.parsing.Parser

class LiveAnalysisTests extends AnyFunSuite:
  test("Live variables should be correctly computed") {
    check(
      "fn main() -> bool { let cond = true; let mut x: bool; while cond { cond = false; x = cond; }; x }",
      IndexedSeq(Set(2), Set(1, 2), Set.empty, Set(1), Set(2), Set(0))
    )

    check(
      "fn main() -> i32 { let mut x = 42; let c = true; let r: i32; if c { x = 10; r = 1; } else { c = true; r = x; }; r }",
      IndexedSeq(
        Set.empty,
        Set(1),
        Set(1, 2),
        Set.empty,
        Set.empty,
        Set(3),
        Set(1),
        Set(1),
        Set(3),
        Set(3),
        Set(0)
      )
    )
  }

  /** Checks that the result of liveness analysis on the declaration in a source
    * string returns an `expected` sequence of local id sets.
    */
  private def check(source: String, expected: IndexedSeq[LiveSet]): Unit =
    Parser.parseModule(source) match {
      case Result.Success(ast) =>
        Lowerer.lowerModule(ast) match {
          case Result.Success(ir) =>
            assert(LiveAnalysis(ir("main")) == expected)
          case Result.Failure(errors) =>
            assert(
              false,
              s"Unexpected lowering errors in the input string: ${errors.mkString("\n")}."
            )
        }
      case Result.Failure(errors) =>
        assert(
          false,
          s"Unexpected syntax errors in the input string: ${errors.mkString("\n")}."
        )
    }
