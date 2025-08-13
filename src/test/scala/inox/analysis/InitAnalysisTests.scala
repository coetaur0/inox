package inox.analysis

import org.scalatest.funsuite.AnyFunSuite
import inox.Result
import inox.lowering.Lowerer
import inox.parsing.Parser

class InitAnalysisTests extends AnyFunSuite {

  import InitState.*

  test("Initialisation analysis should be correctly computed") {
    check(
      """fn main() {
        | let x: i32;
        | if true {
        |   x = 42;
        | }
        |}""".stripMargin,
      IndexedSeq(
        InitMap(IndexedSeq(Uninitialized, Uninitialized, Uninitialized)),
        InitMap(IndexedSeq(Uninitialized, Initialized, Uninitialized)),
        InitMap(IndexedSeq(Uninitialized, Initialized, Initialized)),
        InitMap(IndexedSeq(Uninitialized, Uninitialized, Initialized)),
        InitMap(IndexedSeq(Uninitialized, MaybeInitialized, Initialized)),
        InitMap(IndexedSeq(Initialized, MaybeInitialized, Initialized))
      )
    )
    check(
      """fn main(x: i32) {
        |  let y: i32;
        |  let z: i32;
        |  y = x;
        |  z = y;
        |}
        |""".stripMargin,
      IndexedSeq(
        InitMap(
          IndexedSeq(Uninitialized, Initialized, Uninitialized, Uninitialized)
        ),
        InitMap(
          IndexedSeq(Uninitialized, Initialized, Initialized, Uninitialized)
        ),
        InitMap(
          IndexedSeq(Uninitialized, Initialized, Initialized, Initialized)
        ),
        InitMap(
          IndexedSeq(Initialized, Initialized, Initialized, Initialized)
        )
      )
    )
  }

  /** Checks that the result of initialisation analysis on the declaration in a
    * source string returns an `expected` sequence of initialisation maps.
    */
  private def check(source: String, expected: IndexedSeq[InitMap]): Unit =
    Parser.parseModule(source) match {
      case Result.Success(ast) =>
        Lowerer.lowerModule(ast) match {
          case Result.Success(ir) =>
            val (locals, aliases) = AliasAnalysis(ir, ir("main"))
            assert(InitAnalysis(locals, aliases, ir("main")) == expected)
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

}
