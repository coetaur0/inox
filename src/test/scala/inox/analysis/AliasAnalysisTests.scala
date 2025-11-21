package inox.analysis

import org.scalatest.funsuite.AnyFunSuite
import inox.lowering.Lowerer
import inox.parsing.Parser
import inox.util.Result

class AliasAnalysisTests extends AnyFunSuite {
  test("Alias analysis should be correctly computed") {
    check(
      """fn main() {
        |  let mut x: i32;
        |  let mut r: &mut i32;
        |  r = &mut x;
        |}""".stripMargin,
      IndexedSeq(
        AliasMap(
          IndexedSeq(
            (false, Set()),
            (false, Set()),
            (true, Set()),
            (true, Set())
          )
        ),
        AliasMap(
          IndexedSeq(
            (false, Set()),
            (false, Set()),
            (true, Set()),
            (true, Set(1))
          )
        ),
        AliasMap(
          IndexedSeq(
            (false, Set()),
            (false, Set()),
            (true, Set(1)),
            (true, Set(1))
          )
        ),
        AliasMap(
          IndexedSeq(
            (false, Set()),
            (false, Set()),
            (true, Set(1)),
            (true, Set(1))
          )
        )
      )
    )
    check(
      """fn main<'a>(x: &'a i32) {
        |  let r = &x;
        |}""".stripMargin,
      IndexedSeq(
        AliasMap(
          IndexedSeq(
            (false, Set()),
            (false, Set(4)),
            (false, Set()),
            (false, Set()),
            (false, Set())
          )
        ),
        AliasMap(
          IndexedSeq(
            (false, Set()),
            (false, Set(4)),
            (false, Set(1)),
            (false, Set()),
            (false, Set())
          )
        ),
        AliasMap(
          IndexedSeq(
            (false, Set()),
            (false, Set(4)),
            (false, Set(1)),
            (false, Set(1)),
            (false, Set())
          )
        ),
        AliasMap(
          IndexedSeq(
            (false, Set()),
            (false, Set(4)),
            (false, Set(1)),
            (false, Set(1)),
            (false, Set())
          )
        )
      )
    )
    check(
      """fn main() {
        |  let x = 42;
        |  let y = 1337;
        |  let r: &i32;
        |  if true {
        |    r = &x;
        |  } else {
        |    r = &y;
        |  };
        |}""".stripMargin,
      IndexedSeq(
        AliasMap(
          IndexedSeq(
            (false, Set()),
            (false, Set()),
            (false, Set()),
            (false, Set()),
            (false, Set()),
            (false, Set()),
            (false, Set())
          )
        ),
        AliasMap(
          IndexedSeq(
            (false, Set()),
            (false, Set()),
            (false, Set()),
            (false, Set()),
            (false, Set()),
            (false, Set()),
            (false, Set())
          )
        ),
        AliasMap(
          IndexedSeq(
            (false, Set()),
            (false, Set()),
            (false, Set()),
            (false, Set()),
            (false, Set()),
            (false, Set()),
            (false, Set())
          )
        ),
        AliasMap(
          IndexedSeq(
            (false, Set()),
            (false, Set()),
            (false, Set()),
            (false, Set()),
            (false, Set(1)),
            (false, Set()),
            (false, Set())
          )
        ),
        AliasMap(
          IndexedSeq(
            (false, Set()),
            (false, Set()),
            (false, Set()),
            (false, Set(1)),
            (false, Set(1)),
            (false, Set()),
            (false, Set())
          )
        ),
        AliasMap(
          IndexedSeq(
            (false, Set()),
            (false, Set()),
            (false, Set()),
            (false, Set(1)),
            (false, Set(1)),
            (false, Set()),
            (false, Set())
          )
        ),
        AliasMap(
          IndexedSeq(
            (false, Set()),
            (false, Set()),
            (false, Set()),
            (false, Set()),
            (false, Set()),
            (false, Set(2)),
            (false, Set())
          )
        ),
        AliasMap(
          IndexedSeq(
            (false, Set()),
            (false, Set()),
            (false, Set()),
            (false, Set(2)),
            (false, Set()),
            (false, Set(2)),
            (false, Set())
          )
        ),
        AliasMap(
          IndexedSeq(
            (false, Set()),
            (false, Set()),
            (false, Set()),
            (false, Set(2)),
            (false, Set()),
            (false, Set(2)),
            (false, Set())
          )
        ),
        AliasMap(
          IndexedSeq(
            (false, Set()),
            (false, Set()),
            (false, Set()),
            (false, Set(1, 2)),
            (false, Set(1)),
            (false, Set(2)),
            (false, Set())
          )
        ),
        AliasMap(
          IndexedSeq(
            (false, Set()),
            (false, Set()),
            (false, Set()),
            (false, Set(1, 2)),
            (false, Set(1)),
            (false, Set(2)),
            (false, Set())
          )
        )
      )
    )
  }

  /** Checks that the result of alias analysis on the declaration in a source string returns an
    * `expected` sequence of alias maps.
    */
  private def check(source: String, expected: IndexedSeq[AliasMap]): Unit =
    Parser.parseModule(source) match {
      case Result.Success(ast) =>
        Lowerer(ast) match {
          case Result.Success(ir)     => assert(AliasAnalysis(ir, ir("main"))._2 == expected)
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
