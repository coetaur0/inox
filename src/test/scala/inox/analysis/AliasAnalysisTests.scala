package inox.analysis

import inox.lowering.Lowerer
import inox.parsing.Parser
import inox.util.Result
import org.scalatest.funsuite.AnyFunSuite

class AliasAnalysisTests extends AnyFunSuite {
  test("Alias analysis should be correctly computed") {
    check(
      """fn main() {
        |  let mut x: i32;
        |  let mut r: &mut i32;
        |  r = &mut x;
        |}""".stripMargin,
      IndexedSeq(
        Set(
          IndexedSeq(
            Alias.Undefined,
            Alias.Undefined,
            Alias.Undefined,
            Alias.Undefined
          )
        ),
        Set(
          IndexedSeq(
            Alias.Undefined,
            Alias.Undefined,
            Alias.Undefined,
            Alias.Variable(1)
          )
        ),
        Set(
          IndexedSeq(
            Alias.Undefined,
            Alias.Undefined,
            Alias.Variable(1),
            Alias.Variable(1)
          )
        ),
        Set(
          IndexedSeq(
            Alias.None,
            Alias.Undefined,
            Alias.Variable(1),
            Alias.Variable(1)
          )
        )
      )
    )
    check(
      """fn main<'a>(x: &'a i32) {
        |  let r = &x;
        |}""".stripMargin,
      IndexedSeq(
        Set(
          IndexedSeq(
            Alias.Undefined,
            Alias.Variable(4),
            Alias.Undefined,
            Alias.Undefined,
            Alias.None
          )
        ),
        Set(
          IndexedSeq(
            Alias.Undefined,
            Alias.Variable(4),
            Alias.Variable(1),
            Alias.Undefined,
            Alias.None
          )
        ),
        Set(
          IndexedSeq(
            Alias.Undefined,
            Alias.Variable(4),
            Alias.Variable(1),
            Alias.Variable(1),
            Alias.None
          )
        ),
        Set(
          IndexedSeq(
            Alias.None,
            Alias.Variable(4),
            Alias.Variable(1),
            Alias.Variable(1),
            Alias.None
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
        Set(
          IndexedSeq(
            Alias.Undefined,
            Alias.Undefined,
            Alias.Undefined,
            Alias.Undefined,
            Alias.Undefined,
            Alias.Undefined,
            Alias.Undefined
          )
        ),
        Set(
          IndexedSeq(
            Alias.Undefined,
            Alias.None,
            Alias.Undefined,
            Alias.Undefined,
            Alias.Undefined,
            Alias.Undefined,
            Alias.Undefined
          )
        ),
        Set(
          IndexedSeq(
            Alias.Undefined,
            Alias.None,
            Alias.None,
            Alias.Undefined,
            Alias.Undefined,
            Alias.Undefined,
            Alias.Undefined
          )
        ),
        Set(
          IndexedSeq(
            Alias.Undefined,
            Alias.None,
            Alias.None,
            Alias.Undefined,
            Alias.Undefined,
            Alias.Undefined,
            Alias.Undefined
          )
        ),
        Set(
          IndexedSeq(
            Alias.Undefined,
            Alias.None,
            Alias.None,
            Alias.Undefined,
            Alias.Variable(1),
            Alias.Undefined,
            Alias.Undefined
          )
        ),
        Set(
          IndexedSeq(
            Alias.Undefined,
            Alias.None,
            Alias.None,
            Alias.Variable(1),
            Alias.Variable(1),
            Alias.Undefined,
            Alias.Undefined
          )
        ),
        Set(
          IndexedSeq(
            Alias.Undefined,
            Alias.None,
            Alias.None,
            Alias.Undefined,
            Alias.Undefined,
            Alias.Undefined,
            Alias.Undefined
          )
        ),
        Set(
          IndexedSeq(
            Alias.Undefined,
            Alias.None,
            Alias.None,
            Alias.Undefined,
            Alias.Undefined,
            Alias.Variable(2),
            Alias.Undefined
          )
        ),
        Set(
          IndexedSeq(
            Alias.Undefined,
            Alias.None,
            Alias.None,
            Alias.Variable(2),
            Alias.Undefined,
            Alias.Variable(2),
            Alias.Undefined
          )
        ),
        Set(
          IndexedSeq(
            Alias.Undefined,
            Alias.None,
            Alias.None,
            Alias.Variable(1),
            Alias.Variable(1),
            Alias.Undefined,
            Alias.None
          ),
          IndexedSeq(
            Alias.Undefined,
            Alias.None,
            Alias.None,
            Alias.Variable(2),
            Alias.Undefined,
            Alias.Variable(2),
            Alias.None
          )
        ),
        Set(
          IndexedSeq(
            Alias.None,
            Alias.None,
            Alias.None,
            Alias.Variable(1),
            Alias.Variable(1),
            Alias.Undefined,
            Alias.None
          ),
          IndexedSeq(
            Alias.None,
            Alias.None,
            Alias.None,
            Alias.Variable(2),
            Alias.Undefined,
            Alias.Variable(2),
            Alias.None
          )
        )
      )
    )
    check(
      "fn f<'a>(x: &'a i32, y: &'a i32) -> &'a i32 { x } fn main() { f(&42, &1337) }",
      IndexedSeq(
        Set(
          IndexedSeq(
            Alias.Undefined,
            Alias.Undefined,
            Alias.Undefined,
            Alias.Undefined,
            Alias.Undefined,
            Alias.Undefined
          )
        ),
        Set(
          IndexedSeq(
            Alias.Undefined,
            Alias.None,
            Alias.Undefined,
            Alias.Undefined,
            Alias.Undefined,
            Alias.Undefined
          )
        ),
        Set(
          IndexedSeq(
            Alias.Undefined,
            Alias.None,
            Alias.Variable(1),
            Alias.Undefined,
            Alias.Undefined,
            Alias.Undefined
          )
        ),
        Set(
          IndexedSeq(
            Alias.Undefined,
            Alias.None,
            Alias.Variable(1),
            Alias.None,
            Alias.Undefined,
            Alias.Undefined
          )
        ),
        Set(
          IndexedSeq(
            Alias.Undefined,
            Alias.None,
            Alias.Variable(1),
            Alias.None,
            Alias.Variable(3),
            Alias.Undefined
          )
        ),
        Set(
          IndexedSeq(
            Alias.Undefined,
            Alias.None,
            Alias.Variable(1),
            Alias.None,
            Alias.Variable(3),
            Alias.Variable(1)
          ),
          IndexedSeq(
            Alias.Undefined,
            Alias.None,
            Alias.Variable(1),
            Alias.None,
            Alias.Variable(3),
            Alias.Variable(3)
          )
        ),
        Set(
          IndexedSeq(
            Alias.Variable(1),
            Alias.None,
            Alias.Variable(1),
            Alias.None,
            Alias.Variable(3),
            Alias.Variable(1)
          ),
          IndexedSeq(
            Alias.Variable(3),
            Alias.None,
            Alias.Variable(1),
            Alias.None,
            Alias.Variable(3),
            Alias.Variable(3)
          )
        )
      )
    )
  }

  /** Checks that the result of alias analysis on the declaration in a source string returns an
    * `expected` sequence of aliasing states.
    */
  private def check(source: String, expected: IndexedSeq[AliasState]): Unit =
    Parser(source) match {
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
