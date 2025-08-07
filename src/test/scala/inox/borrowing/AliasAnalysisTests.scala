package inox.borrowing

import org.scalatest.funsuite.AnyFunSuite
import inox.Result
import inox.lowering.Lowerer
import inox.parsing.Parser

class AliasAnalysisTests extends AnyFunSuite {
  test("Alias analysis should be correctly computed") {
    check(
      """fn main() {
        |  let mut x: i32;
        |  let mut r: &mut i32;
        |  r = &mut x;
        |}""".stripMargin,
      IndexedSeq(
        AliasMap(IndexedSeq(Set(), Set(), Set(), Set((true, 1)))),
        AliasMap(IndexedSeq(Set(), Set(), Set((true, 1)), Set((true, 1)))),
        AliasMap(IndexedSeq(Set(), Set(), Set((true, 1)), Set((true, 1))))
      )
    )
    check(
      """fn main<'a>(x: &'a i32) {
        |  let r = &x;
        |}""".stripMargin,
      IndexedSeq(
        AliasMap(
          IndexedSeq(Set(), Set((false, 4)), Set((false, 1)), Set(), Set())
        ),
        AliasMap(
          IndexedSeq(
            Set(),
            Set((false, 4)),
            Set((false, 1)),
            Set((false, 1)),
            Set()
          )
        ),
        AliasMap(
          IndexedSeq(
            Set(),
            Set((false, 4)),
            Set((false, 1)),
            Set((false, 1)),
            Set()
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
        AliasMap(IndexedSeq(Set(), Set(), Set(), Set(), Set(), Set(), Set())),
        AliasMap(IndexedSeq(Set(), Set(), Set(), Set(), Set(), Set(), Set())),
        AliasMap(
          IndexedSeq(Set(), Set(), Set(), Set(), Set((false, 1)), Set(), Set())
        ),
        AliasMap(
          IndexedSeq(
            Set(),
            Set(),
            Set(),
            Set((false, 1)),
            Set((false, 1)),
            Set(),
            Set()
          )
        ),
        AliasMap(
          IndexedSeq(
            Set(),
            Set(),
            Set(),
            Set((false, 1)),
            Set((false, 1)),
            Set(),
            Set()
          )
        ),
        AliasMap(
          IndexedSeq(
            Set(),
            Set(),
            Set(),
            Set(),
            Set(),
            Set((false, 2)),
            Set()
          )
        ),
        AliasMap(
          IndexedSeq(
            Set(),
            Set(),
            Set(),
            Set((false, 2)),
            Set(),
            Set((false, 2)),
            Set()
          )
        ),
        AliasMap(
          IndexedSeq(
            Set(),
            Set(),
            Set(),
            Set((false, 2)),
            Set(),
            Set((false, 2)),
            Set()
          )
        ),
        AliasMap(
          IndexedSeq(
            Set(),
            Set(),
            Set(),
            Set((false, 1), (false, 2)),
            Set((false, 1)),
            Set((false, 2)),
            Set()
          )
        ),
        AliasMap(
          IndexedSeq(
            Set(),
            Set(),
            Set(),
            Set((false, 1), (false, 2)),
            Set((false, 1)),
            Set((false, 2)),
            Set()
          )
        )
      )
    )
  }

  /** Checks that the result of alias analysis on the declaration in a source
    * string returns an `expected` sequence of alias maps.
    */
  private def check(source: String, expected: IndexedSeq[AliasMap]): Unit =
    Parser.parseModule(source) match {
      case Result.Success(ast) =>
        Lowerer.lowerModule(ast) match {
          case Result.Success(ir) =>
            assert(AliasAnalysis(ir).aliasFunction(ir("main")) == expected)
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
