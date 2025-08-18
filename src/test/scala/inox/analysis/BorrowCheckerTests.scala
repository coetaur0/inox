package inox.analysis

import inox.{Location, Result, Span, Spanned}
import inox.lowering.Lowerer
import inox.parsing.Parser
import org.scalatest.funsuite.AnyFunSuite

class BorrowCheckerTests extends AnyFunSuite {

  test("Borrow checking should work correctly") {
    checkOk("fn f<'a>(r: &'a mut i32) { let x = &mut *r; }")
    checkOk("fn f<'a>(r: &'a mut i32) { let x = &mut *r; *x = 42; }")
    checkOk("""fn main() {
        |  let x: i32;
        |  let y = 42;
        |  if true {
        |    x = y;
        |  } else {
        |    x = 10;
        |  };
        |  x;
        |}""".stripMargin)

    checkError(
      """fn main() {
        |  let x = 42;
        |  let r = &mut x;
        |}""".stripMargin,
      IndexedSeq(
        BorrowError.UnauthorisedBorrow(
          Span(Location(3, 11, 36), Location(3, 17, 42))
        )
      )
    )
    checkError(
      """fn main() {
        |  let mut x = 42;
        |  let r = &mut x;
        |  let y = &*r;
        |  let z = r;
        |}""".stripMargin,
      IndexedSeq(
        BorrowError.InvalidReborrow(
          Span(Location(4, 11, 58), Location(4, 14, 61))
        )
      )
    )
    checkError(
      """fn main() {
        |  let mut x = 42;
        |  let r = &mut x;
        |  x = 1337;
        |  let y = &*r;
        |}""".stripMargin,
      IndexedSeq(
        BorrowError.UnauthorisedAssignment(
          Spanned("x", Span(Location(4, 3, 50), Location(4, 4, 51)))
        )
      )
    )
    checkError(
      """fn main() {
        |  let x = 42;
        |  x = 4;
        |}""".stripMargin,
      IndexedSeq(
        BorrowError.UnauthorisedReassignment(
          Spanned("x", Span(Location(3, 3, 28), Location(3, 4, 29)))
        )
      )
    )
    checkError(
      """fn main() {
        |  let x: i32;
        |  let r = &x;
        |}""".stripMargin,
      IndexedSeq(
        BorrowError.UninitializedVariable(
          Spanned("x", Span(Location(3, 11, 36), Location(3, 13, 38)))
        )
      )
    )
  }

  /** Checks that borrow checking the declarations in a source string succeeds.
    */
  private def checkOk(source: String): Unit =
    Parser.parseModule(source) match {
      case Result.Success(ast) =>
        Lowerer.lowerModule(ast) match {
          case Result.Success(ir) =>
            BorrowChecker.checkModule(ir) match {
              case Result.Success(_)      => assert(true)
              case Result.Failure(errors) =>
                assert(
                  false,
                  s"Unexpected borrow checking errors in the input string: ${errors.mkString("\n")}"
                )
            }
          case Result.Failure(errors) =>
            assert(
              false,
              s"Unexpected lowering errors in the input string: ${errors.mkString("\n")}"
            )
        }
      case Result.Failure(errors) =>
        assert(
          false,
          s"Unexpected syntax errors in the input string: ${errors.mkString("\n")}"
        )
    }

  /** Checks that type checking the declarations in a source string returns an
    * `expected` sequence of type errors.
    */
  private def checkError(
      source: String,
      expected: IndexedSeq[BorrowError]
  ): Unit =
    Parser.parseModule(source) match {
      case Result.Success(ast) =>
        Lowerer.lowerModule(ast) match {
          case Result.Success(ir) =>
            BorrowChecker.checkModule(ir) match {
              case Result.Success(_) =>
                assert(
                  false,
                  "Expected borrow checking errors in the input string."
                )
              case Result.Failure(errors) => assert(errors == expected)
            }
          case Result.Failure(errors) =>
            assert(
              false,
              s"Unexpected lowering errors in the input string: ${errors.mkString("\n")}"
            )
        }
      case Result.Failure(errors) =>
        assert(
          false,
          s"Unexpected syntax errors in the input string: ${errors.mkString("\n")}"
        )
    }

}
