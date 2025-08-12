package inox.borrowing

import org.scalatest.funsuite.AnyFunSuite
import inox.{Location, Result, Span, Spanned}
import inox.lowering.Lowerer
import inox.parsing.Parser
import inox.typing.TypeChecker

class BorrowCheckerTests extends AnyFunSuite {
  test("Function declarations should borrow check properly") {
    checkOk(
      "fn main() { let mut x = 42; let y = &x; let z = &x; let w = y; }"
    )
    checkOk("fn main() { let x = 42; let y = &mut x; let z = &x; }")

    checkError(
      "fn main() { let x: i32; let y = &x; }",
      IndexedSeq(
        BorrowError.UninitializedVariable(
          Spanned("x", Span(Location(1, 17, 16), Location(1, 18, 17)))
        )
      )
    )
    checkError(
      "fn main() { let x = 42; let y = &x; let z = &mut *y; let w = y; }",
      IndexedSeq(
        BorrowError.InvalidBorrow(
          Span(Location(1, 45, 44), Location(1, 52, 51))
        )
      )
    )
    checkError(
      "fn f<'a>(x: &'a &'a mut i32) { let r = &mut **x; }",
      IndexedSeq(
        BorrowError.InvalidBorrow(
          Span(Location(1, 40, 39), Location(1, 48, 47))
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
            TypeChecker.checkModule(ir) match {
              case Result.Success(_) =>
                BorrowChecker.checkModule(ir) match {
                  case Result.Success(_)      => assert(true)
                  case Result.Failure(errors) =>
                    assert(
                      false,
                      s"Unexpected borrowing errors in the input string: ${errors.mkString("\n")}"
                    )
                }
              case Result.Failure(errors) =>
                assert(
                  false,
                  s"Unexpected type errors in the input string: ${errors.mkString("\n")}."
                )
            }
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
            TypeChecker.checkModule(ir) match {
              case Result.Success(_) =>
                BorrowChecker.checkModule(ir) match {
                  case Result.Success(_) =>
                    assert(false, "Expected borrowing errors.")
                  case Result.Failure(errors) => assert(errors == expected)
                }
              case Result.Failure(errors) =>
                assert(
                  false,
                  s"Unexpected type errors in the input string: ${errors.mkString("\n")}."
                )
            }
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
