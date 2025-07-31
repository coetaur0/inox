package inox.typing

import org.scalatest.funsuite.AnyFunSuite
import inox.ir.Type
import inox.{Location, Result, Span, Spanned}
import inox.lowering.Lowerer
import inox.parsing.Parser

class TypeCheckerTests extends AnyFunSuite:
  test("Function declarations should be type checked properly") {
    checkOk("fn f<'a>(r: &'a i32) -> i32 { *r } fn main() { f::<'_>(&42); }")
    checkOk(
      "fn f<'a, 'b>(x: &'a i32, g: fn(&'a i32) -> &'a i32) -> &'a i32 { g::<'a>(x) }"
    )

    checkError(
      "fn f(r: &i32) {}",
      IndexedSeq(
        TypeError.OriginNeeded(Span(Location(1, 9, 8), Location(1, 13, 12)))
      )
    )
    checkError(
      "fn f() -> bool { 42 }",
      IndexedSeq(
        TypeError.IncompatibleTypes(
          Type.I32(Span(Location(1, 18, 17), Location(1, 20, 19))),
          Type.Bool(Span(Location(1, 11, 10), Location(1, 15, 14)))
        )
      )
    )
    checkError(
      "fn f<'a>(x: &'a i32) -> &'a i32 { let v = 42; &v }",
      IndexedSeq(
        TypeError.IncompatibleTypes(
          Type.Ref(
            None,
            false,
            Type.I32(Span(Location(1, 43, 42), Location(1, 45, 44))),
            Span(Location(1, 47, 46), Location(1, 49, 48))
          ),
          Type.Ref(
            Some(0),
            false,
            Type.I32(Span(Location(1, 29, 28), Location(1, 32, 31))),
            Span(Location(1, 25, 24), Location(1, 32, 31))
          )
        )
      )
    )
  }

  test("Statement should be type checked properly") {
    checkOk("fn f() -> i32 { let x: i32; x = 42; g(); return x; x } fn g() {}")
    checkOk("fn loop() { let mut cond = true; while cond { cond = false; } }")

    checkError(
      "fn f() -> i32 { return false; 42 }",
      IndexedSeq(
        TypeError.IncompatibleTypes(
          Type.Bool(Span(Location(1, 24, 23), Location(1, 29, 28))),
          Type.I32(Span(Location(1, 11, 10), Location(1, 14, 13)))
        )
      )
    )
    checkError(
      "fn main() { while 0 {} }",
      IndexedSeq(
        TypeError.InvalidCondition(
          Type.I32(Span(Location(1, 19, 18), Location(1, 20, 19)))
        )
      )
    )
  }

  test("Expressions should be type checked properly") {
    checkOk(
      "fn main() { let x = 42; let r = &x; if g() { *r } else { 1 }; } fn g() -> bool { true }"
    )

    checkError(
      "fn main() { if 0 { 1 } else { 2 }; }",
      IndexedSeq(
        TypeError.InvalidCondition(
          Type.I32(Span(Location(1, 16, 15), Location(1, 17, 16)))
        )
      )
    )
    checkError(
      "fn f<'a, 'b>() {} fn main() { let f = f::<'_>; }",
      IndexedSeq(
        TypeError.InvalidOriginArgNum(
          Spanned("f", Span(Location(1, 39, 38), Location(1, 40, 39))),
          1,
          2
        )
      )
    )
    checkError(
      "fn f(x: i32, y: i32) {} fn main() { f(3) }",
      IndexedSeq(
        TypeError.InvalidArgNum(
          Spanned(1, Span(Location(1, 37, 36), Location(1, 38, 37))),
          2
        )
      )
    )
    checkError(
      "fn f(x: i32, y: i32) {} fn main() { f(true, false) }",
      IndexedSeq(
        TypeError.InvalidArgType(
          Type.Bool(Span(Location(1, 39, 38), Location(1, 43, 42))),
          Type.I32(Span(Location(1, 9, 8), Location(1, 12, 11)))
        ),
        TypeError.InvalidArgType(
          Type.Bool(Span(Location(1, 45, 44), Location(1, 50, 49))),
          Type.I32(Span(Location(1, 17, 16), Location(1, 20, 19)))
        )
      )
    )
    checkError(
      "fn f<'a, 'b>(x: &'a i32, g: fn(&'b i32) -> ()) { g::<'b>(x) }",
      IndexedSeq(
        TypeError.InvalidArgType(
          Type.Ref(
            Some(0),
            false,
            Type.I32(Span(Location(1, 21, 20), Location(1, 24, 23))),
            Span(Location(1, 17, 16), Location(1, 24, 23))
          ),
          Type.Ref(
            Some(1),
            false,
            Type.I32(Span(Location(1, 36, 35), Location(1, 39, 38))),
            Span(Location(1, 32, 31), Location(1, 39, 38))
          )
        )
      )
    )
    checkError(
      "fn main() { let x = 42; let y = &mut x; }",
      IndexedSeq(
        TypeError.UnauthorisedBorrow(
          Span(Location(1, 38, 37), Location(1, 39, 38))
        )
      )
    )
  }

  /** Checks that type checking the declarations in a source string succeeds. */
  private def checkOk(source: String): Unit =
    Parser.parseModule(source) match
      case Result.Success(ast) =>
        Lowerer.lowerModule(ast) match
          case Result.Success(ir) =>
            TypeChecker.checkModule(ir) match
              case Result.Success(_)      => assert(true)
              case Result.Failure(errors) =>
                assert(
                  false,
                  s"Unexpected type errors in the input string: ${errors.mkString("\n")}."
                )
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

  /** Checks that type checking the declarations in a source string returns an
    * `expected` sequence of type errors.
    */
  private def checkError(
      source: String,
      expected: IndexedSeq[TypeError]
  ): Unit =
    Parser.parseModule(source) match
      case Result.Success(ast) =>
        Lowerer.lowerModule(ast) match
          case Result.Success(ir) =>
            TypeChecker.checkModule(ir) match
              case Result.Success(_) =>
                assert(false, "Expected type errors in the input string.")
              case Result.Failure(errors) => assert(errors == expected)
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
