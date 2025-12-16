package inox.codegen

import inox.analysis.BorrowChecker
import inox.lowering.Lowerer
import inox.parsing.Parser
import inox.typing.TypeChecker
import inox.util
import inox.util.Result
import org.scalatest.funsuite.AnyFunSuite

class CEmitterTests extends AnyFunSuite {
  test("C codegen should produce valid C code") {
    check(
      """fn main() -> i32 {
        |  42
        |}""".stripMargin,
      """int main();
        |
        |int main() {
        |  int _0;
        |  _0 = 42;
        |  return _0;
        |}""".stripMargin
    )
    check(
      """fn h(c: bool, f1: fn(i32) -> i32, f2: fn(i32) -> i32) -> fn(i32) -> i32 {
        |  if c {
        |    f1
        |  } else {
        |    f2
        |  }
        |}""".stripMargin,
      """typedef int (*_1183758944)(int);
        |
        |_1183758944 h(int _1, _1183758944 _2, _1183758944 _3);
        |
        |_1183758944 h(int _1, _1183758944 _2, _1183758944 _3) {
        |  _1183758944 _0;
        |  _1183758944 _4;
        |  if (_1) {
        |    _4 = _2;
        |  } else {
        |    _4 = _3;
        |  }
        |  _0 = _4;
        |  return _0;
        |}""".stripMargin
    )
    check(
      "fn f<'a>(r: &'a i32) -> i32 { *r } fn main() { f::<'_>(&42); }",
      """int f(int* _1);
        |
        |void main();
        |
        |int f(int* _1) {
        |  int _0;
        |  _0 = *_1;
        |  return _0;
        |}
        |
        |void main() {
        |  int _1;
        |  int* _2;
        |  int _3;
        |  _1 = 42;
        |  _2 = &_1;
        |  _3 = f(_2);
        |}""".stripMargin
    )
    check(
      """fn main() {
        |  let x = 42;
        |  let r = &x;
        |  if g() {
        |    *r
        |  } else {
        |    1
        |  };
        |}
        |
        |fn g() -> bool {
        |  true
        |}""".stripMargin,
      """void main();
        |
        |int g();
        |
        |void main() {
        |  int _1;
        |  int* _2;
        |  int* _3;
        |  int _4;
        |  int _5;
        |  _1 = 42;
        |  _2 = &_1;
        |  _3 = _2;
        |  _4 = g();
        |  if (_4) {
        |    _5 = *_3;
        |  } else {
        |    _5 = 1;
        |  }
        |}
        |
        |int g() {
        |  int _0;
        |  _0 = 1;
        |  return _0;
        |}""".stripMargin
    )
  }

  /** Checks that emitting C code for a source string returns the expected result. */
  private def check(source: String, expected: String): Unit = Parser(source) match {
    case Result.Success(ast) =>
      Lowerer(ast) match {
        case Result.Success(ir) =>
          TypeChecker(ir) match {
            case util.Result.Success(item) =>
              BorrowChecker(ir) match {
                case Result.Success(_) =>
                  assert(CEmitter(ir) == expected)
                case Result.Failure(errors) =>
                  assert(
                    false,
                    s"Unexpected borrow checking errors in the input string: ${errors.mkString("\n")}"
                  )
              }
            case util.Result.Failure(errors) =>
              assert(false, s"Unexpected type errors in the input string: ${errors.mkString("\n")}")
          }
        case Result.Failure(errors) =>
          assert(
            false,
            s"Unexpected lowering errors in the input string: ${errors.mkString("\n")}"
          )
      }
    case Result.Failure(errors) =>
      assert(false, s"Unexpected syntax errors in the input string: ${errors.mkString("\n")}")
  }
}
