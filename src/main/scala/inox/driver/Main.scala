package inox.driver

import inox.analysis.BorrowChecker
import inox.evaluation.Interpreter
import inox.lowering.Lowerer
import inox.parsing.Parser
import inox.typing.TypeChecker
import inox.util.{InoxError, Result}

import scala.io.Source

@main
def main(path: String): Unit = {
  val file = Source.fromFile(path)
  val src = file.mkString
  file.close()
  val result: Result[Interpreter.Value, InoxError] = for {
    ast <- Parser(src).asInstanceOf[Result[inox.ast.ModuleDecl, InoxError]]
    ir <- Lowerer(ast).asInstanceOf[Result[inox.ir.Module, InoxError]]
    _ <- TypeChecker(ir).asInstanceOf[Result[Unit, InoxError]]
    _ <- BorrowChecker(ir).asInstanceOf[Result[Unit, InoxError]]
    value <- Interpreter(ir).asInstanceOf[Result[Interpreter.Value, InoxError]]
  } yield {
    value
  }
  result match {
    case Result.Success(value)  => println(value)
    case Result.Failure(errors) => {
      println("Encountered errors in the input source:")
      errors.foreach(println)
    }
  }
}
