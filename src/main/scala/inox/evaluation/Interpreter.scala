package inox.evaluation

import inox.ast.BinaryOp
import inox.ir.*
import inox.util.{Name, Result, Span}

import scala.collection.mutable.ArrayBuffer

/** The reference interpreter for Inox. */
object Interpreter {

  /** A value in the interpreter. */
  enum Value {
    case Function(name: Name)
    case Address(index: Int)
    case I32(value: Int)
    case Bool(value: Boolean)
    case Unit
    case Uninitialised

    override def toString: String = this match {
      case Value.Function(name) => s"Function ${name.item}"
      case Value.Address(index) => s"&$index"
      case Value.I32(value)     => s"$value"
      case Value.Bool(value)    => s"$value"
      case Value.Unit           => "()"
      case Value.Uninitialised  => "?"
    }
  }

  /** Evaluates an IR program. */
  def apply(module: inox.ir.Module): Result[Value, RuntimeError] = {
    val interpreter = new Interpreter(module)
    for {
      main <- module.get("main") match {
        case Some(function) if function.paramCount == 0 => Result.Success(function)
        case _                                          => Result.fail(RuntimeError.NoMain)
      }
      result <- interpreter.evalFunction(
        (ctxt = IndexedSeq.empty[Int], mem = ArrayBuffer.empty[Value]),
        main,
        IndexedSeq.empty[Operand]
      )
    } yield {
      result
    }
  }
}

/** The reference interpreter for Inox. */
private class Interpreter(module: inox.ir.Module) {
  import Interpreter.Value

  private type Env = (ctxt: Context, mem: Memory)
  private type Context = IndexedSeq[Int]
  private type Memory = ArrayBuffer[Value]

  /** Evaluates a function given some arguments. */
  private def evalFunction(
      env: Env,
      function: inox.ir.Function,
      args: Seq[Operand]
  ): Result[Value, RuntimeError] = {
    def evalArgs(values: Seq[Value], args: Seq[Operand]): Result[Seq[Value], RuntimeError] =
      args.headOption match {
        case Some(value) => evalOperand(env, value).flatMap(v => evalArgs(values :+ v, args.tail))
        case None        => Result.Success(values)
      }

    for {
      argValues <- evalArgs(Seq.empty[Value], args)
      localValues = Seq.fill(function.locals.length - function.paramCount)(Value.Uninitialised)
      memLength = env.mem.length
      newEnv = (
        ctxt = Range(memLength, env.mem.length + function.locals.length).toIndexedSeq,
        mem = env.mem += Value.Uninitialised ++= argValues ++= localValues
      )
      _ <- evalBlock(newEnv, function.body)
    } yield {
      val result = newEnv.mem(memLength)
      env.mem.dropRightInPlace(function.locals.length)
      result
    }
  }

  /** Evaluates an instruction. */
  private def evalInstr(env: Env, instr: Instr): Result[Unit, RuntimeError] = instr match {
    case Instr.While(cond, body)            => evalWhile(env, cond, body)
    case Instr.If(cond, thn, els)           => evalIf(env, cond, thn, els)
    case Instr.Call(target, callee, args)   => evalCall(env, target, callee, args)
    case Instr.Borrow(target, _, source)    => evalBorrow(env, target, source)
    case Instr.Assign(target, value)        => evalAssign(env, target, value)
    case Instr.Binary(target, op, lhs, rhs) => evalBinary(env, target, op, lhs, rhs)
    case Instr.Unary(target, op, operand)   => evalUnary(env, target, op, operand)
    case Instr.Return                       => Result.Success(())
  }

  /** Evaluates a block of instructions. */
  private def evalBlock(env: Env, block: Block): Result[Unit, RuntimeError] =
    if (block.isEmpty || block.head == Instr.Return) {
      Result.Success(())
    } else {
      evalInstr(env, block.head).flatMap(_ => evalBlock(env, block.tail))
    }

  /** Evaluates a loop instruction. */
  private def evalWhile(env: Env, cond: Operand, body: Block): Result[Unit, RuntimeError] = for {
    value <- evalOperand(env, cond)
    _ <-
      if (value == Value.Bool(true)) {
        evalBlock(env, body)
      } else {
        Result.Success(())
      }
  } yield {
    ()
  }

  /** Evaluates a conditional instruction. */
  private def evalIf(env: Env, cond: Operand, thn: Block, els: Block): Result[Unit, RuntimeError] =
    for {
      value <- evalOperand(env, cond)
      _ <-
        if (value == Value.Bool(true)) {
          evalBlock(env, thn)
        } else {
          evalBlock(env, els)
        }
    } yield {
      ()
    }

  /** Evaluates a call instruction. */
  private def evalCall(
      env: Env,
      target: Place,
      callee: Operand,
      args: IndexedSeq[Operand]
  ): Result[Unit, RuntimeError] = for {
    value <- evalOperand(env, callee)
    function <- value match {
      case Value.Function(name) =>
        module.get(name.item) match {
          case Some(function) => Result.Success(function)
          case _              => Result.fail(RuntimeError.UndefinedFunction(name))
        }
      case _ => Result.fail(RuntimeError.InvalidOperation(callee.span))
    }
    result <- evalFunction(env, function, args)
    _ <- assign(env, target, result)
  } yield {
    ()
  }

  /** Evaluates a borrow instruction. */
  private def evalBorrow(env: Env, target: Place, source: Place): Result[Unit, RuntimeError] = for {
    addr <- evalPlace(env, source.item, source.span)
    _ <- assign(env, target, Value.Address(addr))
  } yield {
    ()
  }

  /** Evaluates an assignment instruction. */
  private def evalAssign(env: Env, target: Place, value: Operand) = for {
    value <- evalOperand(env, value)
    _ <- assign(env, target, value)
  } yield {
    ()
  }

  /** Evaluates a binary operation instruction. */
  private def evalBinary(
      env: Env,
      target: Place,
      op: BinaryOp,
      lhs: Operand,
      rhs: Operand
  ): Result[Unit, RuntimeError] = for {
    lValue <- evalOperand(env, lhs)
    rValue <- evalOperand(env, rhs)
    result <- (op, lValue, rValue) match {
      case (BinaryOp.And, Value.Bool(b0), Value.Bool(b1)) => Result.Success(Value.Bool(b0 && b1))
      case (BinaryOp.Or, Value.Bool(b0), Value.Bool(b1))  => Result.Success(Value.Bool(b0 || b1))
      case (BinaryOp.Eq, _, _)  => Result.Success(Value.Bool(lValue == rValue))
      case (BinaryOp.Neq, _, _) => Result.Success(Value.Bool(lValue != rValue))
      case (BinaryOp.Lt, Value.I32(i0), Value.I32(i1))  => Result.Success(Value.Bool(i0 < i1))
      case (BinaryOp.Le, Value.I32(i0), Value.I32(i1))  => Result.Success(Value.Bool(i0 <= i1))
      case (BinaryOp.Gt, Value.I32(i0), Value.I32(i1))  => Result.Success(Value.Bool(i0 > i1))
      case (BinaryOp.Ge, Value.I32(i0), Value.I32(i1))  => Result.Success(Value.Bool(i0 >= i1))
      case (BinaryOp.Add, Value.I32(i0), Value.I32(i1)) => Result.Success(Value.I32(i0 + i1))
      case (BinaryOp.Sub, Value.I32(i0), Value.I32(i1)) => Result.Success(Value.I32(i0 - i1))
      case (BinaryOp.Mul, Value.I32(i0), Value.I32(i1)) => Result.Success(Value.I32(i0 * i1))
      case (BinaryOp.Div, Value.I32(i0), Value.I32(i1)) => Result.Success(Value.I32(i0 / i1))
      case (_, _, _)                                    =>
        Result.fail(RuntimeError.InvalidOperation(Span(start = lhs.span.start, end = rhs.span.end)))
    }
    _ <- assign(env, target, result)
  } yield {
    ()
  }

  /** Evaluates a unary operation instruction. */
  private def evalUnary(
      env: Env,
      target: Place,
      op: UnOp,
      operand: Operand
  ): Result[Unit, RuntimeError] = for {
    value <- evalOperand(env, operand)
    result <- (op, value) match {
      case (UnOp.Not, Value.Bool(b)) => Result.Success(Value.Bool(!b))
      case (UnOp.Neg, Value.I32(i))  => Result.Success(Value.I32(-i))
      case (_, _)                    => Result.fail(RuntimeError.InvalidOperation(operand.span))
    }
    _ <- assign(env, target, result)
  } yield {
    ()
  }

  /** Evaluates an instruction operand to a value. */
  private def evalOperand(env: Env, operand: Operand): Result[Value, RuntimeError] =
    operand.item match {
      case OperandKind.Place(place) =>
        for { addr <- evalPlace(env, place, operand.span) } yield {
          env.mem(addr)
        }
      case OperandKind.Fn(name, _) => Result.Success(Value.Function(name))
      case OperandKind.I32(value)  => Result.Success(Value.I32(value))
      case OperandKind.Bool(value) => Result.Success(Value.Bool(value))
      case OperandKind.Unit        => Result.Success(Value.Unit)
    }

  /** Evaluates a place expression to its address in memory. */
  private def evalPlace(env: Env, place: PlaceKind, span: Span): Result[Int, RuntimeError] =
    place match {
      case PlaceKind.Deref(p) =>
        for {
          addr <- evalPlace(env, p.item, p.span)
          result <- env.mem(addr) match {
            case Value.Address(index) => Result.Success(index)
            case _                    => Result.fail(RuntimeError.InvalidDereference(span))
          }
        } yield {
          result
        }
      case PlaceKind.Var(id) => Result.Success(env.ctxt(id))
    }

  /** Assigns a value to the memory referenced by a place. */
  private def assign(env: Env, target: Place, value: Value): Result[Unit, RuntimeError] = for {
    addr <- evalPlace(env, target.item, target.span)
  } yield {
    env.mem(addr) = value
  }
}
