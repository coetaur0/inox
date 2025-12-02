package inox.analysis

import inox.ir
import inox.ir.*
import inox.util.{Result, Spanned}

import scala.collection.mutable

/** The Inox borrow checker. */
object BorrowChecker {

  /** Borrow checks an IR module. */
  def apply(module: inox.ir.Module): Result[Unit, BorrowError] = Result.build { errors =>
    for { (_, function) <- module } {
      checkFunction(module, function).handleFailure(errs => errors ++= errs)
    }
    ()
  }

  /** Borrow checks a function declaration. */
  private def checkFunction(
      module: inox.ir.Module,
      function: inox.ir.Function
  ): Result[Unit, BorrowError] = {
    val live = LiveAnalysis(function)
    val (locals, aliases) = AliasAnalysis(module, function)
    new BorrowChecker(locals, function.locals.length, live, aliases).checkBlock(function.body)
  }
}

/** The Inox borrow checker. */
class BorrowChecker(
    locals: IndexedSeq[Local],
    localCount: Int,
    liveness: IndexedSeq[LiveSet],
    aliasing: IndexedSeq[AliasState]
) {
  private var liveStates = liveness
  private var aliasStates = aliasing

  /** Borrow checks a block of instructions. */
  private def checkBlock(block: Block): Result[Unit, BorrowError] = if (block.isEmpty) {
    Result.Success(())
  } else {
    checkInstr(block.head).flatMap { _ =>
      advance()
      checkBlock(block.tail)
    }
  }

  /** Borrow checks an IR instruction. */
  private def checkInstr(instr: Instr): Result[Unit, BorrowError] = instr match {
    case Instr.While(cond, body)               => checkWhile(cond, body)
    case Instr.If(cond, thn, els)              => checkIf(cond, thn, els)
    case Instr.Call(target, callee, args)      => checkCall(target, callee, args)
    case Instr.Borrow(target, mutable, source) => checkBorrow(target, mutable, source)
    case Instr.Assign(target, value)           => checkAssign(target, value)
    case Instr.Binary(target, op, lhs, rhs)    => checkBinary(target, lhs, rhs)
    case Instr.Unary(target, op, operand)      => checkAssign(target, operand)
    case Instr.Return                          => checkReturn()
  }

  /** Borrow checks a while instruction. */
  private def checkWhile(cond: Operand, body: Block): Result[Unit, BorrowError] = for {
    _ <- checkOperand(cond)
    _ <- checkBlock(body)
  } yield {
    ()
  }

  /** Borrow checks an if instruction. */
  private def checkIf(cond: Operand, thn: Block, els: Block): Result[Unit, BorrowError] = for {
    _ <- checkOperand(cond)
    _ = advance()
    _ <- checkBlock(thn)
    _ <- checkBlock(els)
  } yield {
    ()
  }

  /** Borrow checks a call instruction. */
  private def checkCall(
      target: Place,
      callee: Operand,
      args: IndexedSeq[Operand]
  ): Result[Unit, BorrowError] = for {
    _ <- checkTarget(target)
    _ <- checkOperand(callee)
    _ <- Result.build { (errors: mutable.Builder[BorrowError, Seq[BorrowError]]) =>
      for { arg <- args } {
        checkOperand(arg).handleFailure(errs => errors ++= errs)
      }
    }
  } yield {
    ()
  }

  /** Borrow checks a borrow instruction. */
  private def checkBorrow(
      target: Place,
      mut: Boolean,
      source: Place
  ): Result[Unit, BorrowError] = for {
    _ <- checkTarget(target)
    _ <- Result.build((errors: mutable.Builder[BorrowError, Seq[BorrowError]]) =>
      for {
        aliasMap <- aliasStates.head
        id = AliasAnalysis.placeAlias(aliasMap, source.item)
        _ = if (mut && !locals(id).mutable) {
          errors += BorrowError.UnauthorisedBorrow(target.span)
        }
        otherId <- liveStates.head - id ++ Range(localCount + 1, locals.length)
      } {
        if (aliasMap(otherId) == Alias.Variable(id)) {
          (mut, locals(otherId).ty.value.item) match {
            case (true, _) | (_, TypeKind.Ref(_, true, _)) =>
              errors += BorrowError.InvalidReborrow(target.span)
            case (_, _) => ()
          }
        }
      }
    )
  } yield {
    ()
  }

  /** Borrow checks an assignment instruction. */
  private def checkAssign(target: Place, value: Operand): Result[Unit, BorrowError] = for {
    _ <- checkTarget(target)
    _ <- checkOperand(value)
  } yield {
    ()
  }

  /** Borrow checks a binary instruction. */
  private def checkBinary(target: Place, lhs: Operand, rhs: Operand): Result[Unit, BorrowError] =
    for {
      _ <- checkTarget(target)
      _ <- checkOperand(lhs)
      _ <- checkOperand(rhs)
    } yield {
      ()
    }

  /** Borrow checks a return instruction. */
  private def checkReturn(): Result[Unit, BorrowError] = Result.build(errors =>
    for { aliasMap <- aliasStates.head } {
      if (aliasMap(0) == Alias.Undefined) {
        errors += BorrowError.UninitializedVariable(locals(0).name)
      }
    }
  )

  /** Checks that an instruction target can be assigned to. */
  private def checkTarget(target: Place): Result[Unit, BorrowError] = Result.build(errors =>
    for { aliasMap <- aliasStates.head } {
      val id = AliasAnalysis.placeAlias(aliasMap, target.item)
      if (!locals(id).mutable && aliasMap(id) != Alias.Undefined) {
        errors += BorrowError.UnauthorisedReassignment(Spanned(locals(id).name.item, target.span))
      }
      for {
        otherId <- liveStates.head - target.item.local ++ Range(localCount + 1, locals.length)
      } {
        if (aliasMap(otherId) == Alias.Variable(id)) {
          errors += BorrowError.UnauthorisedAssignment(Spanned(locals(id).name.item, target.span))
        }
      }
    }
  )

  /** Checks that an instruction operand is initialised before being used. */
  private def checkOperand(operand: Operand): Result[Unit, BorrowError] = Result.build(errors =>
    for {
      id <- operand.item.locals
      aliasMap <- aliasStates.head
    } {
      if (aliasMap(id) == Alias.Undefined) {
        errors += BorrowError.UninitializedVariable(Spanned(locals(id).name.item, operand.span))
      }
    }
  )

  /** Advances the borrow checker to the next instruction. */
  private def advance(): Unit = {
    liveStates = liveStates.tail
    aliasStates = aliasStates.tail
  }
}
