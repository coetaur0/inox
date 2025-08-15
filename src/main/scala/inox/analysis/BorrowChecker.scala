package inox.analysis

import inox.{Result, Spanned}
import inox.ir.{Block, Instr, Local, Operand, Place, PlaceKind, UnOp}

import scala.collection.mutable

/** A borrow checker for Inox. */
object BorrowChecker {

  /** Borrow checks an IR module. */
  def checkModule(module: inox.ir.Module): Result[Unit, BorrowError] =
    Result.build { errors =>
      for (_, function) <- module do
        checkFunction(module, function).handleFailure(errs => errors ++= errs)

      ()
    }

  /** Borrow checks a function declaration. */
  private def checkFunction(
      module: inox.ir.Module,
      function: inox.ir.Function
  ): Result[Unit, BorrowError] = {
    val live = LiveAnalysis(function).tail
    val (locals, aliases) = AliasAnalysis(module, function)
    val init = InitAnalysis(locals, aliases, function)
    new BorrowChecker(locals, function.locals.length, live, aliases, init)
      .checkBlock(function.body)
  }

}

/** A borrow checker for an Inox function. */
private class BorrowChecker(
    locals: IndexedSeq[Local],
    localCount: Int,
    live: IndexedSeq[LiveSet],
    aliases: IndexedSeq[AliasMap],
    init: IndexedSeq[InitMap]
) {

  private var remainingLive = live
  private var remainingAliases = aliases
  private var remainingInit = init

  /** Borrow checks a block of instructions. */
  def checkBlock(block: Block): Result[Unit, BorrowError] =
    if block.isEmpty then Result.Success(())
    else
      checkInstr(block.head).flatMap { _ =>
        advance()
        checkBlock(block.tail)
      }

  /** Borrow checks an IR instruction. */
  private def checkInstr(instr: Instr): Result[Unit, BorrowError] =
    instr match {
      case Instr.While(cond, body)          => checkWhile(cond, body)
      case Instr.If(cond, thn, els)         => checkIf(cond, thn, els)
      case Instr.Call(target, callee, args) => checkCall(target, callee, args)
      case Instr.Borrow(target, mutable, source) =>
        checkBorrow(target, mutable, source)
      case Instr.Assign(target, value)        => checkAssign(target, value)
      case Instr.Binary(target, op, lhs, rhs) => checkBinary(target, lhs, rhs)
      case Instr.Unary(target, _, operand)    => checkAssign(target, operand)
      case Instr.Return                       => checkReturn()
    }

  /** Borrow checks a while instruction. */
  private def checkWhile(
      cond: Operand,
      body: Block
  ): Result[Unit, BorrowError] =
    for {
      _ <- checkOperand(cond)
      _ <- checkBlock(body)
    } yield ()

  /** Borrow checks an if instruction. */
  private def checkIf(
      cond: Operand,
      thn: Block,
      els: Block
  ): Result[Unit, BorrowError] =
    for {
      _ <- checkOperand(cond)
      _ <- checkBlock(thn)
      _ <- checkBlock(els)
    } yield ()

  /** Borrow checks a call instruction. */
  private def checkCall(
      target: Place,
      callee: Operand,
      args: IndexedSeq[Operand]
  ): Result[Unit, BorrowError] =
    for {
      _ <- checkTarget(target)
      _ <- checkOperand(callee)
      _ <- Result.build {
        (errors: mutable.Builder[BorrowError, IndexedSeq[BorrowError]]) =>
          for arg <- args do
            checkOperand(arg).handleFailure(errs => errors ++= errs)
      }
    } yield ()

  /** Borrow checks a borrow instruction. */
  private def checkBorrow(
      target: Place,
      mut: Boolean,
      source: Place
  ): Result[Unit, BorrowError] =
    for {
      _ <- checkTarget(target)
      _ <- Result.build {
        (errors: mutable.Builder[BorrowError, IndexedSeq[BorrowError]]) =>
          if remainingInit.head(source.item.local) != InitState.Initialized
          then
            errors += BorrowError.UninitializedVariable(
              Spanned(locals(source.item.local).name.item, target.span)
            )

          for id <- AliasAnalysis.placeAliases(
              remainingAliases.head,
              source.item
            )
          do {
            if mut && !locals(id).mutable then {
              errors += BorrowError.UnauthorisedBorrow(target.span)
            }
            for otherId <- remainingLive.head - id ++ Range(
                localCount + 1,
                locals.length
              )
            do {
              if remainingAliases
                  .head(otherId)
                  ._2
                  .contains(id) && (remainingAliases.head(otherId)._1 || mut)
              then errors += BorrowError.InvalidReborrow(target.span)
            }
          }
      }
    } yield ()

  /** Borrow checks an assignment instruction. */
  private def checkAssign(
      target: Place,
      value: Operand
  ): Result[Unit, BorrowError] =
    for {
      _ <- checkTarget(target)
      _ <- checkOperand(value)
    } yield ()

  /** Borrow checks a binary instruction. */
  private def checkBinary(
      target: Place,
      lhs: Operand,
      rhs: Operand
  ): Result[Unit, BorrowError] =
    for {
      _ <- checkTarget(target)
      _ <- checkOperand(lhs)
      _ <- checkOperand(rhs)
    } yield ()

  /** Borrow checks a return instruction. */
  private def checkReturn(): Result[Unit, BorrowError] =
    if remainingInit.head(0) == InitState.Initialized then Result.Success(())
    else Result.fail(BorrowError.UninitializedVariable(locals(0).name))

  /** Checks if an instruction target can be assigned to. */
  private def checkTarget(target: Place): Result[Unit, BorrowError] =
    Result.build { errors =>
      for id <- AliasAnalysis.placeAliases(remainingAliases.head, target.item)
      do {
        if !locals(id).mutable &&
          remainingInit.head(id) != InitState.Uninitialized
        then
          errors += BorrowError.UnauthorisedReassignment(
            Spanned(locals(id).name.item, target.span)
          )

        for otherId <- remainingLive.head ++
            Range(localCount + 1, locals.length)
        do
          if remainingAliases.head(otherId)._2.contains(id) then
            errors += BorrowError.UnauthorisedAssignment(
              Spanned(locals(id).name.item, target.span)
            )
      }
    }

  /** Checks if an instruction operand is initialised before being used. */
  private def checkOperand(operand: Operand): Result[Unit, BorrowError] =
    Result.build { errors =>
      for id <- operand.item.locals do
        if remainingInit.head(id) != InitState.Initialized then
          errors += BorrowError.UninitializedVariable(
            Spanned(locals(id).name.item, operand.span)
          )
    }

  /** Advances the borrow checker to the next instruction. */
  private def advance(): Unit = {
    remainingLive = remainingLive.tail
    remainingAliases = remainingAliases.tail
    remainingInit = remainingInit.tail
  }

}
