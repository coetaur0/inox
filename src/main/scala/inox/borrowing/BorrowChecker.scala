package inox.borrowing

import scala.annotation.tailrec
import inox.{Result, Span}
import inox.ir.{Block, Instr, Local}

/** A borrow checker for Inox. */
object BorrowChecker {

  /** Borrow checks a module. */
  def checkModule(module: inox.ir.Module): Result[Unit, BorrowError] =
    Result.build { errors =>
      val aliasAnalyser = AliasAnalysis(module)

      for (_, function) <- module do
        checkFunction(aliasAnalyser, function).handleFailure(errs =>
          errors ++= errs
        )
    }

  /** Borrow checks a function declaration. */
  private def checkFunction(
      aliasAnalyser: AliasAnalysis,
      function: inox.ir.Function
  ): Result[Unit, BorrowError] =
    Result.build { errors =>
      val live = LiveAnalysis.liveFunction(function)
      val (locals, aliases) = aliasAnalyser.aliasFunction(function)

      for id <- live.head do
        if id == 0 || id > function.paramCount then
          errors += BorrowError.UninitializedVariable(locals(id).name)

      checkBlock(
        locals,
        function.locals.length,
        live.tail,
        aliases.tail,
        function.body
      ) match {
        case Result.Success(_)    => ()
        case Result.Failure(errs) => errors ++= errs
      }
    }

  /** Borrow checks a block of instructions. */
  @tailrec
  private def checkBlock(
      locals: IndexedSeq[Local],
      numLocals: Int,
      live: IndexedSeq[LiveSet],
      aliases: IndexedSeq[AliasMap],
      block: Block
  ): Result[(IndexedSeq[LiveSet], IndexedSeq[AliasMap]), BorrowError] = {
    if block.isEmpty then Result.Success((live, aliases))
    else
      checkInstr(locals, numLocals, live, aliases, block.head) match {
        case Result.Success((newLive, newAliases)) =>
          checkBlock(locals, numLocals, newLive, newAliases, block.tail)
        case Result.Failure(errors) => Result.Failure(errors)
      }
  }

  /** Borrow checks an instruction. */
  private def checkInstr(
      locals: IndexedSeq[Local],
      numLocals: Int,
      live: IndexedSeq[LiveSet],
      aliases: IndexedSeq[AliasMap],
      instr: Instr
  ): Result[(IndexedSeq[LiveSet], IndexedSeq[AliasMap]), BorrowError] =
    instr match {
      case Instr.While(cond, body) =>
        checkBlock(locals, numLocals, live.tail, aliases.tail, body)
      case Instr.If(cond, thn, els) =>
        for {
          (newLive, newAliases) <- checkBlock(
            locals,
            numLocals,
            live.tail,
            aliases.tail,
            thn
          )
          result <- checkBlock(locals, numLocals, newLive, newAliases, els)
        } yield result
      case Instr.Call(target, _, _) =>
        for _ <- checkConflicts(
            locals,
            numLocals,
            live.head,
            aliases.head,
            target.span
          )
        yield (live.tail, aliases.tail)
      case Instr.Borrow(target, _, _) =>
        for _ <- checkConflicts(
            locals,
            numLocals,
            live.head,
            aliases.head,
            target.span
          )
        yield (live.tail, aliases.tail)
      case Instr.Assign(target, _) =>
        for _ <- checkConflicts(
            locals,
            numLocals,
            live.head,
            aliases.head,
            target.span
          )
        yield (live.tail, aliases.tail)
      case Instr.Binary(_, _, _, _) | Instr.Unary(_, _, _) | Instr.Return =>
        Result.Success((live.tail, aliases.tail))
    }

  /** Checks that no variable is aliased mutably by more than one live variable.
    */
  private def checkConflicts(
      locals: IndexedSeq[Local],
      numLocals: Int,
      live: LiveSet,
      aliases: AliasMap,
      span: Span
  ): Result[Unit, BorrowError] =
    Result.build { errors =>
      var remaining = live ++ Range(numLocals + 1, locals.length)

      for id <- live ++ Range(numLocals + 1, locals.length) do {
        remaining = remaining - id
        for otherId <- remaining do {
          if (aliases(id)._1 || aliases(otherId)._1) &&
            (aliases(id)._2 & aliases(otherId)._2).nonEmpty
          then errors += BorrowError.InvalidBorrow(span)
        }
      }
    }
}
