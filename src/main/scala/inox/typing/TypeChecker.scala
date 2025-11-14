package inox.typing

import inox.ast.BinaryOp
import inox.ir
import inox.ir.*
import inox.typing.TypeError.*
import inox.util.{Result, Spanned}

import scala.annotation.tailrec
import scala.collection.mutable

/** A type checker for Inox. */
object TypeChecker {

  /** Type checks an IR module. */
  def checkModule(module: Module): Result[Unit, TypeError] = Result.build { errors =>
    val typeChecker = TypeChecker(module)
    for { (_, function) <- module } {
      typeChecker.checkFunction(function).handleFailure(errs => errors ++= errs)
    }
    ()
  }
}

/** A type checker for Inox. */
private class TypeChecker(module: inox.ir.Module) {

  /** Type checks a function declaration. */
  private def checkFunction(function: Function): Result[Unit, TypeError] = Result.build { errors =>
    for { (local, i) <- function.locals.zipWithIndex } {
      checkType(local.ty, i <= function.paramCount).handleFailure(errs => errors ++= errs)
    }
    checkBlock(function.locals, function.body).handleFailure(errs => errors ++= errs)
    ()
  }

  /** Type checks a block of instructions. */
  private def checkBlock(locals: IndexedSeq[Local], block: Block): Result[Unit, TypeError] =
    Result.build { errors =>
      for { instr <- block } {
        checkInstr(locals, instr).handleFailure(errs => errors ++= errs)
      }
      ()
    }

  /** Type checks an IR instruction. */
  private def checkInstr(locals: IndexedSeq[Local], instr: Instr): Result[Unit, TypeError] =
    instr match {
      case Instr.While(cond, body)               => checkWhile(locals, cond, body)
      case Instr.If(cond, thn, els)              => checkIf(locals, cond, thn, els)
      case Instr.Call(target, callee, args)      => checkCall(locals, target, callee, args)
      case Instr.Borrow(target, mutable, source) => checkBorrow(locals, target, mutable, source)
      case Instr.Assign(target, value)           => checkAssignment(locals, target, value)
      case Instr.Binary(target, op, lhs, rhs)    => checkBinary(locals, target, op, lhs, rhs)
      case Instr.Unary(target, op, operand)      => checkUnary(locals, target, op, operand)
      case Instr.Return                          => Result.Success(())
    }

  /** Type checks a while instruction. */
  private def checkWhile(
      locals: IndexedSeq[Local],
      cond: Operand,
      body: Block
  ): Result[Unit, TypeError] = checkOperand(locals, cond).flatMap { ty =>
    ty.value.item match {
      case TypeKind.Bool => for (_ <- checkBlock(locals, body)) yield ()
      case _             => Result.fail(InvalidCondition(ty))
    }
  }

  /** Type checks an if instruction. */
  private def checkIf(
      locals: IndexedSeq[Local],
      cond: Operand,
      thn: Block,
      els: Block
  ): Result[Unit, TypeError] = checkOperand(locals, cond).flatMap { ty =>
    ty.value.item match {
      case TypeKind.Bool =>
        for {
          _ <- checkBlock(locals, thn)
          _ <- checkBlock(locals, els)
        } yield {
          ()
        }
      case _ => Result.fail(InvalidCondition(ty))
    }
  }

  /** Type checks a call instruction. */
  private def checkCall(
      locals: IndexedSeq[Local],
      target: Place,
      callee: Operand,
      args: IndexedSeq[Operand]
  ): Result[Unit, TypeError] = for {
    ty <- checkOperand(locals, callee)
    _ <-
      ty.value.item match {
        case TypeKind.Fn(params, result) =>
          if (args.length != params.length) {
            Result.fail(InvalidArgNum(Spanned(args.length, callee.span), params.length))
          } else {
            Result.build { (errors: mutable.Builder[TypeError, Seq[TypeError]]) =>
              for { (arg, param) <- args.zip(params) } {
                checkOperand(locals, arg) match {
                  case Result.Success(argType) =>
                    if (!(argType :< param)) {
                      errors += InvalidArgType(argType, param)
                    }
                  case Result.Failure(errs) => errors ++= errs
                }
              }
              for { targetType <- checkPlace(locals, target) } yield {
                if (!(result :< targetType)) {
                  errors += IncompatibleTypes(result, targetType)
                }
              }
              ()
            }
          }
        case _ => Result.fail(InvalidCallee(ty))
      }
  } yield {
    ()
  }

  /** Type checks a borrow instruction. */
  private def checkBorrow(
      locals: IndexedSeq[Local],
      target: Place,
      mutable: Boolean,
      source: Place
  ): Result[Unit, TypeError] = for {
    targetType <- checkPlace(locals, target)
    sourceType <- checkPlace(locals, source)
    _ <- {
      val ty = Type.Ref(None, mutable, sourceType, target.span)
      checkCompatibility(targetType, ty)
    }
  } yield {
    ()
  }

  /** Type checks an assignment instruction. */
  private def checkAssignment(
      locals: IndexedSeq[Local],
      target: Place,
      value: Operand
  ): Result[Unit, TypeError] = for {
    targetType <- checkPlace(locals, target)
    valueType <- checkOperand(locals, value)
    _ <- checkCompatibility(targetType, valueType)
  } yield {
    ()
  }

  /** Type checks a binary instruction. */
  private def checkBinary(
      locals: IndexedSeq[Local],
      target: Place,
      op: BinaryOp,
      lhs: Operand,
      rhs: Operand
  ): Result[Unit, TypeError] = for {
    targetType <- checkPlace(locals, target)
    lhsType <- checkOperand(locals, lhs)
    rhsType <- checkOperand(locals, rhs)
    _ <- op match {
      case BinaryOp.And | BinaryOp.Or =>
        if (lhsType.value.item != TypeKind.Bool) {
          Result.fail(InvalidOperand(lhsType, TypeKind.Bool))
        } else if (rhsType.value.item != TypeKind.Bool) {
          Result.fail(InvalidOperand(rhsType, TypeKind.Bool))
        } else {
          checkCompatibility(targetType, lhsType)
        }
      case BinaryOp.Eq | BinaryOp.Neq => checkCompatibility(lhsType, rhsType)
      case _                          =>
        if (lhsType.value.item != TypeKind.I32) {
          Result.fail(InvalidOperand(lhsType, TypeKind.I32))
        } else if (rhsType.value.item != TypeKind.I32) {
          Result.fail(InvalidOperand(rhsType, TypeKind.I32))
        } else {
          checkCompatibility(targetType, lhsType)
        }
    }
  } yield {
    ()
  }

  /** Type checks a unary expression. */
  private def checkUnary(
      locals: IndexedSeq[Local],
      target: Place,
      op: UnOp,
      operand: Operand
  ): Result[Unit, TypeError] = for {
    targetType <- checkPlace(locals, target)
    operandType <- checkOperand(locals, operand)
    _ <-
      if (op == UnOp.Not && operandType.value.item != TypeKind.Bool) {
        Result.fail(InvalidOperand(operandType, TypeKind.Bool))
      } else if (op == UnOp.Neg && operandType.value.item != TypeKind.I32) {
        Result.fail(InvalidOperand(operandType, TypeKind.I32))
      } else {
        checkCompatibility(targetType, operandType)
      }
  } yield {
    ()
  }

  /** Type checks an instruction operand. */
  private def checkOperand(locals: IndexedSeq[Local], operand: Operand): Result[Type, TypeError] =
    operand.item match {
      case ir.OperandKind.Place(p)          => checkPlace(locals, Spanned(p, operand.span))
      case ir.OperandKind.Fn(name, origins) => {
        val fn = module(name.item)
        if (origins.length != fn.originCount) {
          Result.fail(InvalidOriginArgNum(name, origins.length, fn.originCount))
        } else {
          Result.Success(fn.ty.substitute(origins))
        }
      }
      case ir.OperandKind.I32(value)  => Result.Success(Type.I32(operand.span))
      case ir.OperandKind.Bool(value) => Result.Success(Type.Bool(operand.span))
      case ir.OperandKind.Unit        => Result.Success(Type.Unit(operand.span))
    }

  /** Type checks a place expression. */
  private def checkPlace(locals: IndexedSeq[Local], place: Place): Result[Type, TypeError] =
    place.item match {
      case PlaceKind.Deref(p) =>
        checkPlace(locals, p).flatMap { ty =>
          ty.value.item match {
            case TypeKind.Ref(_, _, rType) => Result.Success(rType)
            case _                         => Result.fail(InvalidDeref(ty))
          }
        }
      case PlaceKind.Var(id) => Result.Success(locals(id).ty)
    }

  /** Checks that an IR type is well-formed. */
  @tailrec
  private def checkType(ty: Type, withOrigins: Boolean = false): Result[Unit, TypeError] =
    ty.value.item match {
      case TypeKind.Fn(params, result)    => checkFnType(params, result, withOrigins)
      case TypeKind.Ref(origin, _, rType) =>
        if (withOrigins && origin.isEmpty) {
          Result.fail(OriginNeeded(ty.value.span))
        } else {
          checkType(rType, withOrigins)
        }
      case TypeKind.I32 | TypeKind.Bool | TypeKind.Unit => Result.Success(())
    }

  /** Checks that an IR function type is well-formed. */
  private def checkFnType(
      params: IndexedSeq[Type],
      result: Type,
      withOrigins: Boolean = false
  ): Result[Unit, TypeError] = Result.build { errors =>
    for { param <- params } {
      checkType(param, withOrigins).handleFailure(errs => errors ++= errs)
    }
    checkType(result, withOrigins).handleFailure(errs => errors ++= errs)
    ()
  }

  /** Checks if the type of some value is compatible with some target type. */
  private def checkCompatibility(target: Type, value: Type): Result[Unit, TypeError] =
    if (!(value :< target)) {
      Result.fail(IncompatibleTypes(value, target))
    } else {
      Result.Success(())
    }
}
