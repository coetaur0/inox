package inox.typing

import inox.ast.BinaryOp

import scala.annotation.tailrec
import inox.{Result, Spanned, ir}
import inox.ir.*
import TypeError.*

/** A type checker for Inox. */
object TypeChecker:
  /** Type checks an IR module. */
  def checkModule(module: Module): Result[Unit, TypeError] =
    val typeChecker = TypeChecker(module)
    val errorBuilder = IndexedSeq.newBuilder[TypeError]

    for (_, function) <- module do
      typeChecker
        .checkFunction(function)
        .handleFailure(errors => errorBuilder ++= errors)

    val errors = errorBuilder.result()
    if errors.nonEmpty then Result.Failure(errors)
    else Result.Success(())

  /** Emits a type error. */
  private def error[A](kind: TypeError): Result[A, TypeError] =
    Result.Failure(IndexedSeq(kind))

/** A type checker for Inox. */
private class TypeChecker(module: inox.ir.Module):
  /** Type checks a function declaration. */
  private def checkFunction(function: Function): Result[Unit, TypeError] =
    val errorBuilder = IndexedSeq.newBuilder[TypeError]

    for (local, i) <- function.locals.zipWithIndex do
      checkType(local.ty, i <= function.paramCount).handleFailure(errors =>
        errorBuilder ++= errors
      )

    checkBlock(function.locals, function.body).handleFailure(errors =>
      errorBuilder ++= errors
    )

    val errors = errorBuilder.result()
    if errors.nonEmpty then Result.Failure(errors)
    else Result.Success(())

  /** Type checks a block of instructions. */
  private def checkBlock(
      locals: IndexedSeq[Local],
      block: Block
  ): Result[Unit, TypeError] =
    val errorBuilder = IndexedSeq.newBuilder[TypeError]

    for instr <- block do
      checkInstr(locals, instr).handleFailure(errors => errorBuilder ++= errors)

    val errors = errorBuilder.result()
    if errors.nonEmpty then Result.Failure(errors)
    else Result.Success(())

  /** Type checks an IR instruction. */
  private def checkInstr(
      locals: IndexedSeq[Local],
      instr: Instr
  ): Result[Unit, TypeError] =
    instr match
      case Instr.While(cond, body)          => checkWhile(locals, cond, body)
      case Instr.If(cond, thn, els)         => checkIf(locals, cond, thn, els)
      case Instr.Call(target, callee, args) =>
        checkCall(locals, target, callee, args)
      case Instr.Borrow(target, mutable, source) =>
        checkBorrow(locals, target, mutable, source)
      case Instr.Assign(target, value) => checkAssignment(locals, target, value)
      case Instr.Binary(target, op, lhs, rhs) =>
        checkBinary(locals, target, op, lhs, rhs)
      case Instr.Unary(target, op, operand) =>
        checkUnary(locals, target, op, operand)
      case Instr.Return => Result.Success(())

  /** Type checks a while instruction. */
  private def checkWhile(
      locals: IndexedSeq[Local],
      cond: Operand,
      body: Block
  ): Result[Unit, TypeError] =
    checkOperand(locals, cond).flatMap { ty =>
      ty.value.item match
        case TypeKind.Bool =>
          for _ <- checkBlock(locals, body) yield ()
        case _ => TypeChecker.error(InvalidCondition(ty))
    }

  /** Type checks an if instruction. */
  private def checkIf(
      locals: IndexedSeq[Local],
      cond: Operand,
      thn: Block,
      els: Block
  ): Result[Unit, TypeError] =
    checkOperand(locals, cond).flatMap { ty =>
      ty.value.item match
        case TypeKind.Bool =>
          for
            _ <- checkBlock(locals, thn)
            _ <- checkBlock(locals, els)
          yield ()
        case _ => TypeChecker.error(InvalidCondition(ty))
    }

  /** Type checks a call instruction. */
  private def checkCall(
      locals: IndexedSeq[Local],
      target: Place,
      callee: Operand,
      args: IndexedSeq[Operand]
  ): Result[Unit, TypeError] =
    checkOperand(locals, callee).flatMap { ty =>
      ty.value.item match
        case TypeKind.Fn(params, result) =>
          if args.length != params.length then
            TypeChecker.error(
              InvalidArgNum(Spanned(args.length, callee.span), params.length)
            )
          else
            val errorBuilder = IndexedSeq.newBuilder[TypeError]

            for (arg, param) <- args.zip(params) do
              checkOperand(locals, arg) match
                case Result.Success(argType) =>
                  if !(argType :< param) then
                    errorBuilder += InvalidArgType(argType, param)
                case Result.Failure(errors) => errorBuilder ++= errors

            for (_, targetType) <- checkPlace(locals, target)
            yield
              if !(result :< targetType) then
                errorBuilder += IncompatibleTypes(result, targetType)

              val errors = errorBuilder.result()
              if errors.nonEmpty then Result.Failure(errors)
              else Result.Success(())
        case _ => TypeChecker.error(InvalidCallee(ty))
    }

  /** Type checks a borrow instruction. */
  private def checkBorrow(
      locals: IndexedSeq[Local],
      target: Place,
      mutable: Boolean,
      source: Place
  ): Result[Unit, TypeError] =
    checkPlace(locals, target).flatMap { (_, targetType) =>
      checkPlace(locals, source).flatMap { (sourceMut, sourceType) =>
        val ty = Type.Ref(None, mutable, sourceType, target.span)
        if mutable && !sourceMut then
          TypeChecker.error(UnauthorisedBorrow(source.span))
        else checkCompatibility(targetType, ty)
      }
    }

  /** Type checks an assignment instruction. */
  private def checkAssignment(
      locals: IndexedSeq[Local],
      target: Place,
      value: Operand
  ): Result[Unit, TypeError] =
    checkPlace(locals, target).flatMap { (_, targetType) =>
      checkOperand(locals, value).flatMap { valueType =>
        checkCompatibility(targetType, valueType)
      }
    }

  /** Type checks a binary instruction. */
  private def checkBinary(
      locals: IndexedSeq[Local],
      target: Place,
      op: BinaryOp,
      lhs: Operand,
      rhs: Operand
  ): Result[Unit, TypeError] =
    checkPlace(locals, target).flatMap { (_, targetType) =>
      checkOperand(locals, lhs).flatMap { lhsType =>
        checkOperand(locals, rhs).flatMap { rhsType =>
          op match
            case BinaryOp.And | BinaryOp.Or =>
              if lhsType.value.item != TypeKind.Bool then
                TypeChecker.error(InvalidOperand(lhsType, TypeKind.Bool))
              else if rhsType.value.item != TypeKind.Bool then
                TypeChecker.error(InvalidOperand(rhsType, TypeKind.Bool))
              else checkCompatibility(targetType, lhsType)
            case BinaryOp.Eq | BinaryOp.Neq =>
              checkCompatibility(lhsType, rhsType)
            case _ =>
              if lhsType.value.item != TypeKind.I32 then
                TypeChecker.error(InvalidOperand(lhsType, TypeKind.I32))
              else if rhsType.value.item != TypeKind.I32 then
                TypeChecker.error(InvalidOperand(rhsType, TypeKind.I32))
              else checkCompatibility(targetType, lhsType)
        }
      }
    }

  /** Type checks a unary expression. */
  private def checkUnary(
      locals: IndexedSeq[Local],
      target: Place,
      op: UnOp,
      operand: Operand
  ): Result[Unit, TypeError] =
    checkPlace(locals, target).flatMap { (_, targetType) =>
      checkOperand(locals, operand).flatMap { operandType =>
        if op == UnOp.Not && operandType.value.item != TypeKind.Bool then
          TypeChecker.error(InvalidOperand(operandType, TypeKind.Bool))
        else if op == UnOp.Neg && operandType.value.item != TypeKind.I32 then
          TypeChecker.error(InvalidOperand(operandType, TypeKind.I32))
        else checkCompatibility(targetType, operandType)
      }
    }

  /** Type checks an instruction operand. */
  private def checkOperand(
      locals: IndexedSeq[Local],
      operand: Operand
  ): Result[Type, TypeError] =
    operand.item match
      case ir.OperandKind.Place(p) =>
        checkPlace(locals, Spanned(p, operand.span)).map(_._2)
      case ir.OperandKind.Fn(name, origins) =>
        val fn = module(name.item)
        if origins.length != fn.originCount then
          TypeChecker.error(
            InvalidOriginArgNum(name, origins.length, fn.originCount)
          )
        else Result.Success(fn.ty.substitute(origins))
      case ir.OperandKind.I32(value)  => Result.Success(Type.I32(operand.span))
      case ir.OperandKind.Bool(value) => Result.Success(Type.Bool(operand.span))
      case ir.OperandKind.Unit        => Result.Success(Type.Unit(operand.span))

  /** Type checks a place expression. */
  private def checkPlace(
      locals: IndexedSeq[Local],
      place: Place
  ): Result[(Boolean, Type), TypeError] =
    place.item match
      case PlaceKind.Deref(p) =>
        checkPlace(locals, p).flatMap { (_, ty) =>
          ty.value.item match
            case TypeKind.Ref(_, mut, rType) => Result.Success((mut, rType))
            case _ => TypeChecker.error(InvalidDeref(ty))
        }
      case PlaceKind.Var(id) =>
        Result.Success((locals(id).mutable, locals(id).ty))

  /** Checks that an IR type is well-formed. */
  @tailrec
  private def checkType(
      ty: Type,
      withOrigins: Boolean = false
  ): Result[Unit, TypeError] =
    ty.value.item match
      case TypeKind.Fn(params, result) =>
        checkFnType(params, result, withOrigins)
      case TypeKind.Ref(origin, _, rType) =>
        if withOrigins && origin.isEmpty then
          TypeChecker.error(OriginNeeded(ty.value.span))
        else checkType(rType, withOrigins)
      case TypeKind.I32 | TypeKind.Bool | TypeKind.Unit => Result.Success(())

  /** Checks that an IR function type is well-formed. */
  private def checkFnType(
      params: IndexedSeq[Type],
      result: Type,
      withOrigins: Boolean = false
  ): Result[Unit, TypeError] =
    var e: Either[Boolean, Unit] = Right(())

    val errorBuilder = IndexedSeq.newBuilder[TypeError]

    for param <- params do
      checkType(param, withOrigins).handleFailure(errors =>
        errorBuilder ++= errors
      )

    checkType(result, withOrigins).handleFailure(errors =>
      errorBuilder ++= errors
    )

    val errors = errorBuilder.result()
    if errors.nonEmpty then Result.Failure(errors)
    else Result.Success(())

  /** Checks if the type of some value is compatible with some target type. */
  private def checkCompatibility(
      target: Type,
      value: Type
  ): Result[Unit, TypeError] =
    if !(value :< target) then
      TypeChecker.error(IncompatibleTypes(value, target))
    else Result.Success(())
