package inox.lowering

import scala.collection.mutable
import inox.{Name, Result, Span, Spanned}
import inox.ast.*
import inox.ir.*
import inox.lowering.LowerError.InvalidCallee

/** A mapping from function names to their declared origins and type. */
private type Globals = Map[String, (origins: OriginIds, ty: Type)]

/** A mapping from origin names to their ids. */
private type OriginIds = Map[String, OriginId]

/** An AST to IR lowerer. */
object Lowerer:
  /** Returns a mapping from function names to their declared origin ids and
    * type.
    */
  private def globals(
      module: inox.ast.ModuleDecl
  ): Result[Globals, LowerError] =
    val globals = Map.newBuilder[String, (origins: OriginIds, ty: Type)]
    val errorBuilder = IndexedSeq.newBuilder[LowerError]

    for (name, function) <- module do
      originIds(function) match
        case Result.Success(origins) =>
          irType(origins, function.ty) match
            case Result.Success(ty)     => globals += (name -> (origins, ty))
            case Result.Failure(errors) => errorBuilder ++= errors
        case Result.Failure(errors) => errorBuilder ++= errors

    val errors = errorBuilder.result()
    if errors.nonEmpty then Result.Failure(errors)
    else Result.Success(globals.result())

  /** Returns a mapping from origin names to their ids for a given function
    * declaration.
    */
  private def originIds(fnDecl: FnDecl): Result[OriginIds, LowerError] =
    val originIds = Map.newBuilder[String, OriginId]
    val errorBuilder = IndexedSeq.newBuilder[LowerError]

    for (origin, index) <- fnDecl.origins.zipWithIndex do
      if originIds.result().contains(origin.item) then
        errorBuilder += LowerError.DuplicateOrigin(origin)
      else originIds += ((origin.item, index))

    val errors = errorBuilder.result()
    if errors.nonEmpty then Result.Failure(errors)
    else Result.Success(originIds.result())

  /** Lowers an AST type expression to its IR representation. */
  private def irType(
      origins: OriginIds,
      ty: TypeExpr
  ): Result[Type, LowerError] =
    import inox.ast.TypeExprKind.*
    ty.item match
      case Fn(params, result)      => fnType(origins, params, result, ty.span)
      case Ref(origin, mut, rType) =>
        for
          originId <- irOrigin(origins, origin)
          ty <- irType(origins, ty)
        yield Type.Ref(originId, mut, ty, rType.span)
      case I32  => Result.Success(Type.I32(ty.span))
      case Bool => Result.Success(Type.Bool(ty.span))
      case Unit => Result.Success(Type.Unit(ty.span))

  /** Lowers an AST function type expression to its IR representation. */
  private def fnType(
      origins: OriginIds,
      params: IndexedSeq[TypeExpr],
      result: TypeExpr,
      span: Span
  ): Result[Type, LowerError] =
    val paramTypes = IndexedSeq.newBuilder[Type]
    val errorBuilder = IndexedSeq.newBuilder[LowerError]

    for param <- params do
      irType(origins, param) match
        case Result.Success(ty)     => paramTypes += ty
        case Result.Failure(errors) => errorBuilder ++= errors

    irType(origins, result) match
      case Result.Success(ty) =>
        val errors = errorBuilder.result()
        if errors.nonEmpty then Result.Failure(errors)
        else Result.Success(Type.Fn(paramTypes.result(), ty, span))
      case Result.Failure(errors) =>
        errorBuilder ++= errors
        Result.Failure(errorBuilder.result())

  /** Lowers an AST origin to its IR representation. */
  private def irOrigin(
      origins: OriginIds,
      origin: Option[Name]
  ): Result[Option[OriginId], LowerError] =
    origin match
      case Some(name) =>
        if origins.contains(name.item) then
          Result.Success(Some(origins(name.item)))
        else Result.Failure(IndexedSeq(LowerError.UndefinedOrigin(name)))
      case None => Result.Success(None)

/** An AST to IR lowerer. */
private class Lowerer(globals: Globals):
  private var originIds = globals.head._2.origins
  private val localIds = SymbolTable[LocalId]()
  private val locals = mutable.IndexedBuffer[Local]()

  /** Lowers an expression to its IR representation. */
  private def irExpr(expr: Expr): Result[(Block, Operand, Type), LowerError] =
    expr.item match
      case ExprKind.Block(body)          => ???
      case ExprKind.If(cond, thn, els)   => ???
      case ExprKind.Call(callee, args)   => callExpr(callee, args, expr.span)
      case ExprKind.Borrow(mut, expr)    => borrowExpr(mut, expr, expr.span)
      case ExprKind.Binary(op, lhs, rhs) => binaryExpr(op, lhs, rhs, expr.span)
      case ExprKind.Unary(op, expr)      =>
        op match
          case UnaryOp.Deref => derefExpr(expr, expr.span)
          case UnaryOp.Not   => unaryExpr(UnOp.Not, expr, expr.span)
          case UnaryOp.Neg   => unaryExpr(UnOp.Neg, expr, expr.span)
      case ExprKind.Var(name, origins) =>
        for (operand, ty) <- varExpr(name, origins, expr.span)
        yield (IndexedSeq(), operand, ty)
      case ExprKind.IntLit(value) =>
        Result.Success(
          (IndexedSeq(), Operand.I32(value, expr.span), Type.I32(expr.span))
        )
      case ExprKind.BoolLit(value) =>
        Result.Success(
          (IndexedSeq(), Operand.Bool(value, expr.span), Type.Bool(expr.span))
        )
      case ExprKind.Unit =>
        Result.Success(
          (IndexedSeq(), Operand.Unit(expr.span), Type.Unit(expr.span))
        )

  /** Lowers a block expression to its IR representation. */
  private def blockExpr(
      block: BlockExpr
  ): Result[(Block, Operand, Type), LowerError] = ???

  /** Lowers an if expression to its IR representation. */
  private def ifExpr(
      cond: Expr,
      thn: Spanned[BlockExpr],
      els: Expr,
      span: Span
  ): Result[(Block, Operand, Type), LowerError] =
    for
      (condBlock, condOperand, _) <- irExpr(cond)
      (thenBlock, thenOperand, ty) <- blockExpr(thn.item)
      (elseBlock, elseOperand, _) <- irExpr(els)
    yield
      locals += Local(true, ty)
      val target = Place.Var(locals.length - 1, span)
      val block = condBlock :+ Instr.If(
        condOperand,
        thenBlock :+ Instr.Assign(target, thenOperand),
        elseBlock :+ Instr.Assign(target, elseOperand)
      )
      (block, Operand.Place(target.item, target.span), ty)

  /** Lowers a call expression to its IR representation. */
  private def callExpr(
      callee: Expr,
      args: IndexedSeq[Expr],
      span: Span
  ): Result[(Block, Operand, Type), LowerError] =
    irExpr(callee).flatMap { case (block, operand, ty) =>
      val instrs = IndexedSeq.newBuilder[Instr]
      instrs ++= block
      val operands = IndexedSeq.newBuilder[Operand]
      val errorBuilder = IndexedSeq.newBuilder[LowerError]

      for arg <- args do
        irExpr(arg) match
          case Result.Success((argBlock, argOperand, _)) =>
            instrs ++= argBlock
            operands += argOperand
          case Result.Failure(errors) => errorBuilder ++= errors

      val resultType =
        ty.value.item match
          case TypeKind.Fn(params, result) => result
          case _                           =>
            errorBuilder += InvalidCallee(ty)
            Type.Unit(span)

      locals += Local(true, resultType)
      val target = Place.Var(locals.length - 1, span)
      instrs += Instr.Call(target, operand, operands.result())

      val errors = errorBuilder.result()
      if errors.nonEmpty then Result.Failure(errors)
      else
        Result.Success(
          (instrs.result(), Operand.Place(target.item, target.span), resultType)
        )
    }

  /** Lowers a borrow expression to its IR representation. */
  private def borrowExpr(
      mutable: Boolean,
      expr: Expr,
      span: Span
  ): Result[(Block, Operand, Type), LowerError] =
    for (block, operand, ty) <- irExpr(expr)
    yield
      val (instrs, source) = asPlace(operand, ty)
      val targetType = Type.Ref(None, mutable, ty, span)
      locals += Local(true, ty)
      val target = Place.Var(locals.length - 1, span)
      (
        (block :++ instrs) :+ Instr.Borrow(target, mutable, source),
        Operand.Place(target.item, target.span),
        targetType
      )

  /** Lowers a binary expression to its IR representation. */
  private def binaryExpr(
      op: BinaryOp,
      lhs: Expr,
      rhs: Expr,
      span: Span
  ): Result[(Block, Operand, Type), LowerError] =
    for
      (lhsBlock, lhsOperand, _) <- irExpr(lhs)
      (rhsBlock, rhsOperand, _) <- irExpr(rhs)
    yield
      val ty =
        op match
          case BinaryOp.Add | BinaryOp.Sub | BinaryOp.Mul | BinaryOp.Div =>
            Type.I32(span)
          case _ => Type.Bool(span)
      locals += Local(true, ty)
      val place = Place.Var(locals.length - 1, span)
      (
        (lhsBlock :++ rhsBlock) :+ Instr.Binary(
          place,
          op,
          lhsOperand,
          rhsOperand
        ),
        Operand.Place(place.item, place.span),
        ty
      )

  /** Lowers a dereference expression to its IR representation. */
  private def derefExpr(
      expr: Expr,
      span: Span
  ): Result[(Block, Operand, Type), LowerError] =
    irExpr(expr).flatMap { case (block, operand, ty) =>
      (operand.item, ty.value.item) match
        case (OperandKind.Place(place), TypeKind.Ref(_, _, rType)) =>
          Result.Success(
            (
              block,
              Operand.Place(PlaceKind.Deref(Spanned(place, expr.span)), span),
              rType
            )
          )
        case (_, _) =>
          Result.Failure(IndexedSeq(LowerError.InvalidDeref(ty)))
    }

  /** Lowers a unary expression to its IR representation. */
  private def unaryExpr(
      op: UnOp,
      expr: Expr,
      span: Span
  ): Result[(Block, Operand, Type), LowerError] =
    for (block, operand, ty) <- irExpr(expr)
    yield
      locals += Local(true, ty)
      val place = Place.Var(locals.length - 1, span)
      (
        block :+ Instr.Unary(place, op, operand),
        Operand.Place(place.item, place.span),
        ty
      )

  /** Lowers a variable expression to its IR representation. */
  private def varExpr(
      name: Name,
      origins: IndexedSeq[Option[Name]],
      span: Span
  ): Result[(Operand, Type), LowerError] =
    localIds(name.item) match
      case Some(id) =>
        Result.Success(
          (Operand.Place(PlaceKind.Var(id), span), locals(id).ty)
        )
      case None =>
        globals.get(name.item) match
          case Some((_, ty)) =>
            for args <- originArgs(origins)
            yield (Operand.Fn(name, args, span), ty.substitute(args))
          case None =>
            Result.Failure(IndexedSeq(LowerError.UndefinedName(name)))

  /** Lowers a sequence of named origin arguments to origin ids. */
  private def originArgs(
      args: IndexedSeq[Option[Name]]
  ): Result[IndexedSeq[Option[OriginId]], LowerError] =
    val ids = IndexedSeq.newBuilder[Option[OriginId]]
    val errorBuilder = IndexedSeq.newBuilder[LowerError]

    for arg <- args do
      Lowerer.irOrigin(originIds, arg) match
        case Result.Success(id)     => ids += id
        case Result.Failure(errors) => errorBuilder ++= errors

    val errors = errorBuilder.result()
    if errors.nonEmpty then Result.Failure(errors)
    else Result.Success(ids.result())

  /** Converts an instruction operand into a place expression. */
  private def asPlace(operand: Operand, ty: Type): (Block, Place) =
    operand.item match
      case OperandKind.Place(place) =>
        (IndexedSeq(), Spanned(place, operand.span))
      case _ =>
        locals += Local(true, ty)
        val place = Place.Var(locals.length - 1, operand.span)
        (IndexedSeq(Instr.Assign(place, operand)), place)
