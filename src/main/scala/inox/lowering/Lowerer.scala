package inox.lowering

import scala.collection.mutable
import inox.{Name, Result, Span, Spanned, ast}
import inox.ast.*
import inox.ir.*

/** A mapping from function names to their declared origins and type. */
private type Globals = Map[String, (origins: OriginIds, ty: Type)]

/** A mapping from origin names to their ids. */
private type OriginIds = Map[String, OriginId]

/** An AST to IR lowerer. */
object Lowerer:
  /** Lowers a module declaration to its IR representation. */
  def module(moduleDecl: ModuleDecl): Result[Module, LowerError] =
    globals(moduleDecl).flatMap { globals =>
      Lowerer(globals).irModule(moduleDecl)
    }

  /** Returns a mapping from function names to their declared origin ids and
    * type.
    */
  private def globals(
      moduleDecl: ModuleDecl
  ): Result[Globals, LowerError] =
    val globals = Map.newBuilder[String, (origins: OriginIds, ty: Type)]
    val errorBuilder = IndexedSeq.newBuilder[LowerError]

    for (name, function) <- moduleDecl do
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
          ty <- irType(origins, rType)
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

  /** Lowers a module declaration to its IR representation. */
  private def irModule(moduleDecl: ModuleDecl): Result[Module, LowerError] =
    val functions = Map.newBuilder[String, Function]
    val errorBuilder = IndexedSeq.newBuilder[LowerError]

    for (name, fnDecl) <- moduleDecl do
      irFunction(name, fnDecl) match
        case Result.Success(function) => functions += (name -> function)
        case Result.Failure(errors)   => errorBuilder ++= errors

    val errors = errorBuilder.result()
    if errors.nonEmpty then Result.Failure(errors)
    else Result.Success(functions.result())

  /** Lowers a function declaration to its IR representation. */
  private def irFunction(
      name: String,
      fnDecl: FnDecl
  ): Result[Function, LowerError] =
    originIds = globals(name).origins
    localIds.clear()
    locals.clear()

    val errorBuilder = IndexedSeq.newBuilder[LowerError]

    Lowerer.irType(originIds, fnDecl.result) match
      case Result.Success(ty)     => locals += Local(true, ty)
      case Result.Failure(errors) => errorBuilder ++= errors

    for param <- fnDecl.parameters do
      if !(localIds += (param.name.item, locals.length)) then
        errorBuilder += LowerError.DuplicateParameter(param.name)
      else
        Lowerer.irType(originIds, param.ty) match
          case inox.Result.Success(ty)     => locals += Local(param.mutable, ty)
          case inox.Result.Failure(errors) => errorBuilder ++= errors

    val errors = errorBuilder.result()
    if errors.nonEmpty then Result.Failure(errors)
    else
      for (block, operand, _) <- blockExpr(fnDecl.body)
      yield Function(
        originIds.size,
        fnDecl.parameters.length,
        locals.toIndexedSeq,
        block :+ Instr.Assign(Place.Var(0, operand.span), operand)
      )

  /** Lowers a statement to its IR representation. */
  private def irStmt(stmt: Stmt): Result[Block, LowerError] =
    stmt.item match
      case ast.StmtKind.While(cond, body) => whileStmt(cond, body.item)
      case ast.StmtKind.Let(mutable, name, ty, value) =>
        letStmt(mutable, name, ty, value)
      case ast.StmtKind.Assign(lhs, rhs) => assignStmt(lhs, rhs)
      case ast.StmtKind.Return(value)    => returnStmt(value)
      case ast.StmtKind.ExprStmt(kind)   =>
        irExpr(Spanned(kind, stmt.span)).map(_._1)

  /** Lowers a while statement to its IR representation. */
  private def whileStmt(
      cond: Expr,
      body: BlockExpr
  ): Result[Block, LowerError] =
    for
      (condBlock, condOperand, _) <- irExpr(cond)
      (bodyBlock, _, _) <- blockExpr(body)
    yield condBlock :+ Instr.While(condOperand, bodyBlock)

  /** Lowers a let statement to its IR representation. */
  private def letStmt(
      mutable: Boolean,
      name: Name,
      ty: Option[TypeExpr],
      value: Option[Expr]
  ): Result[Block, LowerError] =
    (ty.map(t => Lowerer.irType(originIds, t)), value.map(v => irExpr(v))) match
      case (Some(t), Some(v)) =>
        for
          ty <- t
          (block, operand, _) <- v
        yield
          locals += Local(mutable, ty)
          localIds += (name.item, locals.length - 1)
          block :+ Instr.Assign(
            Place.Var(locals.length - 1, name.span),
            operand
          )
      case (Some(t), None) =>
        for ty <- t
        yield
          locals += Local(mutable, ty)
          localIds += (name.item, locals.length - 1)
          IndexedSeq()
      case (None, Some(v)) =>
        for (block, operand, ty) <- v
        yield
          locals += Local(mutable, ty)
          localIds += (name.item, locals.length - 1)
          block :+ Instr.Assign(
            Place.Var(locals.length - 1, name.span),
            operand
          )
      case (None, None) =>
        Result.Failure(IndexedSeq(LowerError.UndefinedType(name)))

  /** Lowers an assignment statement to its IR representation. */
  private def assignStmt(lhs: Expr, rhs: Expr): Result[Block, LowerError] =
    irExpr(lhs).flatMap { case (lhsBlock, lhsOperand, _) =>
      irExpr(rhs).flatMap { case (rhsBlock, rhsOperand, _) =>
        lhsOperand.item match
          case OperandKind.Place(place) =>
            Result.Success(
              (rhsBlock :++ lhsBlock) :+ Instr.Assign(
                Spanned(place, lhs.span),
                rhsOperand
              )
            )
          case _ =>
            Result.Failure(IndexedSeq(LowerError.UnassignableExpr(lhs.span)))
      }
    }

  /** Lowers a return statement to its IR representation. */
  private def returnStmt(value: Expr): Result[Block, LowerError] =
    for (block, operand, _) <- irExpr(value)
    yield (block :+ Instr.Assign(
      Place.Var(0, value.span),
      operand
    )) :+ Instr.Return

  /** Lowers an expression to its IR representation. */
  private def irExpr(expr: Expr): Result[(Block, Operand, Type), LowerError] =
    expr.item match
      case ExprKind.Block(body)          => blockExpr(body)
      case ExprKind.If(cond, thn, els)   => ifExpr(cond, thn, els, expr.span)
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
  ): Result[(Block, Operand, Type), LowerError] =
    val blockBuilder = IndexedSeq.newBuilder[Instr]
    val errorBuilder = IndexedSeq.newBuilder[LowerError]
    localIds.push(true)

    for stmt <- block.stmts do
      irStmt(stmt) match
        case inox.Result.Success(item)   => blockBuilder ++= item
        case inox.Result.Failure(errors) => errorBuilder ++= errors

    irExpr(block.result) match
      case inox.Result.Success((instrs, operand, ty)) =>
        blockBuilder ++= instrs
        localIds.pop()
        val errors = errorBuilder.result()
        if errors.nonEmpty then Result.Failure(errors)
        else Result.Success((blockBuilder.result(), operand, ty))
      case inox.Result.Failure(errors) =>
        errorBuilder ++= errors
        localIds.pop()
        Result.Failure(errorBuilder.result())

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
            errorBuilder += LowerError.InvalidCallee(ty)
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
