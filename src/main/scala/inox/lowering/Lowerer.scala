package inox.lowering

import inox.{Result, Span}
import inox.ast.{FnDecl, Name, TypeExpr}
import inox.ir.{OriginId, Type}

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
private class Lowerer(globals: Globals)
