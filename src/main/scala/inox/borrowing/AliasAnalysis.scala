package inox.borrowing

import scala.annotation.tailrec
import inox.ir.*

/** Alias analysis for Inox. */
object AliasAnalysis {

  /** Returns the set of local ids that an instruction operand may alias, along
    * with their mutability.
    */
  private def aliasOperand(aliases: AliasMap, operand: Operand): AliasSet =
    operand.item match {
      case OperandKind.Place(place) =>
        aliasPlace(aliases, place).foldLeft(Set.empty)((result, id) =>
          result | aliases(id)
        )
      case _ => Set.empty
    }

  /** Returns the set of local ids that a place expression may alias. */
  private def aliasPlace(aliases: AliasMap, place: PlaceKind): Set[LocalId] =
    place match {
      case PlaceKind.Deref(place) =>
        aliasPlace(aliases, place.item).foldLeft(Set.empty)((result, id) =>
          result | aliases(id).map(_._2)
        )
      case PlaceKind.Var(id) => Set(id)
    }

  /** Returns the set of local ids that the result of a call expression may
    * alias.
    */
  @tailrec
  private def aliasResult(
      aliases: AliasMap,
      paramType: Type,
      resultType: Type,
      arg: Operand
  ): AliasSet =
    (paramType.value.item, resultType.value.item) match {
      case (TypeKind.Ref(_, _, _), TypeKind.Ref(_, _, rType)) =>
        if paramType :< resultType then aliasOperand(aliases, arg)
        else
          (rType.value.item, arg.item) match {
            case (TypeKind.Ref(_, _, _), OperandKind.Place(place)) =>
              aliasResult(
                aliases,
                rType,
                resultType,
                Operand.Place(place, arg.span)
              )
            case (_, _) => Set.empty
          }
      case _ => Set.empty
    }

  def initParams(
      function: Function
  ): (IndexedSeq[Local], IndexedSeq[AliasSet]) = {
    var locals = function.locals
    var aliases: IndexedSeq[AliasSet] = function.locals.map(_ => Set.empty)

    function.locals
      .slice(1, function.paramCount + 1)
      .zipWithIndex
      .foldLeft((function.locals, function.locals.map(_ => Set.empty)))(
        (result, param) =>
          initParam(result._1, result._2, param._1, param._2 + 1)
      )
  }

  private def initParam(
      locals: IndexedSeq[Local],
      aliases: IndexedSeq[AliasSet],
      param: Local,
      id: LocalId
  ): (IndexedSeq[Local], IndexedSeq[AliasSet]) =
    param.ty.value.item match {
      case TypeKind.Ref(_, mutable, rType) =>
        val (newLocals, newAliases) =
          initType(locals, aliases, mutable, rType)
        (
          newLocals,
          newAliases.updated(id, Set((mutable, newLocals.length - 1)))
        )
      case _ => (locals, aliases)
    }

  private def initType(
      locals: IndexedSeq[Local],
      aliases: IndexedSeq[AliasSet],
      mutable: Boolean,
      ty: Type
  ): (IndexedSeq[Local], IndexedSeq[AliasSet]) =
    ty.value.item match {
      case TypeKind.Ref(_, mut, rType) =>
        val (newLocals, newAliases) = initType(locals, aliases, mut, rType)
        (
          newLocals :+ Local(mutable, ty),
          newAliases :+ Set((mut, newLocals.length - 1))
        )
      case _ => (locals :+ Local(mutable, ty), aliases :+ Set.empty)
    }
}

/** Alias analysis for Inox. */
private class AliasAnalysis(module: inox.ir.Module) {

  /** Returns the alias maps for a function declaration's body. */
  private def aliasFunction(function: Function): IndexedSeq[AliasMap] =
    ???

  /** Returns the alias maps for a block of instructions. */
  private def aliasBlock(
      locals: IndexedSeq[Local],
      before: AliasMap,
      block: Block
  ): IndexedSeq[AliasMap] = {
    var aliases = before
    block.foldLeft(IndexedSeq.empty) { (result, instr) =>
      val after = aliasInstr(locals, aliases, instr)
      aliases = after.lastOption.getOrElse(aliases)
      result :++ after
    }
  }

  /** Returns the alias maps for an instruction. */
  private def aliasInstr(
      locals: IndexedSeq[Local],
      before: AliasMap,
      instr: Instr
  ): IndexedSeq[AliasMap] =
    instr match {
      case Instr.While(cond, body)          => aliasWhile(locals, before, body)
      case Instr.If(cond, thn, els)         => aliasIf(locals, before, thn, els)
      case Instr.Call(target, callee, args) =>
        aliasCall(locals, before, target, callee, args)
      case Instr.Borrow(target, mutable, source) =>
        aliasBorrow(before, target, mutable, source)
      case Instr.Assign(target, value) =>
        IndexedSeq(
          before.updated(
            AliasAnalysis.aliasPlace(before, target.item),
            AliasAnalysis.aliasOperand(before, value)
          )
        )
      case Instr.Binary(_, _, _, _) | Instr.Unary(_, _, _) | Instr.Return =>
        IndexedSeq(before)
    }

  /** Returns the alias maps for a while instruction. */
  private def aliasWhile(
      locals: IndexedSeq[Local],
      before: AliasMap,
      body: Block
  ): IndexedSeq[AliasMap] = {
    @tailrec
    def fixpoint(before: AliasMap): IndexedSeq[AliasMap] = {
      val bodyAliases = aliasBlock(locals, before, body)
      val after = bodyAliases.lastOption.getOrElse(before) | before
      if before === after then bodyAliases :+ after
      else fixpoint(after)
    }

    fixpoint(before)
  }

  /** Returns the alias maps for an if instruction. */
  private def aliasIf(
      locals: IndexedSeq[Local],
      before: AliasMap,
      thn: Block,
      els: Block
  ): IndexedSeq[AliasMap] = {
    val thenAliases = aliasBlock(locals, before, thn)
    val elseAliases = aliasBlock(locals, before, els)
    val join = thenAliases.lastOption.getOrElse(before)
      | elseAliases.lastOption.getOrElse(before)
    thenAliases :++ elseAliases :+ join
  }

  /** Returns the alias maps for a call instruction. */
  private def aliasCall(
      locals: IndexedSeq[Local],
      before: AliasMap,
      target: Place,
      callee: Operand,
      args: IndexedSeq[Operand]
  ): IndexedSeq[AliasMap] =
    operandType(locals, callee).value.item match {
      case TypeKind.Fn(params, result) =>
        val aliasSet: AliasSet = params
          .zip(args)
          .foldLeft(Set.empty)((aliases, arg) =>
            aliases | AliasAnalysis
              .aliasResult(before, arg._1, result, arg._2)
          )
        IndexedSeq(
          before.updated(
            AliasAnalysis.aliasPlace(before, target.item),
            aliasSet
          )
        )
      case _ => IndexedSeq(before) // Unreachable.
    }

  /** Returns the alias maps for a borrow instruction. */
  private def aliasBorrow(
      before: AliasMap,
      target: Place,
      mutable: Boolean,
      source: Place
  ): IndexedSeq[AliasMap] = {
    val aliasSet: AliasSet =
      AliasAnalysis
        .aliasPlace(before, source.item)
        .foldLeft(Set.empty)((result, id) => result + ((mutable, id)))
    IndexedSeq(
      before.updated(AliasAnalysis.aliasPlace(before, target.item), aliasSet)
    )
  }

  /** Returns the type of an operand in a given context. */
  private def operandType(
      locals: IndexedSeq[Local],
      operand: Operand
  ): Type =
    operand.item match {
      case OperandKind.Place(place)      => placeType(locals, place)
      case OperandKind.Fn(name, origins) =>
        module(name.item).ty.substitute(origins)
      case OperandKind.I32(value)  => Type.I32(operand.span)
      case OperandKind.Bool(value) => Type.Bool(operand.span)
      case OperandKind.Unit        => Type.Unit(operand.span)
    }

  /** Returns the type of a place in a given context. */
  private def placeType(locals: IndexedSeq[Local], place: PlaceKind): Type =
    place match {
      case PlaceKind.Deref(p) =>
        placeType(locals, p.item).value.item match {
          case TypeKind.Ref(_, _, rType) => rType
          case _                         => Type.Unit(p.span) // Unreachable.
        }
      case PlaceKind.Var(id) => locals(id).ty
    }
}
