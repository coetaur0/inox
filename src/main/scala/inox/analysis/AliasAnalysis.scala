package inox.analysis

import scala.annotation.tailrec
import inox.{Name, Spanned}
import inox.ast.BinaryOp
import inox.ir.*

/** Alias analysis for Inox. */
object AliasAnalysis {

  /** Applies alias analysis on a function declaration. */
  def apply(
      module: inox.ir.Module,
      function: inox.ir.Function
  ): (IndexedSeq[Local], IndexedSeq[AliasMap]) = {
    val (locals, aliases) = function.locals
      .slice(1, function.paramCount + 1)
      .zipWithIndex
      .foldLeft((function.locals, AliasMap.init(function.locals)))(
        (result: (IndexedSeq[Local], AliasMap), param) =>
          initParam(result._1, result._2, param._1, param._2 + 1)
      )

    (
      locals,
      new AliasAnalysis(module, locals)(IndexedSeq(aliases), function.body)
    )
  }

  /** Returns the set of local ids that an instruction operand may alias.
    */
  def operandAliases(aliases: AliasMap, operand: Operand): AliasSet =
    operand.item match {
      case OperandKind.Place(place) =>
        placeAliases(aliases, place).foldLeft(Set.empty) { (result, id) =>
          result | aliases(id)._2
        }
      case _ => Set.empty
    }

  /** Returns the set of local ids that a place expression may alias. */
  def placeAliases(aliases: AliasMap, place: PlaceKind): AliasSet =
    place match {
      case PlaceKind.Deref(place) =>
        placeAliases(aliases, place.item).foldLeft(Set.empty) { (result, id) =>
          result | aliases(id)._2
        }
      case PlaceKind.Var(id) => Set(id)
    }

  /** Returns the set of local ids that the result of a call expression may
    * alias.
    */
  @tailrec
  private def resultAliases(
      aliases: AliasMap,
      paramType: Type,
      resultType: Type,
      arg: Operand
  ): AliasSet =
    (paramType.value.item, resultType.value.item) match {
      case (TypeKind.Ref(_, _, _), TypeKind.Ref(_, _, rType)) =>
        if paramType :< resultType then operandAliases(aliases, arg)
        else
          (rType.value.item, arg.item) match {
            case (TypeKind.Ref(_, _, _), OperandKind.Place(place)) =>
              resultAliases(
                aliases,
                rType,
                resultType,
                Operand.Place(place, arg.span)
              )
            case (_, _) => Set.empty
          }
      case _ => Set.empty
    }

  /** Extends the list of locals and the initial alias map of a function with
    * aliasing information for a parameter.
    */
  private def initParam(
      locals: IndexedSeq[Local],
      aliases: AliasMap,
      param: Local,
      id: LocalId
  ): (IndexedSeq[Local], AliasMap) =
    param.ty.value.item match {
      case TypeKind.Ref(_, mutable, rType) =>
        val (newLocals, newAliases) =
          initExtern(
            locals,
            aliases,
            mutable,
            Spanned("*" + param.name.item, param.ty.value.span),
            rType
          )
        (
          newLocals,
          newAliases.updated(id, Set(newLocals.length - 1))
        )
      case _ => (locals, aliases)
    }

  /** Extends the locals and the initial alias map of a function with aliasing
    * information for a value of some type appearing in a function parameter's
    * signature.
    */
  private def initExtern(
      locals: IndexedSeq[Local],
      aliases: AliasMap,
      mutable: Boolean,
      name: Name,
      ty: Type
  ): (IndexedSeq[Local], AliasMap) =
    ty.value.item match {
      case TypeKind.Ref(_, mut, rType) =>
        val (newLocals, newAliases) = initExtern(
          locals,
          aliases,
          mut,
          Spanned("*" + name.item, name.span),
          rType
        )
        (
          newLocals :+ Local(mutable, name, ty),
          newAliases :+ (mut, Set(newLocals.length - 1))
        )
      case _ =>
        (locals :+ Local(mutable, name, ty), aliases :+ (false, Set.empty))
    }

}

/** Alias analysis for an Inox function. */
private class AliasAnalysis(module: inox.ir.Module, locals: IndexedSeq[Local])
    extends ForwardAnalysis[AliasMap] {

  override def whileInstr(
      states: IndexedSeq[AliasMap],
      cond: Operand,
      body: Block
  ): IndexedSeq[AliasMap] = {
    @tailrec
    def fixpoint(state: AliasMap): IndexedSeq[AliasMap] = {
      val states = apply(IndexedSeq(state), body)
      val nextState = states.last | state
      if nextState == state then states.tail :+ nextState
      else fixpoint(nextState)
    }

    states :++ fixpoint(states.last)
  }

  override def ifInstr(
      states: IndexedSeq[AliasMap],
      cond: Operand,
      thn: Block,
      els: Block
  ): IndexedSeq[AliasMap] = {
    val state = states.last
    val thnStates = apply(IndexedSeq(state), thn)
    val elsStates = apply(IndexedSeq(state), els)
    val join = thnStates.last | elsStates.last
    states :++ thnStates.tail :++ elsStates.tail :+ join
  }

  override def callInstr(
      states: IndexedSeq[AliasMap],
      target: Place,
      callee: Operand,
      args: IndexedSeq[Operand]
  ): IndexedSeq[AliasMap] = {
    val state = states.last
    operandType(callee).value.item match {
      case TypeKind.Fn(params, result) =>
        val aliasSet: AliasSet =
          params.zip(args).foldLeft(Set.empty) { (aliases, arg) =>
            aliases | AliasAnalysis
              .resultAliases(state, arg._1, result, arg._2)
          }
        states :+ state.updated(
          AliasAnalysis.placeAliases(state, target.item),
          aliasSet
        )
      case _ => states :+ state // Unreachable.
    }
  }

  override def borrowInstr(
      states: IndexedSeq[AliasMap],
      target: Place,
      mutable: Boolean,
      source: Place
  ): IndexedSeq[AliasMap] = {
    val state = states.last
    val aliasSet: AliasSet =
      AliasAnalysis.placeAliases(state, source.item).foldLeft(Set.empty) {
        (result, id) => result + id
      }
    states :+ state.updated(
      AliasAnalysis.placeAliases(state, target.item),
      aliasSet
    )
  }

  override def assignInstr(
      states: IndexedSeq[AliasMap],
      target: Place,
      value: Operand
  ): IndexedSeq[AliasMap] = {
    val state = states.last
    states :+ state.updated(
      AliasAnalysis.placeAliases(state, target.item),
      AliasAnalysis.operandAliases(state, value)
    )
  }

  override def binaryInstr(
      states: IndexedSeq[AliasMap],
      target: Place,
      op: BinaryOp,
      lhs: Operand,
      rhs: Operand
  ): IndexedSeq[AliasMap] =
    states :+ states.last

  override def unaryInstr(
      states: IndexedSeq[AliasMap],
      target: Place,
      op: UnOp,
      operand: Operand
  ): IndexedSeq[AliasMap] =
    states :+ states.last

  override def returnInstr(
      states: IndexedSeq[AliasMap]
  ): IndexedSeq[AliasMap] =
    states :+ states.last

  /** Returns the type of an instruction operand. */
  private def operandType(operand: Operand): Type =
    operand.item match {
      case OperandKind.Place(place)      => placeType(place)
      case OperandKind.Fn(name, origins) =>
        module(name.item).ty.substitute(origins)
      case OperandKind.I32(value)  => Type.I32(operand.span)
      case OperandKind.Bool(value) => Type.Bool(operand.span)
      case OperandKind.Unit        => Type.Unit(operand.span)
    }

  /** Returns the type of a place expression. */
  private def placeType(place: PlaceKind): Type =
    place match {
      case PlaceKind.Deref(p) =>
        placeType(p.item).value.item match {
          case TypeKind.Ref(_, _, rType) => rType
          case _                         => Type.Unit(p.span) // Unreachable.
        }
      case PlaceKind.Var(id) => locals(id).ty
    }

}
