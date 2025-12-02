package inox.analysis

import inox.ast.BinaryOp
import inox.ir.*
import inox.util.{Name, Spanned}

import scala.annotation.tailrec

/** An alias for a variable. */
enum Alias {
  case Variable(local: LocalId)
  case Undefined
  case None

  override def toString: String = this match {
    case Variable(local) => s"$local"
    case Undefined       => "⊥"
    case None            => "⊤"
  }
}

/** A map from local ids to the variables they alias. */
type AliasMap = IndexedSeq[Alias]

/** An aliasing state for alias analysis. */
type AliasState = Set[AliasMap]

/** Inox's alias analysis. */
object AliasAnalysis {

  /** Applies alias analysis on a function declaration. */
  def apply(
      module: inox.ir.Module,
      function: inox.ir.Function
  ): (IndexedSeq[Local], IndexedSeq[AliasState]) = {
    val initAliases = function.locals.map(_ => Alias.Undefined)
    val (locals, aliases) = function.locals
      .slice(1, function.paramCount + 1)
      .zipWithIndex
      .foldLeft((function.locals, initAliases)) { case ((locals, aliases), (param, id)) =>
        initParam(locals, aliases, param, id + 1)
      }
    (locals, new AliasAnalysis(module, locals)(IndexedSeq(Set(aliases)), function.body))
  }

  /** Returns the alias that an instruction operand references in a given alias map. */
  def operandAlias(aliases: AliasMap, operand: Operand): Alias = operand.item match {
    case OperandKind.Place(place) => aliases(placeAlias(aliases, place))
    case _                        => Alias.None
  }

  /** Returns the id of the local that a place expression references in a given alias map. */
  def placeAlias(aliases: AliasMap, place: PlaceKind): LocalId = place match {
    case PlaceKind.Deref(subplace) =>
      aliases(placeAlias(aliases, subplace.item)) match {
        case Alias.Variable(id) => id
        case _ => 0 // Unreachable: dereference operations can only be applied to references.
      }
    case PlaceKind.Var(id) => id
  }

  /** Extends the list of locals and the initial alias map of a function with aliasing information
    * for a parameter.
    */
  private def initParam(
      locals: IndexedSeq[Local],
      aliases: AliasMap,
      param: Local,
      id: LocalId
  ): (IndexedSeq[Local], AliasMap) = param.ty.value.item match {
    case TypeKind.Ref(_, mut, ty) => {
      val newName = Spanned("*" + param.name.item, param.ty.value.span)
      val (newLocals, newAliases) = initExtern(locals, aliases, mut, newName, ty)
      (newLocals, newAliases.updated(id, Alias.Variable(newLocals.length - 1)))
    }
    case _ => (locals, aliases)
  }

  /** Extends the locals and the initial alias map of a function with aliasing information for a
    * value of some type appearing in a function parameter's signature.
    */
  private def initExtern(
      locals: IndexedSeq[Local],
      aliases: AliasMap,
      mutable: Boolean,
      name: Name,
      externType: Type
  ): (IndexedSeq[Local], AliasMap) = externType.value.item match {
    case TypeKind.Ref(_, mut, ty) => {
      val newName = Spanned("*" + name.item, name.span)
      val (newLocals, newAliases) = initExtern(locals, aliases, mut, newName, ty)
      (newLocals :+ Local(mutable, name, ty), newAliases :+ Alias.Variable(newLocals.length - 1))
    }
    case _ => (locals :+ Local(mutable, name, externType), aliases :+ Alias.None)
  }

  /** Returns a new sequence of aliasing states with an additional state resulting from the
    * application of `f` on the last state in the input sequence.
    */
  private def addState(
      states: IndexedSeq[AliasState],
      f: AliasState => AliasState
  ): IndexedSeq[AliasState] = {
    val state = states.last
    states :+ f(state)
  }

  /** Returns a new aliasing state where the location of the `target` place expression is associated
    * with the result of a function `f` in each alias map it contains.
    */
  private def assign(state: AliasState, target: Place, f: AliasMap => Alias): AliasState =
    state.foldLeft(Set.empty[AliasMap]) { (result, aliases) =>
      val id = placeAlias(aliases, target.item)
      result + aliases.updated(id, f(aliases))
    }

  /** Returns the aliasing state that a call instruction may return in a given alias map, depending
    * on its arguments and parameter and return types.
    */
  private def callState(
      aliases: AliasMap,
      id: LocalId,
      paramTypes: Seq[Type],
      returnType: Type,
      args: Seq[Operand]
  ): AliasState =
    paramTypes.zip(args).foldLeft(Set.empty[AliasMap]) { case (result, (ty, operand)) =>
      val aliasSet = argumentAliases(aliases, ty, returnType, operand)
      aliasSet.foldLeft(result)((state, alias) => state + aliases.updated(id, alias))
    }

  /** Returns the set of aliases that a call instruction's argument may alias in a given alias map,
    * with respect to the corresponding parameter and return types.
    */
  @tailrec
  private def argumentAliases(
      aliases: AliasMap,
      paramType: Type,
      returnType: Type,
      arg: Operand
  ): Set[Alias] = (paramType.value.item, returnType.value.item) match {
    case (TypeKind.Ref(_, _, _), TypeKind.Ref(_, _, ty)) =>
      if (paramType :< returnType) {
        Set(operandAlias(aliases, arg))
      } else {
        (ty.value.item, arg.item) match {
          case (TypeKind.Ref(_, _, _), OperandKind.Place(place)) =>
            argumentAliases(aliases, ty, returnType, Operand.Place(place, arg.span))
          case (_, _) => Set.empty
        }
      }
    case (_, _) => Set.empty
  }
}

/** Inox's alias analysis. */
class AliasAnalysis(module: inox.ir.Module, locals: IndexedSeq[Local])
    extends ForwardAnalysis[AliasState] {
  import AliasAnalysis.*

  override def whileInstr(
      states: IndexedSeq[AliasState],
      cond: Operand,
      body: Block
  ): IndexedSeq[AliasState] = {
    @tailrec
    def fixpoint(state: AliasState): IndexedSeq[AliasState] = {
      val loopStates = apply(IndexedSeq(state), body)
      val nextState = loopStates.last | state
      if (nextState == state) {
        loopStates.init :+ nextState
      } else {
        fixpoint(nextState)
      }
    }
    states :++ fixpoint(states.last)
  }

  override def ifInstr(
      states: IndexedSeq[AliasState],
      cond: Operand,
      thn: Block,
      els: Block
  ): IndexedSeq[AliasState] = {
    val entryState = states.last
    val thenStates = apply(IndexedSeq(entryState), thn)
    val elseStates = apply(IndexedSeq(entryState), els)
    states :++ thenStates.init :++ elseStates.init :+ (thenStates.last | elseStates.last)
  }

  override def callInstr(
      states: IndexedSeq[AliasState],
      target: Place,
      callee: Operand,
      args: IndexedSeq[Operand]
  ): IndexedSeq[AliasState] = operandType(callee).value.item match {
    case TypeKind.Fn(paramTypes, returnType) =>
      addState(
        states,
        state =>
          state.foldLeft(Set.empty[AliasMap]) { (result, aliases) =>
            val id = placeAlias(aliases, target.item)
            result | callState(aliases, id, paramTypes, returnType, args)
          }
      )
    case _ => states :+ states.last // Unreachable.
  }

  override def borrowInstr(
      states: IndexedSeq[AliasState],
      target: Place,
      mutable: Boolean,
      source: Place
  ): IndexedSeq[AliasState] =
    addState(states, assign(_, target, aliases => Alias.Variable(placeAlias(aliases, source.item))))

  override def assignInstr(
      states: IndexedSeq[AliasState],
      target: Place,
      value: Operand
  ): IndexedSeq[AliasState] =
    addState(states, assign(_, target, aliases => operandAlias(aliases, value)))

  override def binaryInstr(
      states: IndexedSeq[AliasState],
      target: Place,
      op: BinaryOp,
      lhs: Operand,
      rhs: Operand
  ): IndexedSeq[AliasState] = addState(states, assign(_, target, _ => Alias.None))

  override def unaryInstr(
      states: IndexedSeq[AliasState],
      target: Place,
      op: UnOp,
      operand: Operand
  ): IndexedSeq[AliasState] = addState(states, assign(_, target, _ => Alias.None))

  override def returnInstr(states: IndexedSeq[AliasState]): IndexedSeq[AliasState] =
    states :+ states.last

  /** Returns the type of an instruction operand. */
  private def operandType(operand: Operand): Type = operand.item match {
    case OperandKind.Place(place)      => placeType(place)
    case OperandKind.Fn(name, origins) => module(name.item).ty.substitute(origins)
    case OperandKind.I32(value)        => Type.I32(operand.span)
    case OperandKind.Bool(value)       => Type.Bool(operand.span)
    case OperandKind.Unit              => Type.Unit(operand.span)
  }

  /** Returns the type of a place expression. */
  private def placeType(place: PlaceKind): Type = place match {
    case PlaceKind.Deref(p) =>
      placeType(p.item).value.item match {
        case TypeKind.Ref(_, _, ty) => ty
        case _                      => Type.Unit(p.span) // Unreachable.
      }
    case PlaceKind.Var(id) => locals(id).ty
  }
}
