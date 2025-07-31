package inox.ir

import inox.{Name, Span, Spanned}

/** An instruction operand. */
type Operand = Spanned[OperandKind]

/** An instruction operand. */
object Operand:
  def Place(place: PlaceKind, span: Span): Operand =
    Spanned(OperandKind.Place(place), span)

  def Fn(
      name: Name,
      origins: IndexedSeq[Option[OriginId]],
      span: Span
  ): Operand =
    Spanned(OperandKind.Fn(name, origins), span)

  def I32(value: Int, span: Span): Operand =
    Spanned(OperandKind.I32(value), span)

  def Bool(value: Boolean, span: Span): Operand =
    Spanned(OperandKind.Bool(value), span)

  def Unit(span: Span): Operand =
    Spanned(OperandKind.Unit, span)

/** An instruction operand's kind. */
enum OperandKind:
  case Place(place: PlaceKind)
  case Fn(name: Name, origins: IndexedSeq[Option[OriginId]])
  case I32(value: Int)
  case Bool(value: Boolean)
  case Unit

  /** Returns the set of local ids of the variables that appear in the operand.
    */
  def locals: Set[LocalId] =
    this match
      case Place(place)                       => Set(place.local)
      case Fn(_, _) | I32(_) | Bool(_) | Unit => Set.empty
