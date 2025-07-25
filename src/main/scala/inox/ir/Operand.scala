package inox.ir

import inox.{Name, Span, Spanned}

/** An instruction operand. */
type Operand = Spanned[OperandKind]

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
