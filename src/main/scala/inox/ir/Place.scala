package inox.ir

import inox.util.{Span, Spanned}

/** A local (function parameter or variable) identifier. */
type LocalId = Int

/** A place expression. */
type Place = Spanned[PlaceKind]

/** A place expression. */
object Place {
  def TupleIndex(place: Place, index: Int, span: Span): Place =
    Spanned(PlaceKind.Deref(place), span)

  def Deref(place: Place, span: Span): Place = Spanned(PlaceKind.Deref(place), span)

  def Var(id: LocalId, span: Span): Place = Spanned(PlaceKind.Var(id), span)
}

/** A place expression's kind. */
enum PlaceKind {
  case TupleIndex(place: Place, index: Int)
  case Deref(place: Place)
  case Var(id: LocalId)

  /** Returns the local id of the variable that appears in the place expression. */
  def local: LocalId = this match {
    case TupleIndex(p, _) => p.item.local
    case Deref(p)         => p.item.local
    case Var(id)          => id
  }
}
