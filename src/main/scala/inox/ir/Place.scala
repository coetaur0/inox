package inox.ir

import inox.{Span, Spanned}

/** A local (function parameter or variable) identifier. */
type LocalId = Int

/** A place expression. */
type Place = Spanned[PlaceKind]

/** A place expression. */
object Place {
  def Deref(place: Place, span: Span): Place =
    Spanned(PlaceKind.Deref(place), span)

  def Var(id: LocalId, span: Span): Place =
    Spanned(PlaceKind.Var(id), span)
}

/** A place expression's kind. */
enum PlaceKind {
  case Deref(place: Place)
  case Var(id: LocalId)

  /** Returns the local id of the variable that appears in the place expression.
    */
  def local: LocalId =
    this match {
      case Deref(p) => p.item.local
      case Var(id)  => id
    }
}
