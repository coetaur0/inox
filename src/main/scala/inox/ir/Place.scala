package inox.ir

import inox.{Span, Spanned}

/** A local (function parameter or variable) identifier. */
type LocalId = Int

/** A place expression. */
type Place = Spanned[PlaceKind]

object Place:
  def Deref(place: Place, span: Span): Place =
    Spanned(PlaceKind.Deref(place), span)

  def Var(id: LocalId, span: Span): Place =
    Spanned(PlaceKind.Var(id), span)

/** A place expression's kind. */
enum PlaceKind:
  case Deref(place: Place)
  case Var(id: LocalId)
