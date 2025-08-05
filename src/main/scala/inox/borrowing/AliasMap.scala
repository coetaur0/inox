package inox.borrowing

import inox.ir.LocalId

/** A set of aliases. */
type AliasSet = Set[(Boolean, LocalId)]

/** A mapping binding local ids to the set of ids they may alias. */
class AliasMap(
    val bindings: IndexedSeq[AliasSet] = IndexedSeq.empty
):
  /** Returns the set of aliases for some local id. */
  def apply(id: LocalId): AliasSet = bindings(id)

  /** Returns a new alias map where the bindings for a set of local ids have
    * been updated to a new set of aliases.
    */
  def updated(ids: Set[LocalId], aliases: AliasSet): AliasMap =
    AliasMap(
      bindings.zipWithIndex
        .map((set, index) => if ids.contains(index) then aliases else set)
    )

  /** Checks of two alias maps are equal. */
  def ===(that: AliasMap): Boolean =
    bindings == that.bindings

  /** Alias for `union`. */
  def |(that: AliasMap): AliasMap = union(that)

  /** Returns the union of two alias maps. */
  private def union(that: AliasMap): AliasMap =
    AliasMap(bindings.zip(that.bindings).map((lhs, rhs) => lhs | rhs))
