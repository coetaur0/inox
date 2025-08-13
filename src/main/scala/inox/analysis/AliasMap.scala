package inox.analysis

import inox.ir.{Local, LocalId, TypeKind}

/** A set of aliases. */
type AliasSet = Set[LocalId]

/** A mapping binding local ids to the set of ids they may alias. */
object AliasMap {

  /** Initialises an alias map from a sequence of local declarations. */
  def init(locals: IndexedSeq[Local]): AliasMap =
    AliasMap(locals.map { local =>
      local.ty.value.item match {
        case TypeKind.Ref(_, mut, _) => (mut, Set())
        case _                       => (false, Set())
      }
    })

}

/** A mapping binding local ids to the set of ids they may alias. */
class AliasMap(val bindings: IndexedSeq[(Boolean, AliasSet)]) {

  override def equals(that: Any): Boolean =
    that match {
      case that: AliasMap => this.bindings == that.bindings
      case _              => false
    }

  override def toString: String = {
    val contents = bindings.zipWithIndex
      .map { (info, index) => s"($index) -> (${info._1}, ${info._2})" }
      .mkString(", ")
    s"{$contents}"
  }

  /** Returns the mutability and the set of aliases for some local id. */
  def apply(id: LocalId): (Boolean, AliasSet) = bindings(id)

  /** Returns a new alias map where the binding for a specific local id has been
    * updated to a new set of aliases.
    */
  def updated(id: LocalId, aliases: AliasSet): AliasMap =
    AliasMap(bindings.updated(id, (bindings(id)._1, aliases)))

  /** Returns a new alias map where the bindings for a set of local ids have
    * been updated to a new set of aliases.
    */
  def updated(ids: Set[LocalId], aliases: AliasSet): AliasMap =
    AliasMap(
      bindings.zipWithIndex
        .map((info, index) =>
          if ids.contains(index) then (info._1, aliases) else info
        )
    )

  /** Alias for `appended`. */
  def :+(mutable: Boolean, aliases: AliasSet): AliasMap =
    appended(mutable, aliases)

  /** Alias for `union`. */
  def |(that: AliasMap): AliasMap = union(that)

  /** Returns a new alias map where a new binding has been appended. */
  def appended(mutable: Boolean, aliases: AliasSet): AliasMap =
    AliasMap(bindings :+ (mutable, aliases))

  /** Returns the union of two alias maps. */
  def union(that: AliasMap): AliasMap =
    AliasMap(
      bindings.zip(that.bindings).map((lhs, rhs) => (lhs._1, lhs._2 | rhs._2))
    )

}
