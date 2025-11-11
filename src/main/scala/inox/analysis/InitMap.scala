package inox.analysis

import inox.ir.{Local, LocalId}

/** The initialisation state of a variable. */
enum InitState {
  case Initialized
  case MaybeInitialized
  case Uninitialized

  override def toString: String = this match {
    case Initialized      => "initialized"
    case MaybeInitialized => "maybe initialized"
    case Uninitialized    => "uninitialized"
  }

  /** Alias for `union`. */
  def |(that: InitState): InitState = union(that)

  /** Returns the union of two initialisation states. */
  def union(that: InitState): InitState = (this, that) match {
    case (Initialized, Initialized)     => Initialized
    case (Uninitialized, Uninitialized) => Uninitialized
    case (_, _)                         => MaybeInitialized
  }
}

object InitMap {

  /** Initialises an initialisation map from a sequence of local declarations, given a specific
    * number of parameters and local variables.
    */
  def init(
      locals: IndexedSeq[Local],
      paramCount: Int,
      localCount: Int
  ): InitMap = InitMap(locals.zipWithIndex.map { (local, index) =>
    if (index == 0 || (index > paramCount && index < localCount)) {
      InitState.Uninitialized
    } else {
      InitState.Initialized
    }
  })
}

/** A mapping binding local ids to their initialisation state. */
class InitMap(val bindings: IndexedSeq[InitState]) {
  override def equals(that: Any): Boolean = that match {
    case that: InitMap => this.bindings == that.bindings
    case _             => false
  }

  override def toString: String = {
    val contents = bindings.zipWithIndex
      .map { (state, index) => s"($index) -> $state" }
      .mkString(", ")
    s"{$contents}"
  }

  /** Returns the initialisation state of some local id. */
  def apply(id: LocalId): InitState = bindings(id)

  /** Returns a new initialisation map where the binding for a specific local id has been updated to
    * a new state.
    */
  def updated(id: LocalId, state: InitState): InitMap = InitMap(bindings.updated(id, state))

  /** Returns a new initialisation map where the bindings for a set of local ids have been updated
    * to a new state.
    */
  def updated(ids: Set[LocalId], state: InitState): InitMap = InitMap(
    bindings.zipWithIndex.map((oldState, index) => if ids.contains(index) then state else oldState)
  )

  /** Alias for `union`. */
  def |(that: InitMap): InitMap = union(that)

  /** Returns the union of two initialisation maps. */
  def union(that: InitMap): InitMap = InitMap(
    bindings.zip(that.bindings).map((lhs, rhs) => lhs | rhs)
  )
}
