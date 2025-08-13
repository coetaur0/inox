package inox.lowering

import scala.collection.mutable

/** A symbol table mapping names to values within a hierarchy of scopes. */
private class SymbolTable[A] {

  /** A binding scope mapping names to values. */
  private class Scope(shadowing: Boolean = true) {
    private val bindings = mutable.Map.empty[String, A]

    /** Returns the value associated with a name in the scope, or `None` if
      * there is no binding with that name.
      */
    def apply(name: String): Option[A] =
      bindings.get(name)

    /** Inserts a new binding in the scope and returns `true` if the insertion
      * succeeded. An insertion can fail if variable shadowing is disallowed and
      * a binding with the same name already exists.
      */
    def +=(binding: (name: String, value: A)): Boolean =
      if !shadowing && bindings.contains(binding.name) then false
      else {
        bindings += binding
        true
      }

    /** Clears the scope's contents. */
    def clear(): Unit = bindings.clear()

  }

  private val scopes = mutable.Stack(Scope(false))

  /** Pushes a new scope in the symbol table. */
  def push(shadowing: Boolean): Unit = scopes.push(Scope(shadowing))

  /** Pops the current (most nested) scope from the symbol table. */
  def pop(): Unit =
    if scopes.length > 1 then scopes.pop()
    else scopes.top.clear()

  /** Returns the value associated with a name in the symbol table, or `None` if
    * there is no binding with that name.
    */
  def apply(name: String): Option[A] =
    scopes.find(_.apply(name).isDefined).flatMap(_.apply(name))

  /** Inserts a new binding in the symbol table's current (most nested) scope
    * and returns `true` if the insertion succeeded. An insertion can fail if
    * variable shadowing is disallowed in the current scope and a binding with
    * the same name already exists.
    */
  def +=(binding: (name: String, value: A)): Boolean =
    scopes.top += binding

  /** Clears the symbol table's contents. */
  def clear(): Unit = {
    scopes.dropInPlace(scopes.length - 1)
    scopes.top.clear()
  }

}
