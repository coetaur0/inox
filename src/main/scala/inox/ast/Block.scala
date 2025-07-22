package inox.ast

/** A block expression. */
case class Block(stmts: IndexedSeq[Stmt], result: Expr)
