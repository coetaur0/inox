package inox.ast

/** A block expression. */
case class Block(stmts: Seq[Stmt], result: Expr)
