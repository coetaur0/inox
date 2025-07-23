package inox

/** A location in a source. */
case class Location(line: Int, column: Int, offset: Int):
  override def toString: String = s"$line:$column"

/** A span between two locations. */
case class Span(start: Location, end: Location):
  override def toString: String = s"$start..$end"

/** An item associated with a span in a source. */
case class Spanned[+A](item: A, span: Span)
