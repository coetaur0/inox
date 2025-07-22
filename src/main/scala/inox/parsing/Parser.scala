package inox.parsing

import scala.util.control.Breaks

import inox.ast.{Name, Stmt, Type}
import inox.{Result, Span, Spanned}

/** A parser for Inox. */
object Parser {

  /** Parses a source and returns the corresponding AST or a sequence of errors
    * if parsing fails.
    */
  def parse(source: String): Result[Type, ParseError] =
    Parser(source).ty()
}

/** A parser for Inox. */
private class Parser(source: String) {
  private val lexer = Lexer(source)
  private var token = lexer.next()

  /** Parses a type expression. */
  private def ty(): Result[Type, ParseError] =
    token.item match {
      case Token.LParen    => parenType()
      case Token.FnKw      => fnType()
      case Token.Ampersand => refType()
      case Token.I32Kw     => Result.Success(Type.I32(advance().span))
      case Token.BoolKw    => Result.Success(Type.Bool(advance().span))
      case _               => Result.Failure(expected("a type expression"))
    }

  /** Parses a parenthesised type expression. */
  private def parenType(): Result[Type, ParseError] = {
    val start = advance().span.start
    if (token.item == Token.RParen)
      Result.Success(Type.Unit(Span(start, advance().span.end)))
    else
      for {
        ty <- ty()
        _ <- close("(", Token.RParen)
      } yield ty
  }

  /** Parses a function type expression. */
  private def fnType(): Result[Type, ParseError] = {
    val start = advance().span.start
    for {
      params <- delimited(
        () => list(ty, Token.Comma, Token.RParen),
        Token.LParen,
        Token.RParen
      )
      _ <- consume(Token.Arrow)
      result <- ty()
    } yield Type.Fn(params.item, result, Span(start, result.span.end))
  }

  /** Parses a reference type expression. */
  private def refType(): Result[Type, ParseError] = {
    val start = advance().span.start
    for {
      origin <- origin()
      mutable <- mut()
      ty <- ty()
    } yield Type.Ref(origin, mutable, ty, Span(start, ty.span.end))
  }

  /** Parses an optional origin annotation. */
  private def origin(): Result[Option[Name], ParseError] =
    optional(
      () => for name <- consume(Token.Origin) yield Some(name),
      Token.Origin,
      None
    )

  /** Parses an optional mutability marker. */
  private def mut(): Result[Boolean, ParseError] =
    optional(
      () => for _ <- consume(Token.MutKw) yield true,
      Token.MutKw,
      false
    )

  /** Parses an item delimited by some `start` and `end` tokens. */
  private def delimited[A](
      parse: () => Result[A, ParseError],
      start: Token,
      end: Token
  ): Result[Spanned[A], ParseError] = {
    for {
      open <- consume(start)
      item <- parse()
      close <- close(open.item, end)
    } yield Spanned(item, Span(open.span.start, close.end))
  }

  /** Parses a list of items separated by some `separator` token and ending with
    * some `end` token.
    */
  private def list[A](
      parse: () => Result[A, ParseError],
      separator: Token,
      end: Token
  ): Result[IndexedSeq[A], ParseError] = {
    val items = IndexedSeq.newBuilder[A]
    val errorBuilder = IndexedSeq.newBuilder[ParseError]

    Breaks.breakable {
      while (token.item != Token.Eof && token.item != end) {
        parse() match {
          case Result.Success(item)   => items += item
          case Result.Failure(errors) => errorBuilder ++= errors
        }

        if (token.item == separator)
          advance()
        else
          Breaks.break()
      }
    }

    val errors = errorBuilder.result()
    if (errors.nonEmpty)
      Result.Failure(errors)
    else
      Result.Success(items.result())
  }

  /** Parses an item starting with some `start` token and returns it or returns
    * some default value if the next token in the source doesn't match the
    * `start` kind.
    */
  private def optional[A](
      parse: () => Result[A, ParseError],
      start: Token,
      default: A
  ): Result[A, ParseError] =
    if (token.item == start)
      parse()
    else
      Result.Success(default)

  /** Advances the parser's position in the source to the next token and returns
    * the last token that was seen.
    */
  private def advance(): Spanned[Token] = {
    val last = token
    token = lexer.next()
    last
  }

  /** Consumes the next token in the source if it is of the expected kind or
    * returns an error.
    */
  private def consume(kind: Token): Result[Spanned[String], ParseError] =
    if (token.item == kind) {
      val next = text
      advance()
      Result.Success(next)
    } else
      Result.Failure(expected(kind.toString))

  /** Consumes the next token in the source if it is of the expected `end` kind
    * or returns an error.
    */
  private def close(start: String, end: Token): Result[Span, ParseError] =
    if (token.item == end)
      Result.Success(advance().span)
    else
      Result.Failure(
        IndexedSeq(
          ParseError.UnclosedDelimiter(start, end.toString, text)
        )
      )

  /** Returns a syntax error indicating that something else than the next token
    * in the source was expected.
    */
  private def expected(message: String): IndexedSeq[ParseError] =
    IndexedSeq(ParseError.UnexpectedSymbol(message, text))

  /** Resynchronises the parser at the next token that belongs to a list of
    * token kinds.
    */
  private def recover(kinds: Set[Token]): Unit =
    while (token.item != Token.Eof && !kinds.contains(token.item))
      advance()

  /** Returns the spanned string representation of the next token in the source.
    */
  private def text: Spanned[String] =
    Spanned(
      source.substring(token.span.start.offset, token.span.end.offset),
      token.span
    )
}
